port module Main exposing (main)


import Array exposing (Array)
import Binary
import Bitwise
import List.Extra
import Html exposing (Html)
import Html.Attributes
import Browser
import Browser.Dom
import Browser.Events
import Task
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Assembler
import InfiniteScroll
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field


port showProgramEditorPort : () -> Cmd msg
port hideProgramEditorPort : () -> Cmd msg
port editProgramPort : (String -> msg) -> Sub msg
port showAssemblerErrorPort : ((Int, Int), String) -> Cmd msg
port clearAssemblerErrorPort : () -> Cmd msg
port scrollIntoViewPort : String -> Cmd msg
port stepComputerPort : Int -> Cmd msg
port receiveComputerPort : (Decode.Value -> msg) -> Sub msg
port editRomPort : Array Int -> Cmd msg
port editRamPort : (Int, Int) -> Cmd msg
port resetComputerPort : () -> Cmd msg


type alias Model =
  { computer : Computer
  , editingInstructionIndex : Maybe Int
  , editingRamIndex : Maybe Int
  , assemblerError : Maybe String
  , isEditingProgram : Bool
  , program : String
  , instructions : Array String
  , isRunningComputer : Bool
  , ramScroll : InfiniteScroll.Model Msg
  , ramDisplaySize : Int
  , romScroll : InfiniteScroll.Model Msg
  , romDisplaySize : Int
  , isAnimated : Bool
  }


type Msg
  = StepComputerOneFrame Float
  | StepComputer
  | ReceivedComputer Decode.Value
  | StartRunningComputer
  | StopRunningComputer
  | ResetComputer
  | StartEditingProgram
  | EditProgram String
  | StopEditingProgram
  | StartEditingRam Int
  | EditRam Int String
  | StopEditingRam Int
  | RamScrollMsg InfiniteScroll.Msg
  | LoadedMoreRam
  | RomScrollMsg InfiniteScroll.Msg
  | LoadedMoreRom
  | NoOp


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Computer =
  { a : Int
  , d : Int
  , m : Int
  , pc : Int
  , ram : Array Int
  , rom : Array Int
  }


encodeComputer : Computer -> Encode.Value
encodeComputer computer =
  Encode.object
    [ ("a", Encode.int computer.a)
    , ("d", Encode.int computer.d)
    , ("m", Encode.int computer.m)
    , ("pc", Encode.int computer.pc)
    , ("ram", Encode.array Encode.int computer.ram)
    , ("rom", Encode.array Encode.int computer.rom)
    ]


decodeComputer : Decoder Computer
decodeComputer =
  Field.require "a" Decode.int <| \a ->
  Field.require "d" Decode.int <| \d ->
  Field.require "m" Decode.int <| \m ->
  Field.require "pc" Decode.int <| \pc ->
  Field.require "ram" (Decode.array Decode.int) <| \ram ->

  Decode.succeed
    { a = a
    , d = d
    , m = m
    , pc = pc
    , ram = ram
    , rom = Array.empty
    }

type alias Memory =
  Array Int


colors =
  { lightGreen =
    E.rgb255 102 255 102
  , lightGrey =
    E.rgb255 220 220 220
  , darkGrey =
    E.rgb255 200 200 200
  , lightOrange =
    E.rgb255 255 165 0
  }


styles =
  { button =
    [ Background.color colors.lightGrey
    , E.mouseOver
      [ Background.color colors.darkGrey ]
    , E.paddingXY 15 10
    ]
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { computer =
    { a = 0
    , d = 0
    , m = 0
    , pc = 0
    , rom = Array.repeat (2 ^ 16) 0
    , ram = Array.repeat (2 ^ 17) 0
    }
  , editingInstructionIndex =
    Nothing
  , editingRamIndex =
    Nothing
  , assemblerError =
    Nothing
  , isEditingProgram =
    False
  , program =
    ""
  , instructions =
    Array.repeat (2 ^ 16) ""
  , isRunningComputer =
    False
  , ramScroll =
    InfiniteScroll.init loadMoreRam
  , ramDisplaySize =
    50
  , romScroll =
    InfiniteScroll.init loadMoreRom
  , romDisplaySize =
    50
  , isAnimated =
    True
  }
  , Cmd.none
  )


loadMoreRam : InfiniteScroll.Direction -> Cmd Msg
loadMoreRam _ =
  msgToCmd LoadedMoreRam


loadMoreRom : InfiniteScroll.Direction -> Cmd Msg
loadMoreRom _ =
  msgToCmd LoadedMoreRom


msgToCmd : msg -> Cmd msg
msgToCmd x =
    Task.perform identity (Task.succeed x)


step : Int -> Cmd Msg
step cycles =
  stepComputerPort cycles

view : Model -> Html Msg
view model =
  E.layout
    [ E.width E.fill
    , E.height E.fill
    , E.padding 20
    , Font.family
      [ Font.typeface "Roboto Mono"
      , Font.monospace
      ]
    ] <|
    E.row
      [ E.width E.fill
      , E.spacing 20
      ]
      [ E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewRam model
        , E.column
          [ E.width E.fill
          , E.spacing 10
          ]
          [ viewRegister "A" model.computer.a model.isAnimated
          , viewRegister "D" model.computer.d model.isAnimated
          , viewRegister "M" model.computer.m model.isAnimated
          ]
        ]
      , E.column
        [ E.spacing 20
        , E.alignTop
        ]
        [ viewRom model
        , E.column
          [ E.width E.fill
          , E.spacing 10
          ]
          [ viewRegister "PC" model.computer.pc model.isAnimated
          , E.row
            [ E.spacing 20 ]
            [ viewSingleStepButton
            , viewRunButton model.isRunningComputer
            , viewResetButton
            ]
          , E.row
            [ E.spacing 20 ]
            [ viewEditButton model.computer
            ]
          ]
        ]
      , viewCloseEditorButton model.isEditingProgram
      ]


viewCloseEditorButton : Bool -> E.Element Msg
viewCloseEditorButton isEditingProgram =
  if isEditingProgram then
    Input.button
      ( styles.button
      ++ [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
      , E.htmlAttribute <| Html.Attributes.style "bottom" "10px"
      , E.htmlAttribute <| Html.Attributes.style "left" "0px"
      ]
      )
      { onPress =
        Just StopEditingProgram
      , label =
        E.text "Save and Close"
      }
  else
    E.none


viewRom : Model -> E.Element Msg
viewRom model =
  let
    instructionData =
      Array.toList <| Array.slice 0 model.romDisplaySize model.instructions
  in
  E.column
    [ E.width <| E.px 200
    , E.htmlAttribute <| Html.Attributes.style "height" "640px"
    ] <|
    [ E.text "ROM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RomScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      { data = instructionData
      , columns =
          [ { header = E.none
            , width = E.px 50
            , view =
              \index _ ->
                E.el
                [] <|
                E.text <| String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \index cell ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , Border.width 1
                      , E.height <| E.px 22
                      , E.width <| E.px 130
                      , E.htmlAttribute <| Html.Attributes.id <| "instruction" ++ String.fromInt index
                      ]

                    cellStyle =
                      ( if model.isAnimated && index == model.computer.pc then
                          commonStyle
                          ++ [ Background.color colors.lightGreen
                          ]
                        else
                          commonStyle
                      )
                  in
                  E.el cellStyle <|
                    E.text cell
            }
          ]
      }
    ]


viewRegister : String -> Int -> Bool -> E.Element Msg
viewRegister name value isAnimated =
  E.el
  [ Border.width 2
  , E.width E.fill
  , E.padding 5
  ] <|
  E.text <| name ++ " = "
  ++ ( if isAnimated then
    String.fromInt value
  else
    "-"
  )


viewRam : Model -> E.Element Msg
viewRam model =
  let
    memoryData =
      Array.toList <| Array.slice 0 model.ramDisplaySize model.computer.ram
  in
  E.column
    [ E.width <| E.px 200
    , E.htmlAttribute <| Html.Attributes.style "height" "640px"
    ] <|
    [ E.text "RAM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RamScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      { data = memoryData
      , columns =
          [ { header = E.none
            , width = E.px 50
            , view =
              \index _ ->
                E.el
                [] <|
                E.text <| String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \index cell ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , Border.width 1
                      , E.height <| E.px 22
                      , E.width <| E.px 130
                      ]
                    
                    cellStyle =
                      if index == 0 then
                        commonStyle
                        ++ [ Background.color colors.lightGreen
                        ]
                      else
                        commonStyle

                    isEditing =
                      case model.editingRamIndex of
                        Nothing ->
                          False
                        
                        Just editingIndex ->
                          index == editingIndex
                  in
                  E.el cellStyle <|
                  if isEditing then
                    Input.text
                      (cellStyle
                      ++ [ Events.onLoseFocus <| StopEditingRam index
                        , E.htmlAttribute <| Html.Attributes.id <| "ram" ++ String.fromInt index
                        , E.width E.fill
                      ])
                      { onChange =
                        EditRam index
                      , text =
                        String.fromInt cell
                      , placeholder =
                        Nothing
                      , label =
                        Input.labelHidden "edit ram"
                      }
                  else
                    E.el
                    [ Events.onClick <| StartEditingRam index
                    , E.width E.fill
                    ] <|
                    E.text <|
                    if model.isAnimated then
                      String.fromInt cell
                    else
                      "-"
            }
          ]
      }
    ]


indexedTable :
  List (E.Attribute Msg) ->
  { data : List record
  , columns : List (E.IndexedColumn record Msg)
  } -> E.Element Msg
indexedTable attributes { data, columns } =
  E.column
  ( attributes
    ++ [ E.width E.fill ]
  ) <|
  List.indexedMap
    (\index cell ->
      E.row [ E.width E.fill ] <|
      List.map
      (\column ->
        E.el
        [ E.width <| column.width
        ] <|
        column.view index cell
      )
      columns
    )
    data


viewSingleStepButton : E.Element Msg
viewSingleStepButton =
  Input.button styles.button
    { onPress =
      Just <| StepComputer
    , label =
      E.html
        (FeatherIcons.chevronRight
        |> FeatherIcons.toHtml []
        )
    }


viewRunButton : Bool -> E.Element Msg
viewRunButton isRunningComputer =
  Input.button styles.button
    { onPress =
      if isRunningComputer then
        Just StopRunningComputer
      else
        Just StartRunningComputer
    , label =
      E.html
        ( ( if isRunningComputer then
            FeatherIcons.pause
          else
            FeatherIcons.chevronsRight
        )
        |> FeatherIcons.toHtml []
        )
    }


viewResetButton : E.Element Msg
viewResetButton =
  Input.button styles.button
    { onPress =
      Just ResetComputer
    , label =
      E.html
        (FeatherIcons.chevronsLeft
        |> FeatherIcons.toHtml []
        )
    }


viewEditButton : Computer -> E.Element Msg
viewEditButton computer =
  Input.button styles.button
    { onPress =
      Just StartEditingProgram
    , label =
      E.html
        (FeatherIcons.edit
        |> FeatherIcons.toHtml []
        )
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StepComputer ->
      stepComputer model

    ReceivedComputer json ->
      receivedComputer json model
    
    StepComputerOneFrame time ->
      stepComputerOneFrame time model

    StartRunningComputer ->
      startRunningComputer model

    StopRunningComputer ->
      stopRunningComputer model

    ResetComputer ->
      resetComputer model

    StartEditingProgram ->
      startEditingProgram model

    EditProgram newProgram ->
      editProgram newProgram model

    StopEditingProgram ->
      stopEditingProgram model

    StartEditingRam index ->
      startEditingRam index model
    
    EditRam index newValue->
      editRam index newValue model
    
    StopEditingRam index ->
      stopEditingRam index model

    RamScrollMsg scrollMsg ->
      let
        ( nextRamScroll, cmd ) =
          InfiniteScroll.update RamScrollMsg scrollMsg model.ramScroll
      in
      ( { model | ramScroll = nextRamScroll }, cmd )

    LoadedMoreRam ->
      let
        nextRamScroll =
          InfiniteScroll.stopLoading model.ramScroll
      in
      ( { model | ramScroll = nextRamScroll, ramDisplaySize = model.ramDisplaySize + 200 }, Cmd.none )

    RomScrollMsg scrollMsg ->
      let
        ( nextRomScroll, cmd ) =
          InfiniteScroll.update RomScrollMsg scrollMsg model.romScroll
      in
      ( { model | romScroll = nextRomScroll }, cmd )

    LoadedMoreRom ->
      let
        nextRomScroll =
          InfiniteScroll.stopLoading model.romScroll
      in
      ( { model | romScroll = nextRomScroll, romDisplaySize = model.romDisplaySize + 200 }, Cmd.none )

    NoOp ->
      (model, Cmd.none)


receivedComputer : Decode.Value -> Model -> (Model, Cmd Msg)
receivedComputer json model =
  ({ model
      | computer =
        Result.withDefault model.computer <|
        Result.map
          (\computer ->
            { computer
              | rom =
                model.computer.rom
            }
          )<|
        Decode.decodeValue decodeComputer json
    }
    , Cmd.none
    )


resetComputer : Model -> (Model, Cmd Msg)
resetComputer model =
  let
    oldComputer =
      model.computer
  in
  ({ model
    | computer =
      { oldComputer
        | pc =
          0
      }
    , isRunningComputer =
      False
  }
  , Cmd.batch
    [ scrollIntoViewPort "instruction0"
    , resetComputerPort ()
    ]
  )


stepComputer : Model -> (Model, Cmd Msg)
stepComputer model =
  let
    instructionId =
      "instruction" ++ String.fromInt model.computer.pc
  in
  ( model
  , Cmd.batch
    [ step 1
    , if model.isAnimated then
      scrollIntoViewPort instructionId
    else
      Cmd.none
    ]
  )


stepComputerOneFrame : Float -> Model -> (Model, Cmd Msg)
stepComputerOneFrame time model =
  let
    timeToRun =
      min (max 150 time) 1000
    
    cycles =
      ceiling ((timeToRun / 1000) * 3000 * 1024)
  in
  ( model
  , step cycles
  )


startRunningComputer : Model -> (Model, Cmd Msg)
startRunningComputer model =
  ({ model
    | isRunningComputer =
      True
    , isAnimated =
      False
  }
  , Cmd.none
  )


stopRunningComputer : Model -> (Model, Cmd Msg)
stopRunningComputer model =
  ({ model
    | isRunningComputer =
      False
    , isAnimated =
      True
  }
  , Cmd.none
  )


startEditingRam : Int -> Model -> (Model, Cmd Msg)
startEditingRam index model =
  ({ model
    | editingRamIndex =
      Just index
  }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| "ram" ++ String.fromInt index
  )


editRam : Int -> String -> Model -> (Model, Cmd Msg)
editRam index newValueStr model =
  let
    oldComputer =
      model.computer
    
    newValue =
      Maybe.withDefault 0 <| String.toInt newValueStr
  in
  ({ model
    | computer =
      { oldComputer
        | ram =
          storeToMemory index newValue oldComputer.ram
      }
  }
  , editRamPort (index, newValue)
  )


stopEditingRam : Int -> Model -> (Model, Cmd Msg)
stopEditingRam index model =
  ({ model
    | editingRamIndex =
      Nothing
  }
  , Cmd.none
  )


editProgram : String -> Model -> (Model, Cmd Msg)
editProgram newProgram model =
  case Assembler.parseProgram newProgram of
    Ok _ ->
      ( { model
        | program =
          newProgram
        }
      , clearAssemblerErrorPort ()
      )

    Err error ->
      ( { model
        | program =
          newProgram
        , assemblerError =
          Just <| Tuple.second error
        }
      , showAssemblerErrorPort error
      )


startEditingProgram : Model -> (Model, Cmd Msg)
startEditingProgram model =
  ( { model
    | isEditingProgram =
      True
  }
  , showProgramEditorPort ()
  )


stopEditingProgram : Model -> (Model, Cmd Msg)
stopEditingProgram model =
  case Assembler.parseProgram model.program of
    Ok instructions ->
      let
        oldComputer =
          model.computer

        updatedPartOfInstructions =
          Array.fromList <|
            List.map Assembler.instructionToString instructions
        
        restOfOldInstructions =
          Array.slice (Array.length updatedPartOfInstructions) (Array.length model.instructions) model.instructions
        
        nextInstructions =
          Array.append updatedPartOfInstructions restOfOldInstructions

        updatedPartOfRom =
          Array.fromList <|
            Assembler.emitProgram instructions
        
        restOfOldRom =
          Array.slice (Array.length updatedPartOfRom) (Array.length oldComputer.rom) oldComputer.rom
        
        nextRom =
          Array.append updatedPartOfRom restOfOldRom
      in
      ({ model
        | instructions =
          nextInstructions
        , computer =
          { oldComputer
            | rom =
              nextRom
          }
        , isEditingProgram =
          False
      }
      , Cmd.batch
        [ hideProgramEditorPort ()
        , editRomPort updatedPartOfRom
        ]
      )
    
    Err _ ->
      ({ model
        | isEditingProgram =
          False
      }
      , hideProgramEditorPort ()
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ editProgramPort EditProgram
    , if model.isRunningComputer then
      Browser.Events.onAnimationFrameDelta StepComputerOneFrame
    else
      Sub.none
    , receiveComputerPort ReceivedComputer
    ]


storeToMemory : Int -> Int -> Memory -> Memory
storeToMemory address value memory =
  Array.set address value memory


toBinaryString : Int -> String
toBinaryString number =
  String.join "" <| List.map String.fromInt <| Binary.toIntegers <| Binary.fromDecimal number