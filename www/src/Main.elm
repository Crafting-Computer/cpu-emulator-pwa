port module Main exposing (main)


import Array exposing (Array)
import Array.Extra
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
port editProgramInElmPort : (String -> msg) -> Sub msg
port editProgramInEditorPort : String -> Cmd msg
port showAssemblerErrorPort : ((Int, Int), String) -> Cmd msg
port clearAssemblerErrorPort : () -> Cmd msg
port scrollIntoViewPort : String -> Cmd msg
port stepComputerPort : (Int, Int) -> Cmd msg
port stepComputeLite : (Int) -> Cmd msg
port askForComputerPort : Int -> Cmd msg
port receiveComputerPort : (Decode.Value -> msg) -> Sub msg
port editRomPort : Array Int -> Cmd msg
port editRamPort : (Int, Int) -> Cmd msg
port resetComputerPort : Int -> Cmd msg


type alias Model =
  { computer : Computer
  , programs : Array AsmProgram
  , activeProgramIndex : Int
  , assemblerError : Maybe String
  , isEditingProgram : Bool
  , instructions : Array String
  , isRunningComputer : Bool
  , ramSections : Int
  , ramRanges : Array Range
  , editingRamIndices : Array (Maybe Int)
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
  | StartEditingRam Int Int
  | EditRam Int String
  | StopEditingRam Int Int
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


type alias Range =
  (Int, Int)


type alias AsmProgram =
  { name : String
  , content : String
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


romSize : Int
romSize = 2 ^ 16

ramSize : Int
ramSize = 2 ^ 17

init : () -> (Model, Cmd Msg)
init _ =
  let
    ramDisplaySize =
      501
    
    programs =
      [ { name = "FillScreenOnKeyPress"
      , content =
        """-- Runs an infinite loop that listens to the keyboard input.
-- When a key is pressed (any key), the program blackens the screen,
-- i.e. writes "black" in every pixel;
-- the screen should remain fully black as long as the key is pressed. 
-- When no key is pressed, the program clears the screen, i.e. writes
-- "white" in every pixel;
-- the screen should remain fully clear as long as no key is pressed.

@19200
D=A
@size
M=D

(LOOP)
  @KBD
  A=M
  D=A
  @i
  M=0
  @FILL
  D;JNE
  (CLEAR)
    @i
    D=M
    @size
    D=D-M
    @LOOP
    D;JGE
    @i
    D=M
    @SCREEN
    A=D+A
    M=-1
    @i
    M=M+1
    @CLEAR
    0;JMP
  (FILL)
    @i
    D=M
    @size
    D=D-M
    @LOOP
    D;JGE
    @i
    D=M
    @SCREEN
    A=D+A
    M=0
    @i
    M=M+1
    @FILL
    0;JMP
  @LOOP
  0;JMP
"""
      }
      ]
    
    (model, editRomCmd) =
      stopEditingProgram
      { computer =
        { a = 0
        , d = 0
        , m = 0
        , pc = 0
        , rom = Array.repeat romSize 0
        , ram = Array.repeat ramDisplaySize 0
        }
      , programs =
        Array.fromList programs
      , activeProgramIndex =
        0
      , assemblerError =
        Nothing
      , isEditingProgram =
        False
      , instructions =
        Array.repeat romSize ""
      , isRunningComputer =
        False
      , ramSections =
        2
      , ramRanges =
        Array.fromList [ (0, 256)
        , (256, 501)
        ]
      , editingRamIndices =
        Array.fromList [ Nothing, Nothing ]
      , ramDisplaySize =
        ramDisplaySize
      , romScroll =
        InfiniteScroll.init loadMoreRom
      , romDisplaySize =
        50
      , isAnimated =
        True
      }
  in
  ( model
  , Cmd.batch
    [ editProgramInEditorPort <| .content <| getActiveProgram model
    , editRomCmd
    ]
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


step : Int -> Int -> Cmd Msg
step ramDisplaySize cycles =
  stepComputerPort (ramDisplaySize, cycles)


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
        [ E.row
          [ E.spacing 20 ] <|
          List.map
          (viewRam model)
          (List.range 0 (model.ramSections - 1))
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
    ] <|
    [ E.text "ROM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RomScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      0
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


viewRam : Model -> Int -> E.Element Msg
viewRam model ramIndex =
  let
    (startCellIndex, endCellIndex) =
      Maybe.withDefault (0, 0) <| Array.get ramIndex model.ramRanges

    editingRamIndex =
      Maybe.withDefault Nothing <| Array.get ramIndex model.editingRamIndices
    
    memoryData =
      Array.toList <| Array.slice startCellIndex endCellIndex model.computer.ram
  in
  E.column
    [ E.width <| E.px 200
    ] <|
    [ E.text <| "RAM (" ++ String.fromInt startCellIndex ++ "-" ++ String.fromInt (endCellIndex - 1) ++ ")"
    , indexedTable
      [ E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      startCellIndex
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
                \cellIndex cell ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , Border.width 1
                      , E.height <| E.px 22
                      , E.width <| E.px 130
                      ]
                    
                    cellStyle =
                      if cellIndex == 0 then
                        commonStyle
                        ++ [ Background.color colors.lightGreen
                        ]
                      else
                        commonStyle

                    isEditing =
                      case editingRamIndex of
                        Nothing ->
                          False
                        
                        Just editingIndex ->
                          cellIndex == editingIndex
                  in
                  E.el cellStyle <|
                  if isEditing then
                    Input.text
                      (cellStyle
                      ++ [ Events.onLoseFocus <| StopEditingRam ramIndex cellIndex
                        , E.htmlAttribute <| Html.Attributes.id <| getRamCellId ramIndex cellIndex
                        , E.width E.fill
                      ])
                      { onChange =
                        EditRam cellIndex
                      , text =
                        String.fromInt cell
                      , placeholder =
                        Nothing
                      , label =
                        Input.labelHidden "edit ram"
                      }
                  else
                    E.el
                    [ Events.onClick <| StartEditingRam ramIndex cellIndex
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
  Int ->
  { data : List record
  , columns : List (E.IndexedColumn record Msg)
  } -> E.Element Msg
indexedTable attributes startIndex { data, columns } =
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
        column.view (index +startIndex) cell
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

    StartEditingRam ramIndex cellIndex ->
      startEditingRam ramIndex cellIndex model
    
    EditRam index newValue->
      editRam index newValue model
    
    StopEditingRam ramIndex cellIndex ->
      stopEditingRam ramIndex cellIndex model

    LoadedMoreRam ->
      let
        oldComputer =
          model.computer
        
        nextRamDisplaySize =
          model.ramDisplaySize + 200

        ramSizeDifference =
          nextRamDisplaySize - Array.length model.computer.ram

        nextRam =
          if ramSizeDifference > 0 then
            Array.append
              model.computer.ram
              (Array.repeat ramSizeDifference 0)
          else
            model.computer.ram
      in
      ( { model
        | ramDisplaySize = nextRamDisplaySize
        , computer =
          { oldComputer
            | ram =
              nextRam
          }
      }
      , Cmd.none
      )

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
  ({ model
    | isRunningComputer =
      False
  }
  , Cmd.batch
    [ scrollIntoViewPort "instruction0"
    , resetComputerPort model.ramDisplaySize
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
    [ step model.ramDisplaySize 1
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
      min time 1000
    
    cycles =
      ceiling ((timeToRun / 1000) * 1000 * 1024)
  in
  ( model
  , stepComputeLite cycles
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
  , askForComputerPort model.ramDisplaySize
  )


startEditingRam : Int -> Int -> Model -> (Model, Cmd Msg)
startEditingRam ramIndex cellIndex model =
  ({ model
    | editingRamIndices =
      Array.set ramIndex (Just cellIndex) model.editingRamIndices
  }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| getRamCellId ramIndex cellIndex
  )


getRamCellId : Int -> Int -> String
getRamCellId ramIndex cellIndex =
  "ram-" ++ String.fromInt ramIndex ++ "-" ++ String.fromInt cellIndex


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


stopEditingRam : Int -> Int -> Model -> (Model, Cmd Msg)
stopEditingRam ramIndex cellIndex model =
  ({ model
    | editingRamIndices =
      Array.set ramIndex Nothing model.editingRamIndices
  }
  , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus <| getRamCellId ramIndex cellIndex
  )


editProgram : String -> Model -> (Model, Cmd Msg)
editProgram newContent model =
  let
    newPrograms =
      Array.Extra.update
      model.activeProgramIndex
      (\oldProgram -> { oldProgram | content = newContent })
      model.programs
  in
  case Assembler.parseProgram newContent of
    Ok _ ->
      ( { model
        | programs =
          newPrograms
        }
      , clearAssemblerErrorPort ()
      )

    Err error ->
      ( { model
        | programs =
          newPrograms
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


getActiveProgram : Model -> AsmProgram
getActiveProgram model =
  Maybe.withDefault { name = "IMPOSSIBLE", content = "IMPOSSIBLE"} <| -- impossible
  Array.get model.activeProgramIndex model.programs


stopEditingProgram : Model -> (Model, Cmd Msg)
stopEditingProgram model =
  case Assembler.parseProgram <| .content <| getActiveProgram model of
    Ok instructions ->
      let
        oldComputer =
          model.computer

        updatedPartOfInstructions =
          Array.fromList <|
            List.map Assembler.instructionToString instructions
        
        nextInstructions =
          Array.append
            updatedPartOfInstructions
            (Array.repeat (romSize - Array.length updatedPartOfInstructions) "")

        updatedPartOfRom =
          Array.fromList <|
            Assembler.emitProgram instructions
        
        nextRom =
          Array.append
            updatedPartOfRom
            (Array.repeat (romSize - Array.length updatedPartOfRom) 0)
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
    [ editProgramInElmPort EditProgram
    , if model.isRunningComputer then
      Browser.Events.onAnimationFrameDelta StepComputerOneFrame
    else
      Sub.none
    , receiveComputerPort ReceivedComputer
    ]


storeToMemory : Int -> Int -> Memory -> Memory
storeToMemory address value memory =
  Array.set address value memory

