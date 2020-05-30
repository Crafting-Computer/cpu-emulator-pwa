port module Main exposing (main)


import Array exposing (Array)
import Array.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
import Time


port showProgramEditorPort : () -> Cmd msg
port hideProgramEditorPort : () -> Cmd msg
port editProgramInElmPort : (String -> msg) -> Sub msg
port editProgramInEditorPort : String -> Cmd msg
port showAssemblerErrorPort : ((Int, Int), String) -> Cmd msg
port clearAssemblerErrorPort : () -> Cmd msg
port scrollIntoViewPort : List String -> Cmd msg
port stepComputerPort : (Int, Int) -> Cmd msg
port stepComputeLite : (Int) -> Cmd msg
port askForComputerPort : Int -> Cmd msg
port receiveComputerPort : (Decode.Value -> msg) -> Sub msg
port editRomPort : Array Int -> Cmd msg
port editRamPort : (Int, Int) -> Cmd msg
port setRamPort : Array Int -> Cmd msg
port resetComputerPort : Int -> Cmd msg
port saveModelPort : Encode.Value -> Cmd msg


type alias Model =
  { computer : Computer
  , programs : Array AsmProgram
  , activeProgramIndex : Int
  , assemblerError : Maybe String
  , showProgramList : Bool
  , isEditingProgram : Bool
  , instructions : Array String
  , showLabels : Bool
  , isRunningComputer : Bool
  , ramSections : Int
  , ramRanges : Array Range
  , ramDisplayRanges : Array Range
  , ramScrolls : Array (InfiniteScroll.Model Msg)
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
  | EditProgramName String
  | ShowProgramList
  | HideProgramList
  | SetActiveProgramIndex Int
  | AddProgram
  | RemoveProgram Int
  | StartEditingProgram
  | EditProgram String
  | StopEditingProgram
  | StartEditingRam Int Int
  | EditRam Int String
  | StopEditingRam Int Int
  | EditRamRangeStart Int String
  | EditRamRangeEnd Int String
  | ShowMoreRam Int
  | RamScrollMsg Int InfiniteScroll.Msg
  | AddRamSection Int
  | RemoveRamSection Int
  | RomScrollMsg InfiniteScroll.Msg
  | LoadedMoreRom
  | SaveModel
  | NoOp


main : Program (Maybe String) Model Msg
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


type alias Layout =
  Array Range


type alias AsmProgram =
  { name : String
  , content : String
  , layout : Layout
  }


encodeModel : Model -> Encode.Value
encodeModel model =
  Encode.object
    [ ("programs", Encode.array encodeProgram model.programs)
    , ("activeProgramIndex", Encode.int model.activeProgramIndex)
    , ("computer", encodeComputer model.computer)
    ]


encodeProgram : AsmProgram -> Encode.Value
encodeProgram program =
  Encode.object
    [ ("name", Encode.string program.name)
    , ("content", Encode.string program.content)
    , ("layout", encodeLayout program.layout)
    ]


encodeLayout : Layout -> Encode.Value
encodeLayout layout =
  Encode.array encodeRange layout


encodeRange : Range -> Encode.Value
encodeRange (start, end) =
  Encode.list identity [ Encode.int start, Encode.int end ]


decodeProgram : Decoder AsmProgram
decodeProgram =
  Field.require "name" Decode.string <| \name ->
  Field.require "content" Decode.string <| \content ->
  Field.require "layout" decodeLayout <| \layout ->

  Decode.succeed
    { name =
      name
    , content =
      content
    , layout =
      layout
    }


decodeLayout : Decoder Layout
decodeLayout =
  Decode.array decodeRange


decodeRange : Decoder Range
decodeRange =
  Decode.map2 Tuple.pair 
    (Decode.index 0 Decode.int)
    (Decode.index 1 Decode.int)


decodeModel : Decoder Model
decodeModel =
  Field.require "programs" (Decode.array decodeProgram) <| \programs ->
  Field.require "activeProgramIndex" Decode.int <| \activeProgramIndex ->
  Field.require "computer" decodeComputer <| \computer ->

  Decode.succeed <|
    updateLayout
      { defaultModel
        | computer =
          computer
        , programs =
          programs
        , activeProgramIndex =
          activeProgramIndex
      }


encodeComputer : Computer -> Encode.Value
encodeComputer computer =
  Encode.object
    [ ("a", Encode.int computer.a)
    , ("d", Encode.int computer.d)
    , ("m", Encode.int computer.m)
    , ("pc", Encode.int computer.pc)
    , ("ram", Encode.array Encode.int computer.ram)
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
  { white =
    E.rgb255 255 255 255
  , black =
    E.rgb255 0 0 0
  , lightGreen =
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
ramSize = 2 ^ 16 + 19200


defaultModel : Model
defaultModel =
  let
    ramDisplaySize =
      case Array.get 0 defaultPrograms of
        Just program ->
          Array.length program.layout
        
        Nothing -> -- impossible
          0
  in
  updateLayout
  { computer =
    { a = 0
    , d = 0
    , m = 0
    , pc = 0
    , rom = Array.repeat romSize 0
    , ram = Array.repeat ramDisplaySize 0
    }
  , programs =
    defaultPrograms
  , activeProgramIndex =
    0
  , assemblerError =
    Nothing
  , showProgramList =
    False
  , isEditingProgram =
    False
  , instructions =
    Array.repeat romSize ""
  , showLabels =
    True
  , isRunningComputer =
    False
  , ramSections =
    0
  , ramRanges =
    Array.empty
  , ramDisplayRanges =
    Array.empty
  , ramScrolls =
    Array.initialize 2 <| (\index -> InfiniteScroll.init <| showMoreRamCmd index)
  , editingRamIndices =
    Array.empty
  , ramDisplaySize =
    0
  , romScroll =
    InfiniteScroll.init loadMoreRom
  , romDisplaySize =
    50
  , isAnimated =
    True
  }


defaultLayout : Layout
defaultLayout =
  Array.fromList
    [ (0, 255)
    ]


defaultPrograms : Array AsmProgram
defaultPrograms =
  Array.fromList [ { name = "FillScreenOnKeyPress"
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
  , layout =
    Array.fromList
    [ (0, 255)
    , (65536, 84736)
    ]
  }
  , { name =
    "addTwoNumbers"
    , content =
      """-- Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2
D=A
@3
D=D+A
@R0
M=D
"""
  , layout =
    defaultLayout
  }
  , { name =
    "drawRectangle"
    , content =
    """-- Draws a rectangle at the top-left corner of the screen.
-- The rectangle is (R0 * 4) pixels wide and R1 pixels high.

-- if width is 0 goto infinite loop
@R0
D=M
@INFINITE_LOOP
D;JLE
-- if height is 0 goto infinite loop
@R1
D=M
@INFINITE_LOOP
D;JLE
@height
M=D
@SCREEN
D=A
@address
M=D
(LOOP_HEIGHT)
    @R0
    D=M
    @width
    M=D
    
    (LOOP_WIDTH)
        @address
        A=M
        M=0
        @address
        M=M+1
        @width
        MD=M-1
        @LOOP_WIDTH
        D;JGT

    @80 -- screen width in # of registers
    D=A
    @R0 -- rectangle width
    D=D-M
    @address
    M=D+M
    @height
    MD=M-1
    @LOOP_HEIGHT
    D;JGT
(INFINITE_LOOP)
    @INFINITE_LOOP
    0;JMP
"""
  , layout =
    defaultLayout
  }
  ]


init : Maybe String -> (Model, Cmd Msg)
init savedModelString =
  let
    model =
      case savedModelString of
        Just str ->
          Result.withDefault defaultModel <| Decode.decodeString decodeModel str
        
        Nothing ->
          defaultModel
  in
  compileProgram
    (\_ m ->
      (m, setRamPort m.computer.ram)
    )
    model


updateLayout : Model -> Model
updateLayout  model =
  let
    ramRanges =
      .layout <| getActiveProgram model

    ramDisplayRanges =
      Array.map
        (\(start, end) ->
          (start, min (start + 256) end)
        )
        ramRanges
    
    ramDisplaySize =
      getMaxRangeIndex ramRanges
    
    ramSections =
      Array.length ramRanges

    editingRamIndices =
      Array.repeat ramSections Nothing
  in
  { model
    | ramRanges =
      ramRanges
    , ramDisplayRanges =
      ramDisplayRanges
    , ramDisplaySize =
      ramDisplaySize
    , ramSections =
      ramSections
    , editingRamIndices =
      editingRamIndices
  }


loadMoreRam : Int -> Model -> Model
loadMoreRam nextRamDisplaySize model =
  let
    oldComputer =
      model.computer
    
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
  { model
    | ramDisplaySize = nextRamDisplaySize
    , computer =
      { oldComputer
        | ram =
          nextRam
      }
  }


loadMoreRom : InfiniteScroll.Direction -> Cmd Msg
loadMoreRom _ =
  msgToCmd LoadedMoreRom


showMoreRamCmd : Int -> InfiniteScroll.Direction -> Cmd Msg
showMoreRamCmd ramIndex _ =
  msgToCmd <| ShowMoreRam ramIndex


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
      , E.spacing 30
      ]
      [ E.column
        [ E.spacing 20
        , E.alignTop
        , Background.color colors.white
        ]
        [ E.row
          [ E.spacing 30 ] <|
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
            , viewEditButton model.computer
            ]
          , E.row
            [ E.spacing 20 ]
            [ viewProgramName model
            , viewProgramList model
            ]
          ]
        ]
      , viewCloseEditorButton model.isEditingProgram
      ]


viewProgramList : Model -> E.Element Msg
viewProgramList model =
  let
    programList =
      E.column
        [ E.alignRight ] <|
        Array.Extra.indexedMapToList
          (\index program ->
            if index /= model.activeProgramIndex then
              Input.button
                ( styles.button
                ++ [ E.width <| E.px 300
                  , E.onRight <|
                    Input.button
                    [ E.htmlAttribute <| Html.Attributes.style "right" "25px"
                    , E.centerY
                    , E.htmlAttribute <| onClickNoProp <| RemoveProgram index
                    ]
                    { onPress =
                      Nothing
                    , label =
                      E.html
                        (FeatherIcons.x |>
                          FeatherIcons.toHtml []
                        )
                    }
                ]
                )
                { onPress =
                  Just <| SetActiveProgramIndex index
                , label =
                  E.paragraph []
                    [ E.text program.name
                    ]
                }
            else
              E.none
          )
          model.programs
        ++ [
          Input.button
          ( styles.button
          ++ [ E.width <| E.px 300 ]
          )
          { onPress =
            Just AddProgram
          , label =
            E.html
            ( FeatherIcons.plus |> FeatherIcons.toHtml [] )
          }
        ]
  in
  Input.button
    ( styles.button
    ++ [ E.above <|
      if model.showProgramList then
        programList
      else
        E.none
    ]
    )
    { onPress =
      Just <|
      if model.showProgramList then
        HideProgramList
      else
        ShowProgramList
    , label =
      E.html
      ( ( if model.showProgramList then
        FeatherIcons.chevronUp
      else
        FeatherIcons.chevronDown
      ) |> FeatherIcons.toHtml []
      )
    }


viewProgramName : Model -> E.Element Msg
viewProgramName model =
  Input.text []
    { onChange = EditProgramName
    , text = .name <| getActiveProgram model
    , placeholder = Nothing
    , label = Input.labelHidden "program name"
    }


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
    [ E.width <| (E.fill |> E.minimum 210)
    , Background.color colors.white
    ] <|
    [ E.text "ROM"
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll RomScrollMsg
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      0
      (\index instruction ->
        if isLabelInstruction instruction then
          index
        else
          index + 1
      )
      { data = instructionData
      , columns =
          [ { header = E.none
            , width = E.px 60
            , view =
              \index instruction ->
                E.el
                [ Font.letterSpacing -0.5 ] <|
                E.text <|
                  -- label is not actually stored in the ROM
                  if isLabelInstruction instruction then
                    ""
                  else
                    String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \index instruction ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , E.htmlAttribute <| Html.Attributes.style "border-width" "0.5px 1px"
                      , E.height <| E.px 22
                      , E.width E.fill
                      , E.htmlAttribute <| Html.Attributes.id <| getInstructionId index
                      ]

                    cellStyle =
                      if model.isAnimated
                      && index == model.computer.pc
                      && not (isLabelInstruction instruction)
                      then
                          commonStyle
                          ++ [ Background.color colors.lightGreen
                          ]
                      else
                        commonStyle
                  in
                  E.el cellStyle <|
                    E.text instruction
            }
          ]
      }
    ]


isLabelInstruction : String -> Bool
isLabelInstruction instruction =
  String.startsWith "(" instruction


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
    
    (startDisplayCellIndex, endDisplayCellIndex) =
      Maybe.withDefault (0, 0) <| Array.get ramIndex model.ramDisplayRanges

    editingRamIndex =
      Maybe.withDefault Nothing <| Array.get ramIndex model.editingRamIndices
    
    memoryData =
      Array.toList <| Array.slice startDisplayCellIndex (endDisplayCellIndex + 1) model.computer.ram
  in
  E.column
    [ E.width <| E.px 210
    , E.onRight <|
      E.column []
      [ viewAddRamSectionButton (ramIndex + 1)
      , viewRemoveRamSectionButton ramIndex
      ]
    ] <|
    [ E.row[]
      [ E.text <| "RAM "
      , Input.text
        [ E.padding 0 ]
        { text =
          String.fromInt startCellIndex
        , onChange =
          EditRamRangeStart ramIndex
        , placeholder =
          Nothing
        , label =
          Input.labelHidden "RAM range start"
        }
      , E.text <| "-"
      , Input.text
        [ E.padding 0 ]
        { text =
          String.fromInt endCellIndex
        , onChange =
          EditRamRangeEnd ramIndex
        , placeholder =
          Nothing
        , label =
          Input.labelHidden "RAM range end"
        }
      ]
    , indexedTable
      [ E.htmlAttribute <| InfiniteScroll.infiniteScroll <| RamScrollMsg ramIndex
      , E.htmlAttribute <| Html.Attributes.style "height" "640px"
      , E.htmlAttribute <| Html.Attributes.style "overflow-y" "auto"
      ]
      startCellIndex
      (\index _ ->
        index + 1
      )
      { data = memoryData
      , columns =
          [ { header = E.none
            , width = E.px 60
            , view =
              \index _ ->
                E.el
                [ Font.letterSpacing -0.5 ] <|
                E.text <| String.fromInt index
          }
          , { header = E.none
            , width = E.fill
            , view =
                \cellIndex cell ->
                  let
                    commonStyle =
                      [ E.paddingXY 10 0
                      , E.htmlAttribute <| Html.Attributes.style "border-width" "0.5px 1px"
                      , E.height <| E.px 22
                      , E.width <| E.px 130
                      ]
                    
                    cellStyle =
                      if cellIndex == model.computer.a then
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
                    , E.htmlAttribute <| Html.Attributes.id <| getRamCellId ramIndex cellIndex
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


viewAddRamSectionButton : Int -> E.Element Msg
viewAddRamSectionButton ramIndex =
  Input.button
  [ Background.color colors.white
  , Font.color colors.darkGrey
  , E.mouseOver
    [ Font.color colors.black ]
  ]
  { onPress =
    Just <| AddRamSection ramIndex
  , label =
    E.html
    ( FeatherIcons.plusSquare |>
      FeatherIcons.toHtml []
    )
  }


viewRemoveRamSectionButton : Int -> E.Element Msg
viewRemoveRamSectionButton ramIndex =
  Input.button
  [ Background.color colors.white
  , Font.color colors.darkGrey
  , E.mouseOver
    [ Font.color colors.black ]
  ]
  { onPress =
    Just <| RemoveRamSection ramIndex
  , label =
    E.html
    ( FeatherIcons.minusSquare |>
      FeatherIcons.toHtml []
    )
  }


indexedTable :
  List (E.Attribute Msg) ->
  Int ->
  (Int -> record -> Int) ->
  { data : List record
  , columns : List (E.IndexedColumn record Msg)
  } -> E.Element Msg
indexedTable attributes startIndex getNextIndex { data, columns } =
  E.column
  ( attributes
    ++ [ E.width E.fill ]
  ) <|
  List.reverse <|
  Tuple.second <|
  List.foldl
    (\cell (index, table) ->
      ( getNextIndex index cell
      , ( E.row [ E.width E.fill ] <|
        List.map
        (\column ->
          E.el
          [ E.width <| column.width
          ] <|
          column.view (index + startIndex) cell
        )
        columns
        ) :: table
      )
    )
    (0, [])
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

    ShowProgramList ->
      showProgramList model
    
    HideProgramList ->
      hideProgramList model

    SetActiveProgramIndex newIndex ->
      setActiveProgramIndex newIndex model

    EditProgramName newName ->
      editProgramName newName model
    
    AddProgram ->
      addProgram model

    RemoveProgram index ->
      removeProgram index model

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

    EditRamRangeStart ramIndex cellIndexStr ->
      editRamRangeStart ramIndex cellIndexStr model
    
    EditRamRangeEnd ramIndex cellIndexStr ->
      editRamRangeEnd ramIndex cellIndexStr model

    RamScrollMsg ramIndex scrollMsg ->
      let
        ( nextRamScroll, cmd ) =
          case Array.get ramIndex model.ramScrolls of
            Just ramScroll ->
              InfiniteScroll.update (RamScrollMsg ramIndex) scrollMsg ramScroll
            
            Nothing ->
              (InfiniteScroll.init <| showMoreRamCmd 0, Cmd.none) -- impossible
      in
      ( { model | ramScrolls = Array.set ramIndex nextRamScroll model.ramScrolls }, cmd )

    ShowMoreRam ramIndex ->
      showMoreRam ramIndex model

    AddRamSection ramIndex ->
      addRamSection ramIndex model
    
    RemoveRamSection ramIndex ->
      removeRamSection ramIndex model

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

    SaveModel ->
      ( model
      , saveModelPort (encodeModel model)
      )

    NoOp ->
      (model, Cmd.none)


addRamSection : Int -> Model -> (Model, Cmd Msg)
addRamSection ramIndex model =
  let
    newLayout =
      insertToArray ramIndex (0, 255) model.ramRanges
  in
  ( updateLayout <|
    updateActiveProgram
    (\oldProgram -> { oldProgram | layout = newLayout })
    model
  , Cmd.none
  )


removeRamSection : Int -> Model -> (Model, Cmd Msg)
removeRamSection ramIndex model =
  let
    newLayout =
      Array.Extra.removeAt ramIndex model.ramRanges
  in
  ( updateLayout <|
    updateActiveProgram
    (\oldProgram -> { oldProgram | layout = newLayout })
    model
  , Cmd.none
  )


showMoreRam : Int -> Model -> (Model, Cmd Msg)
showMoreRam ramIndex model =
  let
    nextRamScrolls =
      Array.Extra.update ramIndex InfiniteScroll.stopLoading model.ramScrolls
    
    (_, nextRamDisplayRanges) =
      unzipArray <|
      Array.Extra.update
      ramIndex
      (\((start, end), (displayStart, displayEnd)) ->
        let
          delta =
            (end - start) // 10
          
          nextDisplayEnd =
            if displayEnd + delta > end then
              end
            else
              displayEnd + delta
        in
        ((start, end), (displayStart, nextDisplayEnd))
      ) <| Array.Extra.zip model.ramRanges model.ramDisplayRanges
  in
  ( { model | ramScrolls = nextRamScrolls, ramDisplayRanges = nextRamDisplayRanges }, Cmd.none )


editRamRangeStart : Int -> String -> Model -> (Model, Cmd Msg)
editRamRangeStart ramIndex cellIndexStr model =
  editRamRange Tuple.mapFirst ramIndex cellIndexStr model


editRamRangeEnd : Int -> String -> Model -> (Model, Cmd Msg)
editRamRangeEnd ramIndex cellIndexStr model =
  editRamRange Tuple.mapSecond ramIndex cellIndexStr model


editRamRange : ((Int -> Int) -> Range -> Range)
 -> Int -> String -> Model -> (Model, Cmd Msg)
editRamRange f ramIndex cellIndexStr model =
  case String.toInt cellIndexStr of
    Just newCellIndex ->
      if newCellIndex > ramSize then
        (model, Cmd.none)  
      else
        let
          (newRamRanges, newRamDisplayRanges) =
            unzipArray <|
            Array.Extra.update
            ramIndex
            (\((start, end), _) ->
              let
                (newStart, newEnd) =
                  f (\_ -> newCellIndex) (start, end)
                
                newDisplayStart =
                  newStart
                
                newDisplayEnd =
                  min (newStart + 256) newEnd
              in
              ((newStart, newEnd), (newDisplayStart, newDisplayEnd))
            ) <|
            Array.Extra.zip model.ramRanges model.ramDisplayRanges
          
          newModel =
            { model
              | ramRanges =
                newRamRanges
              , ramDisplayRanges =
                newRamDisplayRanges
            }

          maxCellIndex =
            getMaxRangeIndex newModel.ramRanges
        in
        ( updateActiveProgram
          (\oldProgram -> { oldProgram | layout = newRamRanges }) <|
          if maxCellIndex >= newModel.ramDisplaySize then
            loadMoreRam (maxCellIndex + 1) newModel
          else
            newModel
        , Cmd.none
        )
    
    Nothing ->
      ( model
      , Cmd.none
      )


getMaxRangeIndex : Layout -> Int
getMaxRangeIndex layout =
  Array.foldl
    (\range maxIndex ->
      let
        currentMaxIndex =
          max (Tuple.first range) (Tuple.second range)
      in
      if currentMaxIndex > maxIndex then
        currentMaxIndex
      else
        maxIndex
    )
    0
    layout


removeProgram : Int -> Model -> (Model, Cmd Msg)
removeProgram index model =
  ({ model
    | programs =
      Array.Extra.removeAt index model.programs
    , activeProgramIndex =
      if index > model.activeProgramIndex then
        model.activeProgramIndex
      else
        model.activeProgramIndex - 1
  }
  , Cmd.none
  )


addProgram : Model -> (Model, Cmd Msg)
addProgram model =
  let
    newProgram =
      { name =
        "Untitled"
      , content =
        "{- Replace with your assembly code -}\n@0\nD=A"
      , layout =
        defaultLayout
      }
  in
  compileProgram
    (\_ m ->
      ( m, Cmd.none )
    )
    { model
      | programs =
        Array.push newProgram model.programs
      , activeProgramIndex =
        Array.length model.programs
    }


compileProgram : (Bool -> Model -> (Model, Cmd Msg)) -> Model -> (Model, Cmd Msg)
compileProgram f model =
  case Assembler.parseProgram <| .content <| getActiveProgram model of
    Ok instructions ->
      let
        oldComputer =
          model.computer

        updatedPartOfInstructions =
          Array.fromList <|
            if model.showLabels then
              Assembler.parseProgramKeepLabels <| .content <| getActiveProgram model
            else
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
        
        (newModel, newCmd) =
          f
          True
          { model
          | instructions =
            nextInstructions
          , computer =
            { oldComputer
              | rom =
                nextRom
            }
          }
      in
      ( newModel
      , Cmd.batch
        [ editRomPort updatedPartOfRom
        , editProgramInEditorPort <| .content <| getActiveProgram model
        , newCmd
        ]
      )
    
    Err _ ->
      let
        ( newModel, newCmd ) =
          f False model
      in
      ( newModel
      , Cmd.batch
        [ editProgramInEditorPort <| .content <| getActiveProgram model
        , newCmd
        ]
      )


setActiveProgramIndex : Int -> Model -> (Model, Cmd Msg)
setActiveProgramIndex newIndex model =
  compileProgram
    (\_ m ->
      (m, setRamPort m.computer.ram)
    )
    <| updateLayout { model
      | activeProgramIndex =
        newIndex
    }


hideProgramList : Model -> (Model, Cmd Msg)
hideProgramList model =
  ({ model
    | showProgramList =
      False
  }
  , Cmd.none
  )


showProgramList : Model -> (Model, Cmd Msg)
showProgramList model =
  ({ model
    | showProgramList =
      True
  }
  , Cmd.none
  )


editProgramName : String -> Model -> (Model, Cmd Msg)
editProgramName newName model =
  ( updateActiveProgram
    (\oldProgram -> { oldProgram | name = newName })
    model
  , Cmd.none
  )


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
    [ scrollIntoViewPort [ getInstructionId 0 ]
    , resetComputerPort model.ramDisplaySize
    ]
  )


stepComputer : Model -> (Model, Cmd Msg)
stepComputer model =
  let
    instructionId =
      getInstructionId model.computer.pc
    
    ramCellIds =
      List.map
        (\ramIndex ->
          getRamCellId ramIndex model.computer.a
        )
        (List.range 0 <| model.ramSections - 1)
  in
  ( model
  , Cmd.batch
    [ step model.ramDisplaySize 1
    , if model.isAnimated then
      scrollIntoViewPort <| instructionId :: ramCellIds
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
      ceiling ((timeToRun / 1000) * 700 * 1024)
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


getInstructionId : Int -> String
getInstructionId instructionIndex =
  "instruction-" ++ String.fromInt instructionIndex


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
    newModel =
      updateActiveProgram
      (\oldProgram -> { oldProgram | content = newContent })
      model
  in
  case Assembler.parseProgram newContent of
    Ok _ ->
      ( newModel
      , clearAssemblerErrorPort ()
      )

    Err error ->
      ( { newModel
          | assemblerError =
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
  Maybe.withDefault { name = "IMPOSSIBLE", content = "IMPOSSIBLE", layout = defaultLayout } <| -- impossible
  Array.get model.activeProgramIndex model.programs


updateActiveProgram : (AsmProgram -> AsmProgram) -> Model -> Model
updateActiveProgram f model =
  { model
    | programs =
      Array.Extra.update
      model.activeProgramIndex
      f
      model.programs
  }


stopEditingProgram : Model -> (Model, Cmd Msg)
stopEditingProgram model =
  compileProgram
    (\_ m ->
      ({ m
        | isEditingProgram =
          False
      }
      , hideProgramEditorPort ()
      )
    )
    model


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ editProgramInElmPort EditProgram
    , if model.isRunningComputer then
      Browser.Events.onAnimationFrameDelta StepComputerOneFrame
    else
      Time.every 2000 (\_ -> SaveModel)
    , receiveComputerPort ReceivedComputer
    ]


storeToMemory : Int -> Int -> Memory -> Memory
storeToMemory address value memory =
  Array.set address value memory


onClickNoProp : Msg -> Html.Attribute Msg
onClickNoProp msg =
  Html.Events.custom "click"
    (Decode.succeed
    { message = msg
    , stopPropagation = True
    , preventDefault = False
    }
  )

unzipArray : Array (a, b) -> (Array a, Array b)
unzipArray arr =
  Array.foldl
    (\(a, b) (arrA, arrB) ->
      (Array.push a arrA, Array.push b arrB)
    )
    (Array.empty, Array.empty)
    arr


insertToArray : Int -> a -> Array a -> Array a
insertToArray index val values =
  let
    length =
      Array.length values
  in
  if index >= 0 && index <= length then
    let
      before =
        Array.slice 0 index values

      after =
        Array.slice index length values
    in
    Array.append (Array.push val before) after

  else
    values