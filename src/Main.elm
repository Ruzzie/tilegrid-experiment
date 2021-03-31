module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, h4, hr, span)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse exposing (Event)
import Html.Keyed as Keyed
import Html.Lazy
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events
import Svg.Lazy


type alias GridPoint =
    { x : Int, y : Int }


type alias GridSize =
    { width : Int, height : Int }


getGridPointForIndex idx widthInCells =
    let
        x =
            modBy widthInCells idx

        y =
            idx // widthInCells
    in
    { x = x, y = y }


emptyDefaultTileCard : TileCard
emptyDefaultTileCard =
    { id = 0
    , sizeInCells = { width = 3, height = 3 }
    , cells =
        Array.fromList
            [ Open
            , Open
            , Open
            , Open
            , Open
            , Open
            , Open
            , Open
            , Open
            ]
    }


mainGridDefaultSizeInTileCards =
    { width = 8, height = 8 }


emptyDefaultMainGrid : MainGrid
emptyDefaultMainGrid =
    { mouseAtPoint = Nothing
    , cellSizeInPixels = { width = 64, height = 64 }
    , tileCardSizeInCells = emptyDefaultTileCard.sizeInCells
    , tileIdToShowAtCursor = Nothing
    , currentTileCardRotation = 0
    , sizeInTileCards = mainGridDefaultSizeInTileCards
    , tileCards = Array.fromList (List.repeat (mainGridDefaultSizeInTileCards.width * mainGridDefaultSizeInTileCards.height) Nothing)
    , tool = PlaceSelectionTool
    }


initialModel : MainModel
initialModel =
    { cardDesigns = Dict.fromList [ ( emptyDefaultTileCard.id, emptyDefaultTileCard ) ]
    , uidCounter = 1
    , mouseOverCellInTileCard = Nothing
    , cellSizeInPixels = emptyDefaultMainGrid.cellSizeInPixels
    , selectedTileCardId = Nothing
    , mainGrid = emptyDefaultMainGrid
    }


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( MainModel, Cmd msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : MainModel -> Sub UpdateMsg
subscriptions model =
    Browser.Events.onKeyPress keyDecoder


keyDecoder =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


type UpdateMsg
    = MouseOverHighlightCellInTileCardUpdate TileCardSelectedCell
    | ClearMouseOverCellInTileCard
    | ToggleCellTypeInTileCard TileCardSelectedCell
    | CopyTileCardDesign TileCard
    | DeleteTileCardDesign TileCard
    | SelectTileCardDesign TileCard
    | DeselectTileCardDesign
    | MouseOverMainGrid Event
    | MouseLeaveMainGrid
    | MouseClickMainGrid MainGridAction
    | KeyPressed String
    | ChangeTool MainGridTool


type CellType
    = Open
    | Closed


type alias TileCard =
    { id : Int
    , sizeInCells : GridSize
    , cells : Array CellType
    }


type alias TileCardPlacement =
    { tileCardId : Int
    , tileCardStartPx : GridPoint
    , rotation : Int
    }


type alias TileCardSelectedCell =
    { tileCardId : Int
    , cellCoord : GridPoint
    }


type alias Point =
    { x : Float
    , y : Float
    }


type MainGridAction
    = Primary Point
    | Secondary


type MainGridTool
    = PlaceSelectionTool
    | Random
    | DeleteTool
    | Select


type alias MainGrid =
    { mouseAtPoint : Maybe ( Float, Float )
    , cellSizeInPixels : GridSize
    , tileCardSizeInCells : GridSize
    , tileIdToShowAtCursor : Maybe Int
    , currentTileCardRotation : Int
    , sizeInTileCards : GridSize
    , tileCards : Array (Maybe TileCardPlacement)
    , tool : MainGridTool
    }


type alias MainModel =
    { cardDesigns : Dict Int TileCard
    , uidCounter : Int
    , mouseOverCellInTileCard : Maybe TileCardSelectedCell
    , cellSizeInPixels : GridSize
    , selectedTileCardId : Maybe Int
    , mainGrid : MainGrid
    }


pointToTileCardCoordinates : ( Float, Float ) -> GridSize -> GridSize -> { absCellCoord : GridPoint, absTileCardCoord : GridPoint, absTileCardStartInPx : GridPoint, absTileCardCentreInPx : { x : Float, y : Float } }
pointToTileCardCoordinates point cellSizeInPx tileCardSizeInCells =
    let
        ( mX, mY ) =
            point

        cellCoord =
            { x = round mX // cellSizeInPx.width, y = round mY // cellSizeInPx.height }

        absTileCardCoord =
            { x = cellCoord.x // tileCardSizeInCells.width, y = cellCoord.y // tileCardSizeInCells.height }

        tileCardAbsStartX =
            absTileCardCoord.x * cellSizeInPx.width * tileCardSizeInCells.width

        tileCardAbsStartY =
            absTileCardCoord.y * cellSizeInPx.height * tileCardSizeInCells.height

        tileCardAbsoluteCentreX =
            toFloat tileCardAbsStartX + (toFloat cellSizeInPx.width * (toFloat tileCardSizeInCells.width * 0.5))

        tileCardAbsoluteCentreY =
            toFloat tileCardAbsStartY + (toFloat cellSizeInPx.height * (toFloat tileCardSizeInCells.height * 0.5))
    in
    { absCellCoord = cellCoord
    , absTileCardCoord = absTileCardCoord
    , absTileCardStartInPx = { x = tileCardAbsStartX, y = tileCardAbsStartY }
    , absTileCardCentreInPx = { x = tileCardAbsoluteCentreX, y = tileCardAbsoluteCentreY }
    }


update : UpdateMsg -> MainModel -> ( MainModel, Cmd UpdateMsg )
update updateMsg model =
    case updateMsg of
        ChangeTool toTool ->
            let
                oldMainGrid =
                    model.mainGrid

                updatedMainGrid =
                    { oldMainGrid | tool = toTool }
            in
            ( { model | mainGrid = updatedMainGrid }, Cmd.none )

        MouseOverHighlightCellInTileCardUpdate tileCardSelectedCell ->
            ( { model | mouseOverCellInTileCard = Just tileCardSelectedCell }, Cmd.none )

        ClearMouseOverCellInTileCard ->
            ( { model | mouseOverCellInTileCard = Nothing }, Cmd.none )

        ToggleCellTypeInTileCard tileCardSelectedCell ->
            {- naive lookup and map -}
            let
                updatedDesigns =
                    case Dict.get tileCardSelectedCell.tileCardId model.cardDesigns of
                        Just tileCard ->
                            Dict.update tileCard.id (Maybe.map (updateTileCellType tileCardSelectedCell)) model.cardDesigns

                        Nothing ->
                            model.cardDesigns
            in
            ( { model | cardDesigns = updatedDesigns }, Cmd.none )

        CopyTileCardDesign sourceCard ->
            let
                newId =
                    model.uidCounter
            in
            ( { model | uidCounter = newId + 1, cardDesigns = Dict.insert newId { sourceCard | id = newId } model.cardDesigns }, Cmd.none )

        DeleteTileCardDesign cardToDelete ->
            ( { model | cardDesigns = Dict.remove cardToDelete.id model.cardDesigns }, Cmd.none )

        SelectTileCardDesign tileCard ->
            ( { model | selectedTileCardId = Just tileCard.id }, Cmd.none )

        DeselectTileCardDesign ->
            ( { model | selectedTileCardId = Nothing }, Cmd.none )

        MouseOverMainGrid mouseEvent ->
            let
                oldMainGrid =
                    model.mainGrid

                updateMainGrid e =
                    { oldMainGrid | mouseAtPoint = Just e, tileIdToShowAtCursor = model.selectedTileCardId }
            in
            ( { model | mainGrid = updateMainGrid mouseEvent.offsetPos }, Cmd.none )

        MouseLeaveMainGrid ->
            {- let
                   oldMainGrid =
                       model.mainGrid

                   updateMainGrid =
                       { oldMainGrid | mouseAtPoint = Nothing }
               in
               ( { model | mainGrid = updateMainGrid }, Cmd.none )
            -}
            ( model, Cmd.none )

        MouseClickMainGrid action ->
            case action of
                Primary actionCoordInPixels ->
                    case model.mainGrid.tool of
                        PlaceSelectionTool ->
                            let
                                tileCardDesign =
                                    model.selectedTileCardId |> Maybe.andThen (\id -> Dict.get id model.cardDesigns)
                            in
                            case tileCardDesign of
                                Just tileCard ->
                                    let
                                        coords =
                                            pointToTileCardCoordinates ( actionCoordInPixels.x, actionCoordInPixels.y ) model.mainGrid.cellSizeInPixels tileCard.sizeInCells

                                        idxInArray =
                                            indexForCoord model.mainGrid.sizeInTileCards.width coords.absTileCardCoord

                                        updatedMainGrid =
                                            updateMainGridTileCardsArray idxInArray
                                                (Just
                                                    { tileCardId = tileCard.id
                                                    , tileCardStartPx = coords.absTileCardStartInPx
                                                    , rotation = model.mainGrid.currentTileCardRotation
                                                    }
                                                )
                                                model.mainGrid
                                    in
                                    ( { model | mainGrid = updatedMainGrid }, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        DeleteTool ->
                            let
                                coords =
                                    pointToTileCardCoordinates ( actionCoordInPixels.x, actionCoordInPixels.y ) model.mainGrid.cellSizeInPixels model.mainGrid.tileCardSizeInCells

                                idxInArray =
                                    indexForCoord model.mainGrid.sizeInTileCards.width coords.absTileCardCoord

                                updatedMainGrid =
                                    updateMainGridTileCardsArray idxInArray
                                        Nothing
                                        model.mainGrid
                            in
                            ( { model | mainGrid = updatedMainGrid }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyPressed key ->
            case key of
                "]" ->
                    -- add rotate 90 deg
                    case model.mainGrid.tileIdToShowAtCursor of
                        Just tileCardId ->
                            let
                                oldGrid =
                                    model.mainGrid

                                rotation =
                                    modBy 360 (model.mainGrid.currentTileCardRotation + 90)
                            in
                            ( { model | mainGrid = { oldGrid | currentTileCardRotation = rotation } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                "p" ->
                    update (ChangeTool PlaceSelectionTool) model

                "d" ->
                    update (ChangeTool DeleteTool) model

                _ ->
                    ( model, Cmd.none )


updateMainGridTileCardsArray idx placementMaybe mainGrid =
    let
        updatedGridArray =
            Array.set idx placementMaybe mainGrid.tileCards

        updatedMainGrid =
            { mainGrid | tileCards = updatedGridArray }
    in
    updatedMainGrid


updateTileCellType : TileCardSelectedCell -> TileCard -> TileCard
updateTileCellType tileCellToToggle tile =
    if tile.id == tileCellToToggle.tileCardId then
        let
            index =
                indexForCoord tile.sizeInCells.width tileCellToToggle.cellCoord

            oldCell =
                Array.get index tile.cells
        in
        case oldCell of
            Just cell ->
                let
                    newCell =
                        case cell of
                            Open ->
                                Closed

                            Closed ->
                                Open
                in
                { tile | cells = Array.set index newCell tile.cells }

            Nothing ->
                tile

    else
        tile


indexForCoord : Int -> GridPoint -> Int
indexForCoord widthInCells cellCoord =
    (widthInCells * cellCoord.y) + cellCoord.x


toolToString : MainGridTool -> String
toolToString tool =
    case tool of
        PlaceSelectionTool ->
            "Place"

        DeleteTool ->
            "Delete"

        _ ->
            "?"


view : MainModel -> Html UpdateMsg
view mainModel =
    let
        cellWPxStr =
            String.fromInt mainModel.cellSizeInPixels.width

        cellHPxStr =
            String.fromInt mainModel.cellSizeInPixels.height

        defaultTileCardGridSize =
            3
    in
    div []
        [ viewTileCardsOverView mainModel.cardDesigns mainModel.selectedTileCardId mainModel.cellSizeInPixels mainModel.mouseOverCellInTileCard
        , Html.Lazy.lazy viewMainGridDebugInfo mainModel.mainGrid
        , span []
            [ span [] [ Html.text <| "|Current: " ++ toolToString mainModel.mainGrid.tool ]
            , span [] [ Html.text " |Tools: " ]
            , if mainModel.mainGrid.tool == PlaceSelectionTool then
                Html.button [ Html.Attributes.disabled True, Html.Attributes.style "border" "solid 2px magenta" ] [ Html.text "Place" ]

              else
                Html.button [ Html.Events.onClick (ChangeTool PlaceSelectionTool) ] [ Html.text "Place" ]
            , if mainModel.mainGrid.tool == DeleteTool then
                Html.button [ Html.Attributes.disabled True, Html.Attributes.style "border" "solid 2px red" ] [ Html.text "Delete" ]

              else
                Html.button [ Html.Events.onClick (ChangeTool DeleteTool) ] [ Html.text "Delete" ]
            , span [] [ Html.text " Rotate: ']', Place tool: 'p' , Delete tool: 'd'" ]
            ]
        , div []
            [ Svg.svg
                {- https://stackoverflow.com/questions/14208673/how-to-draw-grid-using-html5-and-canvas-or-svg -}
                ([ SvgAttr.width (String.fromInt ((mainModel.mainGrid.cellSizeInPixels.width * mainModel.mainGrid.sizeInTileCards.width * mainModel.mainGrid.tileCardSizeInCells.width) + 1))
                 , SvgAttr.height (String.fromInt ((mainModel.mainGrid.cellSizeInPixels.height * mainModel.mainGrid.sizeInTileCards.height * mainModel.mainGrid.tileCardSizeInCells.height) + 1))
                 , Mouse.onMove MouseOverMainGrid
                 , Svg.Events.onMouseOut MouseLeaveMainGrid
                 ]
                    ++ Maybe.withDefault []
                        (Maybe.map (\( mX, mY ) -> [ Svg.Events.onClick (MouseClickMainGrid (Primary { x = mX, y = mY })) ])
                            mainModel.mainGrid.mouseAtPoint
                        )
                )
                [ Svg.defs []
                    [ Svg.pattern
                        [ SvgAttr.id "cellGrid"
                        , SvgAttr.width cellWPxStr
                        , SvgAttr.height cellHPxStr
                        , SvgAttr.patternUnits "userSpaceOnUse"
                        ]
                        [ Svg.rect
                            [ SvgAttr.width cellWPxStr
                            , SvgAttr.height cellHPxStr
                            , SvgAttr.stroke "gray"
                            , SvgAttr.strokeWidth "0.5"
                            , SvgAttr.fill "none"
                            , SvgAttr.opacity "0.8"
                            ]
                            []
                        ]
                    , Svg.pattern
                        [ SvgAttr.id "tileCardGrid"
                        , SvgAttr.width (String.fromInt (mainModel.cellSizeInPixels.width * defaultTileCardGridSize))
                        , SvgAttr.height (String.fromInt (mainModel.cellSizeInPixels.height * defaultTileCardGridSize))
                        , SvgAttr.patternUnits "userSpaceOnUse"
                        ]
                        [ Svg.rect
                            [ SvgAttr.width (String.fromInt (mainModel.cellSizeInPixels.width * defaultTileCardGridSize))
                            , SvgAttr.height (String.fromInt (mainModel.cellSizeInPixels.height * defaultTileCardGridSize))
                            , SvgAttr.fill "url(#cellGrid)"
                            , SvgAttr.opacity "0.8"
                            ]
                            []
                        , Svg.path
                            [ SvgAttr.d ("M " ++ String.fromInt (mainModel.cellSizeInPixels.width * defaultTileCardGridSize) ++ " 0 L 0 0 0 " ++ String.fromInt (mainModel.cellSizeInPixels.height * defaultTileCardGridSize))
                            , SvgAttr.fill "none"
                            , SvgAttr.stroke "Black"
                            , SvgAttr.strokeWidth "1"
                            , SvgAttr.opacity "0.8"
                            ]
                            []
                        ]
                    ]
                , Svg.rect
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    , SvgAttr.fill "url(#tileCardGrid)"
                    , SvgAttr.opacity "0.8"
                    ]
                    []
                , Svg.Lazy.lazy renderMainGridPlacedTileCards mainModel
                , case mainModel.mainGrid.tool of
                    PlaceSelectionTool ->
                        let
                            selectedTileCard =
                                Maybe.andThen (\id -> Dict.get id mainModel.cardDesigns) mainModel.selectedTileCardId
                        in
                        Maybe.withDefault (Svg.rect [] [])
                            (Maybe.map2
                                (renderSelectedTileCardInMainGrid
                                    mainModel.mainGrid.cellSizeInPixels
                                    mainModel.mainGrid.tileCardSizeInCells
                                    mainModel.mainGrid.currentTileCardRotation
                                )
                                mainModel.mainGrid.mouseAtPoint
                                selectedTileCard
                            )

                    DeleteTool ->
                        -- Show only outline
                        let
                            deleteSelectionOutlineRect ( mX, mY ) =
                                let
                                    tileCardSizeInPx =
                                        { width = mainModel.mainGrid.cellSizeInPixels.width * mainModel.mainGrid.tileCardSizeInCells.width
                                        , height = mainModel.mainGrid.cellSizeInPixels.height * mainModel.mainGrid.tileCardSizeInCells.height
                                        }

                                    coords =
                                        pointToTileCardCoordinates ( mX, mY ) mainModel.mainGrid.cellSizeInPixels mainModel.mainGrid.tileCardSizeInCells

                                    gridRect =
                                        { defaultGridRect | start = coords.absTileCardStartInPx, size = tileCardSizeInPx, fillColor = "red", opacity = 0.7 }
                                in
                                cellRectAbs gridRect [] []
                        in
                        Maybe.withDefault (Svg.rect [] []) (Maybe.map deleteSelectionOutlineRect mainModel.mainGrid.mouseAtPoint)

                    _ ->
                        Svg.g [] []
                ]
            ]
        ]


renderPlacedCell : MainGrid -> TileCardPlacement -> GridSize -> Int -> CellType -> Svg msg
renderPlacedCell mainGrid placement tileCardSizeInCells idxInTileCard cell =
    let
        absStartInPixels =
            addGridPoint placement.tileCardStartPx
                (calcCellOffsetInPx (getGridPointForIndex idxInTileCard tileCardSizeInCells.width) mainGrid.cellSizeInPixels)

        cellRect =
            { defaultGridRect
                | start = absStartInPixels
                , size = mainGrid.cellSizeInPixels
                , strokeColor = "slategrey"
                , strokeOpacity = 0.8
                , fillColor =
                    case cell of
                        Open ->
                            -- lavender blue
                            "#CED8F7"

                        Closed ->
                            "slategrey"
            }
    in
    cellRectAbs cellRect [] []


renderPlacedTileCardCellsOnMainGrid mainGrid placement tileCard =
    Array.indexedMap (renderPlacedCell mainGrid placement tileCard.sizeInCells) tileCard.cells


renderPlacedTileCardOnMainGrid : MainGrid -> ( TileCardPlacement, TileCard ) -> Svg msg
renderPlacedTileCardOnMainGrid mainGrid ( placement, tileCard ) =
    let
        tileCardRect : GridRect
        tileCardRect =
            { defaultGridRect
                | start = placement.tileCardStartPx
                , size =
                    { width = tileCard.sizeInCells.width * mainGrid.cellSizeInPixels.width
                    , height = tileCard.sizeInCells.height * mainGrid.cellSizeInPixels.height
                    }
                , strokeColor = "royalblue"
                , strokeWidth = 4
                , opacity = 0.8
                , rotation = placement.rotation
            }
    in
    cellRectAbs tileCardRect [] (Array.toList (renderPlacedTileCardCellsOnMainGrid mainGrid placement tileCard))


renderMainGridPlacedTileCards : MainModel -> Svg msg
renderMainGridPlacedTileCards model =
    let
        -- naive implementation with a simple map, when and output an empty Element for an empty tilecard
        {- filterAndMap : Maybe TileCardPlacement -> List ( TileCardPlacement, TileCard ) -> List ( TileCardPlacement, TileCard ) -}
        filterAndMap renderF placement acc =
            case placement of
                Just placed ->
                    case Dict.get placed.tileCardId model.cardDesigns of
                        Just design ->
                            renderF ( placed, design ) :: acc

                        Nothing ->
                            acc

                Nothing ->
                    acc

        tileCardsSvgs =
            Array.foldl (filterAndMap (renderPlacedTileCardOnMainGrid model.mainGrid)) [] model.mainGrid.tileCards
    in
    Svg.g []
        tileCardsSvgs


type alias PointF =
    { x : Float
    , y : Float
    }


centreOf : GridRect -> PointF
centreOf gridRect =
    { x = toFloat gridRect.start.x + (toFloat gridRect.size.width / 2)
    , y = toFloat gridRect.start.y + (toFloat gridRect.size.height / 2)
    }


cellRectAbs : GridRect -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
cellRectAbs gridRectInPx attrs inner =
    let
        r =
            Svg.rect
                ([ SvgAttr.width <| String.fromInt gridRectInPx.size.width
                 , SvgAttr.height <| String.fromInt gridRectInPx.size.height
                 , SvgAttr.x <| String.fromInt gridRectInPx.start.x
                 , SvgAttr.y <| String.fromInt gridRectInPx.start.y
                 , SvgAttr.opacity <| String.fromFloat gridRectInPx.opacity
                 , SvgAttr.strokeWidth <| String.fromFloat gridRectInPx.strokeWidth
                 , SvgAttr.stroke <| gridRectInPx.strokeColor
                 , SvgAttr.strokeOpacity <| String.fromFloat gridRectInPx.strokeOpacity
                 , SvgAttr.fill <| gridRectInPx.fillColor
                 ]
                    ++ attrs
                )
    in
    if gridRectInPx.rotation /= 0 then
        let
            rotationOrigin =
                centreOf gridRectInPx
        in
        Svg.g
            [ SvgAttr.transform <| "rotate(" ++ String.fromInt gridRectInPx.rotation ++ "," ++ String.fromFloat rotationOrigin.x ++ "," ++ String.fromFloat rotationOrigin.y ++ ")"
            ]
            (r [] :: inner)

    else
        Svg.g []
            (r [] :: inner)


calcCellOffsetInPx : GridPoint -> GridSize -> GridPoint
calcCellOffsetInPx cellCoordInTileCard cellSizeInPx =
    { x = cellCoordInTileCard.x * cellSizeInPx.width, y = cellCoordInTileCard.y * cellSizeInPx.height }


addGridPoint first second =
    { x = first.x + second.x
    , y = first.y + second.y
    }


type alias GridRect =
    { start : GridPoint
    , size : GridSize
    , rotation : Int
    , fillColor : String
    , strokeColor : String
    , strokeWidth : Float
    , strokeOpacity : Float
    , opacity : Float
    }


defaultGridRect : GridRect
defaultGridRect =
    { start = { x = 0, y = 0 }
    , size = { width = 0, height = 0 }
    , rotation = 0
    , fillColor = "none"
    , strokeColor = "black"
    , strokeWidth = 0.5
    , strokeOpacity = 1
    , opacity = 1
    }


renderSelectedTileCardInMainGrid cellSizeInPx tileCardSizeInCells rotate ( mX, mY ) tileCard =
    let
        coords =
            pointToTileCardCoordinates ( mX, mY ) cellSizeInPx tileCardSizeInCells

        renderTileCardCell idx cell =
            let
                relCellCoords =
                    getGridPointForIndex idx tileCardSizeInCells.width

                offset =
                    calcCellOffsetInPx relCellCoords cellSizeInPx

                absoluteCoordInPx =
                    addGridPoint coords.absTileCardStartInPx offset

                cellGridRectPx =
                    { defaultGridRect
                        | start = absoluteCoordInPx
                        , size = cellSizeInPx
                        , strokeColor = "red"
                        , strokeOpacity = 0.5
                        , strokeWidth = 2
                        , opacity = 0.5
                        , fillColor =
                            case cell of
                                Open ->
                                    "white"

                                Closed ->
                                    "black"
                    }
            in
            cellRectAbs cellGridRectPx [] []

        renderTileCardOnGrid =
            Array.indexedMap (\idx cell -> renderTileCardCell idx cell) tileCard.cells
    in
    let
        tileCardSizeInPx =
            { width = cellSizeInPx.width * tileCardSizeInCells.width
            , height = cellSizeInPx.height * tileCardSizeInCells.height
            }

        tileCardRectInPx =
            { defaultGridRect
                | start = coords.absTileCardStartInPx
                , size = tileCardSizeInPx
                , fillColor = "magenta"
                , rotation = rotate
                , opacity = 0.5
            }
    in
    cellRectAbs tileCardRectInPx [] (Array.toList renderTileCardOnGrid)


viewMainGridDebugInfo : MainGrid -> Html msgType
viewMainGridDebugInfo mainGrid =
    div []
        [ span [] [ Html.text "(MouseAt) > " ]
        , case mainGrid.mouseAtPoint of
            Just ( x, y ) ->
                let
                    coords =
                        pointToTileCardCoordinates ( x, y ) mainGrid.cellSizeInPixels mainGrid.tileCardSizeInCells

                    coordsToStr coord =
                        "(" ++ String.fromInt coord.x ++ "," ++ String.fromInt coord.y ++ ")"
                in
                span []
                    [ span [] [ Html.text (String.fromFloat x ++ "," ++ String.fromFloat y) ]
                    , span [] [ Html.text ("\t(CellCoord) > " ++ coordsToStr coords.absCellCoord) ]
                    , span [] [ Html.text ("\t(TileCardCoord) > " ++ coordsToStr coords.absTileCardCoord) ]
                    ]

            Nothing ->
                span [] [ Html.text "nothing" ]
        , span [] [ Html.text ("\t(rotation) > " ++ String.fromInt mainGrid.currentTileCardRotation) ]
        ]


viewTileCardsOverView tileCards selectedTileCardId cellSizeInPixels mouseOverCellInTileCard =
    let
        selectedTileCard =
            Maybe.andThen (\id -> Dict.get id tileCards) selectedTileCardId
    in
    div []
        [ h4 [] [ Html.text "All Tile Cards" ]
        , Keyed.node "div" [] (List.map (viewKeyedTileCard mouseOverCellInTileCard cellSizeInPixels selectedTileCard) (Dict.values tileCards))
        , hr [] []
        ]


viewKeyedTileCard : Maybe TileCardSelectedCell -> GridSize -> Maybe TileCard -> TileCard -> ( String, Html UpdateMsg )
viewKeyedTileCard mouseOverCellInTileCard cellSizeInPixels selectedTileCard tileCard =
    let
        isSelected =
            Maybe.withDefault False (Maybe.map (\tc -> tc.id == tileCard.id) selectedTileCard)
    in
    ( String.fromInt tileCard.id, Html.Lazy.lazy (viewTileCard tileCard cellSizeInPixels isSelected) mouseOverCellInTileCard )


viewTileCard : TileCard -> GridSize -> Bool -> Maybe TileCardSelectedCell -> Html UpdateMsg
viewTileCard tileCard cellSizeInPixels isSelected mouseOverCellInTileCard =
    let
        cardWidthInPixels =
            tileCard.sizeInCells.width * cellSizeInPixels.width

        cardWidthInPixelsStr =
            String.fromInt cardWidthInPixels

        cardHeightInPixelsStr =
            String.fromInt (tileCard.sizeInCells.height * cellSizeInPixels.height)
    in
    div [ Html.Attributes.style "display" "inline-flex" ]
        [ span [] [ Html.text <| String.fromInt tileCard.id ]
        , Svg.svg
            [ SvgAttr.width cardWidthInPixelsStr
            , SvgAttr.height cardHeightInPixelsStr
            , SvgAttr.viewBox ("0 0 " ++ cardWidthInPixelsStr ++ " " ++ cardHeightInPixelsStr)
            , Svg.Events.onMouseOut ClearMouseOverCellInTileCard
            ]
            (Svg.rect
                [ SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.width cardWidthInPixelsStr
                , SvgAttr.height cardHeightInPixelsStr
                , SvgAttr.stroke "gray"
                , SvgAttr.fill "none"
                ]
                []
                :: Array.toList (renderCells tileCard cellSizeInPixels mouseOverCellInTileCard)
                ++ (if isSelected then
                        [ Svg.path
                            [ SvgAttr.stroke "Red"
                            , SvgAttr.strokeWidth "8.0"
                            , SvgAttr.fill "none"
                            , SvgAttr.d ("M 0 0 h " ++ cardWidthInPixelsStr ++ " v " ++ cardWidthInPixelsStr ++ " h -" ++ cardWidthInPixelsStr ++ "z")
                            ]
                            []
                        ]

                    else
                        []
                   )
            )
        , if isSelected then
            Html.button [ Html.Events.onClick DeselectTileCardDesign ]
                [ Html.text "Deselect" ]

          else
            Html.button [ Html.Events.onClick (SelectTileCardDesign tileCard) ] [ Html.text "Select" ]
        , Html.button [ Html.Events.onClick (CopyTileCardDesign tileCard) ] [ Html.text "Copy" ]
        , Html.button [ Html.Events.onClick (DeleteTileCardDesign tileCard) ] [ Html.text "Delete" ]
        ]


renderCells : TileCard -> GridSize -> Maybe TileCardSelectedCell -> Array (Svg UpdateMsg)
renderCells tileCard cellSizeInPx mouseOverCellInTileCard =
    Array.indexedMap (\idx cell -> renderCell tileCard.id idx cell cellSizeInPx tileCard.sizeInCells.width mouseOverCellInTileCard) tileCard.cells


renderCell tileCardId idx cellType cellSizeInPx tileWidthInCells mouseOverCellInTileCard =
    let
        cellCoords =
            getGridPointForIndex idx tileWidthInCells
    in
    Svg.rect
        [ SvgAttr.width <| String.fromInt cellSizeInPx.width
        , SvgAttr.height <| String.fromInt cellSizeInPx.height
        , SvgAttr.x <| String.fromInt (cellCoords.x * cellSizeInPx.width)
        , SvgAttr.y <| String.fromInt (cellCoords.y * cellSizeInPx.height)
        , SvgAttr.opacity "0.8"
        , case mouseOverCellInTileCard of
            Just selectedInfo ->
                if selectedInfo.tileCardId == tileCardId && selectedInfo.cellCoord == cellCoords then
                    case cellType of
                        Open ->
                            SvgAttr.fill "Yellow"

                        Closed ->
                            SvgAttr.fill "Black"

                else
                    case cellType of
                        Open ->
                            SvgAttr.fill "WhiteSmoke"

                        Closed ->
                            SvgAttr.fill "DarkGray"

            Nothing ->
                case cellType of
                    Open ->
                        SvgAttr.fill "WhiteSmoke"

                    Closed ->
                        SvgAttr.fill "DarkGray"
        , SvgAttr.strokeWidth "0.5"
        , SvgAttr.stroke "Gray"
        , Svg.Events.onMouseOver (MouseOverHighlightCellInTileCardUpdate { tileCardId = tileCardId, cellCoord = cellCoords })
        , Svg.Events.onClick (ToggleCellTypeInTileCard { tileCardId = tileCardId, cellCoord = cellCoords })
        ]
        []
