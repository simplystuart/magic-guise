module Main exposing (main)

import Array
import Browser
import Browser.Events
import Dict
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import Random



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- FLAGS


type alias Flags =
    { debug : Bool
    , pixelWidth : Int
    , wallpaperRepeat : { x : Float, y : Float }
    , wallpaperSize : Size
    }


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map4 Flags
        (Json.Decode.oneOf
            [ Json.Decode.field "debug" Json.Decode.bool
            , Json.Decode.succeed False
            ]
        )
        (Json.Decode.field "pixelWidth" Json.Decode.int)
        (Json.Decode.map2 (\x y -> { x = x, y = y })
            (Json.Decode.at [ "wallpaperRepeat", "x" ] Json.Decode.float)
            (Json.Decode.at [ "wallpaperRepeat", "y" ] Json.Decode.float)
        )
        (Json.Decode.map2 Size
            (Json.Decode.at [ "wallpaperSize", "height" ] Json.Decode.int)
            (Json.Decode.at [ "wallpaperSize", "width" ] Json.Decode.int)
        )



-- MODEL


type Model
    = Loading Data
    | Loaded Data Wallpaper
    | Lost Data Wallpaper
    | Won Data Wallpaper
    | Error Json.Decode.Error


type alias Data =
    { clock : Float
    , debug : Bool
    , piece : Piece
    , pixelWidth : Int
    , resolution : Size
    , wallpaperSize : Size
    }


type alias Piece =
    { height : Int
    , width : Int
    , x : Float
    , y : Float
    , depth : Int
    , velocity : { x : Int, y : Int }
    }


type alias Size =
    { height : Int
    , width : Int
    }


type alias Wallpaper =
    Dict.Dict Int (List Pixel)


type alias Pixel =
    { hue : Int
    , saturation : Int
    , lightness : Int
    , x : Int
    , y : Int
    , shift : Int
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init value =
    case Json.Decode.decodeValue flagsDecoder value of
        Ok flags ->
            ( Loading
                { clock = 30000
                , debug = flags.debug
                , piece =
                    { height = 18
                    , width = 18
                    , x = 48
                    , y = 9
                    , depth = 10
                    , velocity = { x = 0, y = 0 }
                    }
                , pixelWidth = flags.pixelWidth
                , resolution =
                    { height =
                        round <|
                            toFloat flags.wallpaperSize.height
                                * flags.wallpaperRepeat.y
                    , width =
                        round <|
                            toFloat flags.wallpaperSize.width
                                * flags.wallpaperRepeat.x
                    }
                , wallpaperSize = flags.wallpaperSize
                }
            , Random.generate NewWallpaper <|
                wallpaperGenerator
                    ( flags.wallpaperSize.width, flags.wallpaperSize.height )
            )

        Err err ->
            ( Error err, Cmd.none )


wallpaperGenerator : ( Int, Int ) -> Random.Generator Wallpaper
wallpaperGenerator ( width, height ) =
    Random.map
        (Dict.fromList << List.indexedMap Tuple.pair)
        (Random.list height <|
            Random.list width <|
                Random.map3
                    (\hue saturation lightness ->
                        Pixel hue saturation lightness 0 0 0
                    )
                    (Random.int 0 359)
                    (Random.constant 60)
                    (Random.constant 60)
        )



-- UPDATE


type Msg
    = NewWallpaper Wallpaper
    | KeyDown Direction
    | KeyUp Direction
    | Stop
    | Animate Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading data ->
            case msg of
                NewWallpaper wallpaper ->
                    ( Loaded data wallpaper
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Loaded data wallpaper ->
            case msg of
                KeyDown direction ->
                    case direction of
                        Enter ->
                            let
                                centerX =
                                    (toFloat data.resolution.width / 2.0)
                                        - (toFloat data.piece.width / 2.0)

                                centerY =
                                    (toFloat data.resolution.height / 2.0)
                                        - (toFloat data.piece.height / 2.0)
                            in
                            if
                                data.piece.x
                                    == centerX
                                    && data.piece.y
                                    == centerY
                            then
                                ( Won data wallpaper, Cmd.none )

                            else
                                ( Lost data wallpaper, Cmd.none )

                        _ ->
                            ( Loaded
                                { data | piece = move On direction data.piece }
                                wallpaper
                            , Cmd.none
                            )

                KeyUp direction ->
                    ( Loaded { data | piece = move Off direction data.piece }
                        wallpaper
                    , Cmd.none
                    )

                Stop ->
                    ( Loaded { data | piece = stop data.piece } wallpaper
                    , Cmd.none
                    )

                Animate delta ->
                    if data.clock - delta > 0 then
                        ( Loaded (animate delta data) wallpaper, Cmd.none )

                    else
                        ( Lost { data | clock = data.clock - delta } wallpaper
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


move : Toggle -> Direction -> Piece -> Piece
move toggle direction ({ velocity } as piece) =
    let
        modifier : Int
        modifier =
            case toggle of
                On ->
                    1

                Off ->
                    0

        change : { x : Int, y : Int }
        change =
            case direction of
                Up ->
                    { velocity | y = modifier * -1 }

                Right ->
                    { velocity | x = modifier * 1 }

                Down ->
                    { velocity | y = modifier * 1 }

                Left ->
                    { velocity | x = modifier * -1 }

                _ ->
                    velocity
    in
    { piece | velocity = change }


stop : Piece -> Piece
stop ({ velocity } as piece) =
    let
        change : { x : Int, y : Int }
        change =
            { velocity | x = 0, y = 0 }
    in
    { piece | velocity = change }


type Toggle
    = On
    | Off


animate : Float -> Data -> Data
animate delta ({ clock, piece, resolution, wallpaperSize } as data) =
    let
        ( velocityX, velocityY ) =
            ( toFloat piece.velocity.x, toFloat piece.velocity.y )

        rate : Float
        rate =
            delta / 20

        pieceChange : Piece
        pieceChange =
            { piece
                | x =
                    (piece.x + rate * velocityX)
                        |> max (toFloat wallpaperSize.width)
                        |> min
                            (toFloat
                                (resolution.width
                                    - wallpaperSize.width
                                    - piece.height
                                )
                            )
                , y =
                    (piece.y + rate * velocityY)
                        |> max 0.0
                        |> min (toFloat (resolution.height - piece.height))
            }
    in
    { data | clock = clock - delta, piece = pieceChange }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Animate
        , Browser.Events.onKeyDown <|
            Json.Decode.map (toKeyMsg KeyDown) <|
                Json.Decode.field "key" Json.Decode.string
        , Browser.Events.onKeyUp <|
            Json.Decode.map (toKeyMsg KeyUp) <|
                Json.Decode.field "key" Json.Decode.string
        , Browser.Events.onVisibilityChange
            (\visibility ->
                case visibility of
                    Browser.Events.Hidden ->
                        Stop

                    _ ->
                        NoOp
            )
        ]


toKeyMsg : (Direction -> Msg) -> String -> Msg
toKeyMsg tag key =
    case key of
        "ArrowUp" ->
            tag Up

        "ArrowRight" ->
            tag Right

        "ArrowDown" ->
            tag Down

        "ArrowLeft" ->
            tag Left

        "Enter" ->
            tag Enter

        "Return" ->
            tag Enter

        _ ->
            NoOp


type Direction
    = Up
    | Right
    | Down
    | Left
    | Enter



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Loading _ ->
            { title = "magic guise | loading..."
            , body = []
            }

        Loaded data wallpaper ->
            { title = "magic guise | play"
            , body = [ viewLoadedBody data wallpaper ]
            }

        Lost data wallpaper ->
            { title = "magic guise | game over!"
            , body = [ viewLostBody data wallpaper ]
            }

        Won data wallpaper ->
            { title = "magic guise | you won!"
            , body = [ viewWonBody data wallpaper ]
            }

        Error err ->
            { title = "magic guise | error!"
            , body = [ Html.text <| Json.Decode.errorToString err ]
            }


viewLoadedBody : Data -> Wallpaper -> Html.Html msg
viewLoadedBody data wallpaper =
    Html.div [ Html.Attributes.id "screen" ]
        (case makePicture data wallpaper of
            Just pixels ->
                [ viewPicture data pixels
                , Html.div [ Html.Attributes.id "top" ]
                    [ Html.h1 [] [ Html.text "magic guise" ]
                    , viewClock data.clock
                    ]
                ]

            Nothing ->
                [ viewRenderError ]
        )


viewLostBody : Data -> Wallpaper -> Html.Html Msg
viewLostBody data wallpaper =
    Html.div [ Html.Attributes.id "screen" ]
        (case makePicture data wallpaper of
            Just pixels ->
                [ viewPicture data pixels
                , Html.div [ Html.Attributes.id "top" ]
                    [ Html.h1 [] [ Html.text "magic guise" ]
                    , viewClock data.clock
                    ]
                , Html.div [ Html.Attributes.id "content" ]
                    [ Html.p []
                        [ Html.text <|
                            "Instructions:"
                                ++ " center the square on the screen"
                                ++ " and press enter."
                                ++ " A centered square wins!"
                        ]
                    , Html.p []
                        [ Html.text <|
                            "Warning:"
                                ++ " may cause eye strain"
                                ++ " or other issues."
                                ++ " Play with caution."
                        ]
                    , Html.a [ Html.Attributes.href "/" ]
                        [ Html.text "play again" ]
                    ]
                ]

            Nothing ->
                [ viewRenderError ]
        )


viewWonBody : Data -> Wallpaper -> Html.Html Msg
viewWonBody data wallpaper =
    Html.div [ Html.Attributes.id "screen" ]
        (case makePicture data wallpaper of
            Just pixels ->
                [ viewPicture data pixels
                , Html.div [ Html.Attributes.id "top" ]
                    [ Html.h1 [] [ Html.text "magic guise" ]
                    , viewClock data.clock
                    ]
                , Html.div [ Html.Attributes.id "content" ]
                    [ Html.p [] [ Html.text <| "YOU WON! 🎉" ]
                    , Html.a [ Html.Attributes.href "/" ]
                        [ Html.text "play again" ]
                    ]
                ]

            Nothing ->
                [ viewRenderError ]
        )


makePicture : Data -> Wallpaper -> Maybe (List Pixel)
makePicture data wallpaper =
    Maybe.map (List.reverse << Tuple.second) <|
        List.foldr (\index -> Maybe.andThen (addNextPixel data wallpaper index))
            (Just ( Array.empty, [] ))
            (List.reverse <|
                List.range 0
                    (data.resolution.width * data.resolution.height - 1)
            )


addNextPixel :
    Data
    -> Wallpaper
    -> Int
    -> ( Array.Array Pixel, List Pixel )
    -> Maybe ( Array.Array Pixel, List Pixel )
addNextPixel data wallpaper index ( pattern, pixels ) =
    let
        ( x, y ) =
            ( modBy data.resolution.width index
            , index // data.resolution.width
            )

        ( pieceX, pieceY ) =
            ( round data.piece.x, round data.piece.y )

        shift : Int
        shift =
            if
                (x >= pieceX)
                    && (x < pieceX + data.piece.width)
                    && (y >= pieceY)
                    && (y < pieceY + data.piece.height)
            then
                data.piece.depth

            else
                0

        patternIndex : Int
        patternIndex =
            modBy data.wallpaperSize.width x

        patternIndexWithShift : Int
        patternIndexWithShift =
            modBy data.wallpaperSize.width (patternIndex + shift)

        maybePattern : Maybe (Array.Array Pixel)
        maybePattern =
            if x == 0 then
                Maybe.map Array.fromList <|
                    Dict.get (modBy data.wallpaperSize.height y) wallpaper

            else if patternIndex == 0 && shift == 0 then
                Just <|
                    Array.fromList <|
                        List.reverse <|
                            List.take data.wallpaperSize.width pixels

            else
                Just pattern

        addMeta : Pixel -> Pixel
        addMeta pixel =
            { pixel | x = x, y = y, shift = shift }
    in
    Maybe.map2
        (\pixel pattern_ -> ( pattern_, addMeta pixel :: pixels ))
        (Maybe.andThen
            (\pattern_ -> Array.get patternIndexWithShift pattern_)
            maybePattern
        )
        maybePattern


viewPicture : Data -> List Pixel -> Html.Html msg
viewPicture data pixels =
    let
        newRow children =
            Html.div [ Html.Attributes.class "row" ] children

        render =
            List.foldr
                (\pixel ( rows, row ) ->
                    if List.length row < data.resolution.width then
                        ( rows, viewPixel data pixel :: row )

                    else
                        ( newRow row :: rows
                        , [ viewPixel data pixel ]
                        )
                )
                ( [], [] )
                pixels
    in
    Html.div [ Html.Attributes.id "picture" ]
        (newRow (Tuple.second render) :: Tuple.first render)


viewPixel : Data -> Pixel -> Html.Html msg
viewPixel data pixel =
    let
        color : String
        color =
            "hsl("
                ++ String.fromInt pixel.hue
                ++ ","
                ++ String.fromInt pixel.saturation
                ++ "%,"
                ++ String.fromInt pixel.lightness
                ++ "%)"

        debugAttributes : List (Html.Attribute msg)
        debugAttributes =
            if data.debug && pixel.shift > 0 then
                [ Html.Attributes.style "opacity" "0.2" ]

            else
                []

        content : List (Html.Html msg)
        content =
            if data.debug then
                [ Html.pre [ Html.Attributes.style "display" "none" ]
                    [ Html.text <|
                        Json.Encode.encode 0 <|
                            Json.Encode.object
                                [ ( "x", Json.Encode.int pixel.x )
                                , ( "y", Json.Encode.int pixel.y )
                                , ( "shift", Json.Encode.int pixel.shift )
                                ]
                    ]
                ]

            else
                []
    in
    Html.div
        ([ Html.Attributes.class "pixel"
         , Html.Attributes.style "background-color" color
         ]
            ++ debugAttributes
        )
        content


viewClock : Float -> Html.Html msg
viewClock clock =
    Html.div [ Html.Attributes.id "clock" ]
        [ Html.text <|
            "0:"
                ++ (String.padLeft 2 '0' <|
                        String.fromInt <|
                            max 0 <|
                                round <|
                                    clock
                                        / 1000
                   )
        ]


viewRenderError : Html.Html msg
viewRenderError =
    Html.div [] [ Html.text "error rendering screen..." ]
