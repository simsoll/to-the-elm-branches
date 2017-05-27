module Main exposing (..)

import Html exposing (Html, text, div, Attribute)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import Random
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)
import Color exposing (..)
import Json.Decode exposing (int, string, float, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Http
import Tiled


-- elm-live Main.elm --open --pushstate --output=elm.js


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { viewWidth : Int
    , viewHeight : Int
    , pixelSize : Int
    , player : Player
    , actions : List Action
    , randomNumber : Int
    , framesPerSecond : Int
    , timeElapsedInSeconds : Float
    , map : Tiled.Map
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Pixel =
    { color : Color
    , position : Position
    }


type alias Sprite =
    List Pixel


type alias Player =
    { velocity : Float
    , position : Position
    , shotsFired : Int
    , sprite : Sprite
    }


type alias Velocity =
    Float


type Action
    = Shot
    | MoveLeft Velocity
    | MoveRight Velocity
    | MoveDown Velocity
    | MoveUp Velocity


type alias RawPixel =
    { x : Int
    , y : Int
    , r : Int
    , g : Int
    , b : Int
    , a : Float
    }


rawPixelDecoder : Decoder RawPixel
rawPixelDecoder =
    decode RawPixel
        |> required "x" int
        |> required "y" int
        |> required "r" int
        |> required "g" int
        |> required "b" int
        |> required "a" float


rawPixelListDecoder : Decoder (List RawPixel)
rawPixelListDecoder =
    list rawPixelDecoder


toPixel : RawPixel -> Pixel
toPixel rawPixel =
    { position = Position rawPixel.x rawPixel.y
    , color = rgba rawPixel.r rawPixel.g rawPixel.b rawPixel.a
    }


pixelSize : number
pixelSize =
    10


viewWidth : number
viewWidth =
    800


viewHeight : number
viewHeight =
    600


model : Model
model =
    { viewWidth = viewWidth
    , viewHeight = viewHeight
    , pixelSize = pixelSize
    , player =
        { velocity = 0.05
        , position = Position 10 10
        , shotsFired = 0
        , sprite =
            [ { position = Position 0 0
              , color = rgba 150 150 150 1
              }
            , { position = Position 1 0
              , color = rgba 150 150 150 1
              }
            , { position = Position 0 1
              , color = rgba 150 150 150 1
              }
            , { position = Position 1 1
              , color = rgba 150 150 150 1
              }
            ]
        }
    , actions = []
    , randomNumber = 0
    , framesPerSecond = 0
    , timeElapsedInSeconds = 0
    , map =
        { height = 0
        , layers = []
        , tileHeight = 0
        , tileSets = []
        , tileWidth = 0
        , width = 0
        }
    }


init : ( Model, Cmd Msg )
init =
    let
        loadPlayerSpriteCommand =
            createHttpCommand "/assets/avatar.json" rawPixelListDecoder LoadPlayerSprite

        loadMapCommand =
            createHttpCommand "/assets/level-one.json" Tiled.mapDecoder LoadMap
    in
        ( model, Cmd.batch [ loadPlayerSpriteCommand, loadMapCommand ] )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | LoadPlayerSprite (Result Http.Error (List RawPixel))
    | LoadMap (Result Http.Error Tiled.Map)


updateSprite : Sprite -> Player -> Player
updateSprite sprite player =
    { player | sprite = sprite }


createHttpCommand : String -> Json.Decode.Decoder a -> (Result Http.Error a -> Msg) -> Cmd Msg
createHttpCommand url decoder msg =
    Http.get url decoder |> Http.send msg


toAction : Model -> Int -> Maybe Action
toAction model keyCode =
    case Key.fromCode keyCode of
        Just key ->
            case key of
                Space ->
                    Just Shot

                ArrowLeft ->
                    Just (MoveLeft model.player.velocity)

                ArrowUp ->
                    Just (MoveUp model.player.velocity)

                ArrowRight ->
                    Just (MoveRight model.player.velocity)

                ArrowDown ->
                    Just (MoveDown model.player.velocity)

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate time ->
            ( model |> applyTime time, Cmd.none )

        KeyDown keyCode ->
            let
                action =
                    keyCode |> toAction model
            in
                case action of
                    Just action ->
                        case model.actions |> List.member action of
                            True ->
                                ( model, Cmd.none )

                            False ->
                                ( { model | actions = action :: model.actions }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        KeyUp keyCode ->
            let
                action =
                    keyCode |> toAction model
            in
                case action of
                    Just action ->
                        let
                            isReleased =
                                isActionsDifferent action
                        in
                            ( { model | actions = model.actions |> List.filter isReleased }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        LoadPlayerSprite (Ok data) ->
            let
                sprite =
                    data |> List.map toPixel

                player =
                    model.player |> updateSprite sprite
            in
                ( { model | player = player }, Cmd.none )

        LoadPlayerSprite (Err _) ->
            ( model, Cmd.none )

        LoadMap (Ok map) ->
            ( { model | map = map }, Cmd.none )

        LoadMap (Err message) ->
            let
                _ =
                    Debug.log "Error: " message
            in
                ( model, Cmd.none )


isActionsDifferent : Action -> Action -> Bool
isActionsDifferent firstAction secondAction =
    firstAction /= secondAction


applyTime : Time -> Model -> Model
applyTime deltaTime model =
    { model
        | player = List.foldl (applyAction deltaTime) model.player model.actions
        , framesPerSecond = round (60 / Time.inSeconds deltaTime)
        , timeElapsedInSeconds = model.timeElapsedInSeconds + Time.inSeconds deltaTime
        , randomNumber =
            case round model.timeElapsedInSeconds % 1 of
                0 ->
                    round model.timeElapsedInSeconds
                        |> Random.initialSeed
                        |> Random.step (Random.int 1 100)
                        |> Tuple.first

                _ ->
                    model.randomNumber
    }


add : Position -> Position -> Position
add a b =
    Position (a.x + b.x) (a.y + b.y)


applyAction : Time -> Action -> Player -> Player
applyAction deltaTime action player =
    case action of
        Shot ->
            { player | shotsFired = player.shotsFired + 1 }

        MoveLeft velocity ->
            { player | position = add (player.position) (Position (-1 * round (player.velocity * Time.inMilliseconds deltaTime)) 0) }

        MoveRight velocity ->
            { player | position = add (player.position) (Position (round (player.velocity * Time.inMilliseconds deltaTime)) 0) }

        MoveUp velocity ->
            { player | position = add (player.position) (Position 0 (-1 * round (player.velocity * Time.inMilliseconds deltaTime))) }

        MoveDown velocity ->
            { player | position = add (player.position) (Position 0 (round (player.velocity * Time.inMilliseconds deltaTime))) }



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ div
            [ style
                [ ( "border-radius", "0%" )
                , ( "box-shadow", drawPlayer model.player )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "margin", "0" )
                , ( "padding", "0" )
                , ( "overflow", "hidden" )
                ]
            ]
            []
        , text (toString model.map)
        ]



-- [ div [ style [ ( "position", "fixed" ), ( "bottom", "0" ) ] ] [ text (toString model) ] ]


translatePixel : Position -> Pixel -> Pixel
translatePixel pos pixel =
    { pixel | position = add pos pixel.position }


translateSprite : Position -> Sprite -> Sprite
translateSprite pos sprite =
    sprite |> List.map (translatePixel pos)


drawPlayer : Player -> String
drawPlayer player =
    translateSprite player.position player.sprite
        |> drawSprite


drawSprite : Sprite -> String
drawSprite sprite =
    sprite
        |> List.map pixelStyle
        |> String.join ","


pixelStyle : Pixel -> String
pixelStyle pixel =
    let
        { red, green, blue, alpha } =
            Color.toRgb pixel.color

        { x, y } =
            pixel.position
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ") " ++ toString (pixelSize * x) ++ "px " ++ toString (pixelSize * y) ++ "px 0px " ++ toString (pixelSize / 2) ++ "px"



-- "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ") " ++ toString (pixelSize * x) ++ "px " ++ toString (pixelSize * y) ++ "px 0px " ++ toString (pixelSize) ++ "px"
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
