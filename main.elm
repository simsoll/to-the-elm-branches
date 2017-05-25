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
import Task


-- elm-live Main.elm --open --pushstate --output=elm.js


main : Program Never Model Msg
main =
    Html.program
        { init = init "/avatar.json"
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


updateSprite : Sprite -> Player -> Player
updateSprite sprite player =
    { player | sprite = sprite }


type alias RawPixel =
    { x : Int
    , y : Int
    , r : Int
    , g : Int
    , b : Int
    }


rawPixelDecoder : Decoder RawPixel
rawPixelDecoder =
    decode RawPixel
        |> required "x" int
        |> required "y" int
        |> required "r" int
        |> required "g" int
        |> required "b" int


rawPixelListDecoder : Decoder (List RawPixel)
rawPixelListDecoder =
    list rawPixelDecoder


toPixel : RawPixel -> Pixel
toPixel rawPixel =
    { position = Position rawPixel.x rawPixel.y
    , color = rgb rawPixel.r rawPixel.g rawPixel.b
    }


pixelSize : number
pixelSize =
    4


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
              , color = rgb 150 150 150
              }
            , { position = Position 1 0
              , color = rgb 150 150 150
              }
            , { position = Position 0 1
              , color = rgb 150 150 150
              }
            , { position = Position 1 1
              , color = rgb 150 150 150
              }
            ]
        }
    , actions = []
    , randomNumber = 0
    , framesPerSecond = 0
    , timeElapsedInSeconds = 0
    }


init : String -> ( Model, Cmd Msg )
init url =
    ( model, loadRawPixels url )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | LoadData (Result Http.Error (List RawPixel))


type alias Velocity =
    Float


type Action
    = Shot
    | MoveLeft Velocity
    | MoveRight Velocity
    | MoveDown Velocity
    | MoveUp Velocity


loadRawPixels : String -> Cmd Msg
loadRawPixels url =
    let
        request =
            Http.get url rawPixelListDecoder
    in
        Http.send LoadData request


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

        LoadData (Ok data) ->
            let
                sprite =
                    data |> List.map toPixel

                player =
                    model.player |> updateSprite sprite
            in
                ( { model | player = player }, Cmd.none )

        LoadData (Err _) ->
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


toPosition : List Int -> Int -> List Position
toPosition ys x =
    ys |> List.map (\y -> Position x y)


listProduct : List Int -> List Int -> List Position
listProduct xs ys =
    xs
        |> List.map (toPosition ys)
        |> List.concat



-- VIEW


view : Model -> Html msg
view model =
    div
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
        { red, green, blue } =
            Color.toRgb pixel.color

        { x, y } =
            pixel.position
    in
        "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ") " ++ toString (pixelSize * x) ++ "px " ++ toString (pixelSize * y) ++ "px 0px " ++ toString (pixelSize) ++ "px"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
