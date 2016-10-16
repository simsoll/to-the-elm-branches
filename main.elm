module Game exposing (..)

import Html exposing (Html, text, div, Attribute)
import Html.Attributes exposing (style)
import Html.App as Html
import Keyboard exposing (KeyCode)
import Random
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)


main : Program Never
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
    , numberOfPixels : Int
    , randomNumber : Int
    , framesPerSecond : Int
    , timeElapsedInSeconds : Float
    }


type alias Player =
    { velocity : Float
    , position : Float
    , shotsFired : Int
    }


pixelSize : number
pixelSize =
    8


viewWidth : number
viewWidth =
    1024


viewHeight : number
viewHeight =
    768


model : Model
model =
    { viewWidth = viewWidth
    , viewHeight = viewHeight
    , pixelSize = pixelSize
    , numberOfPixels = round (viewWidth / pixelSize * viewHeight / pixelSize)
    , player =
        { velocity = 0
        , position = 0
        , shotsFired = 0
        }
    , randomNumber = 0
    , framesPerSecond = 0
    , timeElapsedInSeconds = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate time ->
            ( model |> applyPhysics time, Cmd.none )

        KeyDown keyCode ->
            ( { model | player = keyDown keyCode model.player }, Cmd.none )

        KeyUp keyCode ->
            ( { model | player = keyUp keyCode model.player }, Cmd.none )


keyDown : KeyCode -> Player -> Player
keyDown keyCode player =
    case Key.fromCode keyCode of
        Space ->
            incrementShotsFired player

        ArrowLeft ->
            updateVelocity -1.0 player

        ArrowRight ->
            updateVelocity 1.0 player

        _ ->
            player


keyUp : KeyCode -> Player -> Player
keyUp keyCode player =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 player

        ArrowRight ->
            updateVelocity 0 player

        _ ->
            player


applyPhysics : Time -> Model -> Model
applyPhysics deltaTime model =
    { model
        | player = applyTime deltaTime model.player
        , framesPerSecond = round (60 / Time.inSeconds deltaTime)
        , timeElapsedInSeconds = model.timeElapsedInSeconds + Time.inSeconds deltaTime
        , randomNumber =
            case round model.timeElapsedInSeconds % 1 of
                0 ->
                    round model.timeElapsedInSeconds
                        |> Random.initialSeed
                        |> Random.step (Random.int 1 100)
                        |> fst

                _ ->
                    model.randomNumber
    }


applyTime : Time -> Player -> Player
applyTime deltaTime player =
    { player
        | position = player.position + player.velocity * Time.inMilliseconds deltaTime
    }


updateVelocity : Time -> Player -> Player
updateVelocity newVelocity player =
    { player | velocity = newVelocity }


incrementShotsFired : Player -> Player
incrementShotsFired player =
    { player | shotsFired = player.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ canvas model, text (toString model) ]


canvas : Model -> Html msg
canvas model =
    let
        blocks =
            [1..model.numberOfPixels]
                |> List.map (\n -> model)
                |> List.map block
    in
        div [ canvasStyle model ] blocks


canvasStyle : Model -> Attribute msg
canvasStyle model =
    style
        [ ( "backgroundColor", "cornflowerblue" )
        , ( "height", toString model.viewHeight ++ "px" )
        , ( "width", toString model.viewWidth ++ "px" )
        , ( "margin", "auto" )
        ]


block : Model -> Html msg
block model =
    div [ blockStyle model ] []


blockStyle : Model -> Attribute msg
blockStyle model =
    style
        [ ( "backgroundColor", "yellow" )
        , ( "float", "left" )
        , ( "height", toString model.pixelSize ++ "px" )
        , ( "width", toString model.pixelSize ++ "px" )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
