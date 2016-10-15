module Game exposing (..)

import Html exposing (Html, text, div)
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
    , velocity : Float
    , position : Float
    , shotsFired : Int
    , randomNumber : Int
    , framesPerSecond : Int
    , timeElapsedInSeconds : Float
    }


model : Model
model =
    { viewWidth = 800
    , viewHeight = 600
    , pixelSize = 5
    , velocity = 0
    , position = 0
    , shotsFired = 0
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
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            incrementShotsFired model

        ArrowLeft ->
            updateVelocity -1.0 model

        ArrowRight ->
            updateVelocity 1.0 model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model


applyPhysics : Time -> Model -> Model
applyPhysics deltaTime model =
    { model
        | position = model.position + model.velocity * Time.inMilliseconds deltaTime
        , framesPerSecond = round (60 / Time.inSeconds deltaTime)
        , timeElapsedInSeconds = model.timeElapsedInSeconds + Time.inSeconds deltaTime
        , randomNumber =
            case round model.timeElapsedInSeconds % 2 of
                0 ->
                    round model.timeElapsedInSeconds
                        |> Random.initialSeed
                        |> Random.step (Random.int 1 100)
                        |> fst

                _ ->
                    model.randomNumber
    }


updateVelocity : Time -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ text (toString model) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
