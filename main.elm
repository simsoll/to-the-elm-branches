module Game exposing (..)

import Html exposing (Html, text, div, Attribute)
import Html.Attributes exposing (style)
import Html.App as Html
import Keyboard exposing (KeyCode)
import Random
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)
import Color exposing (..)
import Dict exposing (..)


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
    , actions : List Action
    , numberOfPixels : Int
    , randomNumber : Int
    , framesPerSecond : Int
    , timeElapsedInSeconds : Float
    }


type alias Position =
    ( Int, Int )


type alias Pixel =
    { position : Position
    , color : Color
    }


type alias PixelMap =
    List Pixel


type alias Player =
    { velocity : Float
    , position : Position
    , shotsFired : Int
    , pixelMap : PixelMap
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
        { velocity = 1
        , position = ( 0, 0 )
        , shotsFired = 0
        , pixelMap =
            [ { position = ( 0, 0 )
              , color = rgb 150 150 150
              }
            , { position = ( 1, 0 )
              , color = rgb 150 150 150
              }
            , { position = ( 0, 1 )
              , color = rgb 150 150 150
              }
            , { position = ( 1, 1 )
              , color = rgb 150 150 150
              }
            ]
        }
    , actions = []
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


type alias Velocity =
    Float


type Action
    = Shot
    | MoveLeft Velocity
    | MoveRight Velocity


toAction : Model -> Int -> Maybe Action
toAction model keyCode =
    case Key.fromCode keyCode of
        Just key ->
            case key of
                Space ->
                    Just Shot

                ArrowLeft ->
                    Just (MoveLeft model.player.velocity)

                ArrowRight ->
                    Just (MoveRight model.player.velocity)

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- TODO: fix this! apply actions in applyPhysics
        TimeUpdate time ->
            ( model |> applyTime time, Cmd.none )

        KeyDown keyCode ->
            let
                action =
                    keyCode |> toAction model
            in
                case action of
                    Just action ->
                        case List.member action model.actions of
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
                        |> fst

                _ ->
                    model.randomNumber
    }


applyAction : Time -> Action -> Player -> Player
applyAction deltaTime action player =
    case action of
        Shot ->
            { player | shotsFired = player.shotsFired + 1 }

        MoveLeft velocity ->
            { player | position = ( fst player.position - round (player.velocity * Time.inMilliseconds deltaTime), snd player.position ) }

        MoveRight velocity ->
            { player | position = ( fst player.position + round (player.velocity * Time.inMilliseconds deltaTime), snd player.position ) }



-- VIEW


view : Model -> Html msg
view model =
    div [] [ canvas model, text (toString model) ]


type alias Canvas =
    Dict Position Color


background : Canvas
background =
    let
        xs =
            [0..round (viewWidth / pixelSize) - 1]

        ys =
            [0..round (viewHeight / pixelSize) - 1]
    in
        ys
            |> List.map (singleTupleZip xs)
            |> List.concat
            |> singleTupleZipReserve (rgb 150 150 150)
            |> Dict.fromList


singleTupleZip : List a -> b -> List ( a, b )
singleTupleZip a b =
    a |> List.map (\x -> ( x, b ))


singleTupleZipReserve : a -> List b -> List ( b, a )
singleTupleZipReserve a b =
    b |> List.map (\x -> ( x, a ))


canvas : Model -> Html msg
canvas model =
    let
        blocks =
            background
                |> Dict.toList
                |> List.map (canvasPixel model)
    in
        div [ canvasStyle model ] blocks


canvasPixel : Model -> ( Position, Color ) -> Html msg
canvasPixel model ( position, color ) =
    div [ canvasPixelStyle model position color ] []


canvasPixelStyle : Model -> Position -> Color -> Attribute msg
canvasPixelStyle model position color =
    let
        { red, green, blue } =
            Color.toRgb color

        ( xPos, yPos ) =
            position
    in
        style
            [ ( "backgroundColor", "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")" )
            , ( "position", "absolute" )
            , ( "height", toString model.pixelSize ++ "px" )
            , ( "width", toString model.pixelSize ++ "px" )
            , ( "left", toString (model.pixelSize * xPos) ++ "px" )
            , ( "bottom", toString (model.pixelSize * yPos) ++ "px" )
            ]


canvasStyle : Model -> Attribute msg
canvasStyle model =
    style
        [ ( "backgroundColor", "cornflowerblue" )
        , ( "position", "relative" )
        , ( "height", toString model.viewHeight ++ "px" )
        , ( "width", toString model.viewWidth ++ "px" )
        , ( "margin", "auto" )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
