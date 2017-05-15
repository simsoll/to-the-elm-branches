module Game exposing (..)

import Html exposing (Html, text, div, Attribute)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import Random
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)
import Color exposing (..)


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
    , numberOfPixels : Int
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


pixelSize : number
pixelSize =
    16


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
    , numberOfPixels = round (viewWidth / pixelSize * viewHeight / pixelSize)
    , player =
        { velocity = 0.05
        , position = Position 0 0
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
    div []
        [ background
        , (drawPlayer model.player)
          -- , text (toString model)
          -- , div [] [ text (toString (listProduct x y)) ]
        ]


background : Html msg
background =
    let
        xs =
            List.range 0 (round (viewWidth / pixelSize) - 1)

        ys =
            List.range 0 (round (viewHeight / pixelSize) - 1)

        color =
            rgb 0 0 0
    in
        ys
            |> listProduct xs
            |> List.map (Pixel color)
            |> List.map canvasPixel
            |> div []


translatePixel : Position -> Pixel -> Pixel
translatePixel pos pixel =
    { pixel | position = add pos pixel.position }


translateSprite : Position -> Sprite -> Sprite
translateSprite pos sprite =
    sprite |> List.map (translatePixel pos)


drawPlayer : Player -> Html msg
drawPlayer player =
    translateSprite player.position player.sprite
        |> drawSprite


drawSprite : Sprite -> Html msg
drawSprite sprite =
    sprite
        |> List.map canvasPixel
        |> div []


canvasPixel : Pixel -> Html msg
canvasPixel pixel =
    div [ canvasPixelStyle pixel ] []


canvasPixelStyle : Pixel -> Attribute msg
canvasPixelStyle pixel =
    let
        { red, green, blue } =
            Color.toRgb pixel.color

        { x, y } =
            pixel.position
    in
        style
            [ ( "backgroundColor", "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")" )
            , ( "position", "absolute" )
            , ( "height", toString pixelSize ++ "px" )
            , ( "width", toString pixelSize ++ "px" )
            , ( "transform", "translate(" ++ toString (pixelSize * x) ++ "px," ++ toString (pixelSize * y) ++ "px)" )
              -- , ( "left", toString (pixelSize * x) ++ "px" )
              -- , ( "bottom", toString (pixelSize * y) ++ "px" )
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
