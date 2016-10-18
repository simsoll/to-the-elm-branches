module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight


fromCode : Int -> Maybe Key
fromCode keyCode =
    case keyCode of
        32 ->
            Just Space

        37 ->
            Just ArrowLeft

        39 ->
            Just ArrowRight

        _ ->
            Nothing
