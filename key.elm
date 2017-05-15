module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowUp
    | ArrowRight
    | ArrowDown


fromCode : Int -> Maybe Key
fromCode keyCode =
    case keyCode of
        32 ->
            Just Space

        37 ->
            Just ArrowLeft

        38 ->
            Just ArrowUp

        39 ->
            Just ArrowRight

        40 ->
            Just ArrowDown

        _ ->
            Nothing
