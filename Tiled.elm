module Tiled exposing (..)

import Json.Decode exposing (int, string, float, bool, list, map, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Map =
    { height : Int
    , layers : List Layer
    , tileHeight : Int
    , tileSets : List TileSet
    , tileWidth : Int
    , width : Int
    }


type alias Layer =
    { data : List Int
    , heigt : Int
    , name : String
    , opacity : Int
    , layerType : LayerType
    , visible : Bool
    , width : Int
    , x : Int
    , y : Int
    }


type LayerType
    = TileLayer
    | ObjectGroup
    | ImageLayer


type alias TileSet =
    { columns : Int
    , image : String
    , imageHeight : Int
    , imageWidth : Int
    , margin : Int
    , name : String
    , spacing : Int
    , tileCount : Int
    , tileHeight : Int
    , tileWidth : Int
    }


mapDecoder : Decoder Map
mapDecoder =
    decode Map
        |> required "height" int
        |> required "layers" (list layerDecoder)
        |> required "tileheight" int
        |> required "tilesets" (list tileSetDecoder)
        |> required "tilewidth" int
        |> required "width" int


toTileLayer : String -> LayerType
toTileLayer str =
    case str of
        "obejctGroup" ->
            ObjectGroup

        "imagelayer" ->
            ImageLayer

        _ ->
            TileLayer


layerTypeDecoder : Decoder LayerType
layerTypeDecoder =
    map toTileLayer string


layerDecoder : Decoder Layer
layerDecoder =
    decode Layer
        |> required "data" (list int)
        |> required "height" int
        |> required "name" string
        |> required "opacity" int
        |> required "type" layerTypeDecoder
        |> required "visible" bool
        |> required "width" int
        |> required "x" int
        |> required "y" int


tileSetDecoder : Decoder TileSet
tileSetDecoder =
    decode TileSet
        |> required "columns" int
        |> required "image" string
        |> required "imageheight" int
        |> required "imagewidth" int
        |> required "margin" int
        |> required "name" string
        |> required "spacing" int
        |> required "tilecount" int
        |> required "tileheight" int
        |> required "tilewidth" int
