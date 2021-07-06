module Pies exposing (main)

import Array exposing (Array)
import Color exposing (Color)
import Path
import Shape exposing (Arc, defaultPieConfig)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


w : Float
w = 100

h : Float
h = 100


cornerRadius : Float
cornerRadius = 12


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    Color.fromRgba { red = toFloat r / 255, green = toFloat g / 255, blue = toFloat b / 255, alpha = a }


colors : Array Color
colors =
    Array.fromList
        [ rgba255 214 39 40 0.5,  -- blue 31 119 180 
        rgba255 31 180 51 0.5 ]  -- red 214 39 40
radius : Float
radius =
    min (w / 2) h / 2 * 0.5
annular : List Arc -> Svg msg
annular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius*0.7 })
                [ fill <| Paint <| Maybe.withDefault Color.white <| Array.get index colors,
                stroke <| Paint Color.white ]
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]

 
view : List Float -> Svg msg
view model =
    let
        pieData =
            model |> Shape.pie { defaultPieConfig | outerRadius = radius, padAngle = 0.03 }
    in
    svg [ viewBox 0 0 w h ]
        [ annular pieData ]


data : List Float
data =
    [ 1, 2 ]

main : Svg msg
main = view data