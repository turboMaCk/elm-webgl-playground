module Main exposing (main)

{-
   Rotating cube with colored sides.
-}

import AnimationFrame
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


-- Modules

import IcoSphere


main : Program Never Float Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = (\_ -> AnimationFrame.diffs Basics.identity)
        , update = (\dt theta -> ( theta + dt / 5000, Cmd.none ))
        }


view : Float -> Html Time
view theta =
    WebGL.toHtml
        [ width 400
        , height 400
        , style
            [ ( "display", "block" )
            , ( "background", "#404150" )
            ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            sphereMesh
            (uniforms theta)
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Float -> Uniforms
uniforms theta =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * theta) (vec3 0 1 0))
            (Mat4.makeRotate (2 * theta) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



-- Mesh


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


sphereMesh : Mesh Vertex
sphereMesh =
    IcoSphere.sphere 10
        -- |> List.concat
        |>
            List.map
                (\( a, b, c ) ->
                    ( Vertex a a
                    , Vertex b b
                    , Vertex c c
                    )
                )
        |> WebGL.triangles



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
