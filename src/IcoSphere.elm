module IcoSphere exposing (sphere)

import Math.Vector3 as Vec3 exposing (vec3, Vec3)


type alias Face =
    ( Vec3, Vec3, Vec3 )


sphere : Int -> List Face
sphere subdivisions =
    icotris1 |> subdivide subdivisions


icotris1 : List Face
icotris1 =
    let
        t =
            (1.0 + sqrt (5.0)) / 2.0

        ---------------------
        v0 =
            vec3 (-1) t 0

        v1 =
            vec3 1 t 0

        v2 =
            vec3 (-1) (-t) 0

        v3 =
            vec3 1 (-t) 0

        ---------------------
        v4 =
            vec3 0 (-1) t

        v5 =
            vec3 0 1 t

        v6 =
            vec3 0 -1 (-t)

        v7 =
            vec3 0 1 (-t)

        ---------------------
        v8 =
            vec3 t 0 (-1)

        v9 =
            vec3 t 0 1

        v10 =
            vec3 (-t) 0 (-1)

        v11 =
            vec3 (-t) 0 1
    in
        [ ( v0, v11, v5 )
        , ( v0, v5, v1 )
        , ( v0, v1, v7 )
        , ( v0, v7, v10 )
        , ( v0, v10, v11 )
          -------------------
        , ( v1, v5, v9 )
        , ( v5, v11, v4 )
        , ( v11, v10, v2 )
        , ( v10, v7, v6 )
        , ( v7, v1, v8 )
          -------------------
        , ( v3, v9, v4 )
        , ( v3, v4, v2 )
        , ( v3, v2, v6 )
        , ( v3, v6, v8 )
        , ( v3, v8, v9 )
          -------------------
        , ( v4, v9, v5 )
        , ( v2, v4, v11 )
        , ( v6, v2, v10 )
        , ( v8, v6, v7 )
        , ( v9, v8, v1 )
        ]


midPoint : Vec3 -> Vec3 -> Vec3
midPoint a b =
    let
        ( x1, y1, z1 ) =
            Vec3.toTuple a

        ( x2, y2, z2 ) =
            Vec3.toTuple b
    in
        vec3 ((x1 - x2) / 2) ((y1 - y2) / 2) ((z1 - z2) / 2)


subdivide : Int -> List Face -> List Face
subdivide times faces =
    let
        subdivideFaces ( v1, v2, v3 ) =
            let
                a =
                    midPoint v1 v2

                b =
                    midPoint v2 v3

                c =
                    midPoint v3 v1
            in
                [ ( v1, a, c )
                , ( v2, b, a )
                , ( v3, c, b )
                , ( a, b, c )
                ]
    in
        if times <= 0 then
            faces
        else
            subdivide (times - 2) (List.concatMap subdivideFaces faces)
