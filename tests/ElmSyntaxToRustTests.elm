module ElmSyntaxToRustTests exposing (suite)

{-| This module doesn't test a whole lot.
I mostly use it to check the project still compiles.

If you want to test certain things, it might also be nice
to use this module to quickly inspect the transpiled code

-}

import Elm.Parser
import ElmSyntaxToRust
import Expect
import FastDict
import Test exposing (Test)


suite : Test
suite =
    Test.describe "elm-syntax-to-rust"
        [ Test.test ":: with multiple initial elements and final tail variable"
            (\() ->
                """module A exposing (..)
a0 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (d) ->
            b

        _ ->
            Maybe.Nothing

a1 =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (_) ->
            b

        _ ->
            Nothing

a2 x =
    case x of
        (({y,z}::tail), Maybe.Nothing as nothing, (Just[ "" ],0)) ->
            0
        _ ->
            1
"""
                    |> elmModuleSourceTranspileToRust
                    |> Expect.ok
            )
        ]


elmModuleSourceTranspileToRust : String -> Result String String
elmModuleSourceTranspileToRust source =
    case
        [ source ]
            |> List.foldl
                (\moduleSource soFarOrError ->
                    case moduleSource |> Elm.Parser.parseToFile of
                        Err deadEnds ->
                            Err
                                (("failed to parse actual source: "
                                    ++ (deadEnds |> Debug.toString)
                                 )
                                    :: (case soFarOrError of
                                            Err errors ->
                                                errors

                                            Ok _ ->
                                                []
                                       )
                                )

                        Ok parsed ->
                            case soFarOrError of
                                Err error ->
                                    Err error

                                Ok soFar ->
                                    Ok (parsed :: soFar)
                )
                (Ok [])
    of
        Err deadEnds ->
            Err
                ("failed to parse actual source: "
                    ++ (deadEnds |> Debug.toString)
                )

        Ok parsedModules ->
            let
                transpiledResult :
                    { errors : List String
                    , declarations :
                        { fns :
                            FastDict.Dict
                                String
                                { parameters :
                                    List
                                        { pattern : ElmSyntaxToRust.RustPattern
                                        , type_ : ElmSyntaxToRust.RustType
                                        }
                                , result : ElmSyntaxToRust.RustExpression
                                , resultType : ElmSyntaxToRust.RustType
                                , lifetimeParameters : List String
                                }
                        , lets :
                            FastDict.Dict
                                String
                                { result : ElmSyntaxToRust.RustExpression
                                , resultType : ElmSyntaxToRust.RustType
                                }
                        , typeAliases :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , type_ : ElmSyntaxToRust.RustType
                                }
                        , enumTypes :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , variants :
                                    FastDict.Dict
                                        String
                                        (List ElmSyntaxToRust.RustType)
                                }
                        , structs :
                            FastDict.Dict
                                String
                                { parameters : List String
                                , fields : FastDict.Dict String ElmSyntaxToRust.RustType
                                }
                        }
                    }
                transpiledResult =
                    parsedModules |> ElmSyntaxToRust.modules
            in
            case transpiledResult.errors of
                transpilationError0 :: transpilationError1Up ->
                    Err
                        ("failed to transpile the parsed elm to rust: "
                            ++ ((transpilationError0 :: transpilationError1Up)
                                    |> String.join " and "
                               )
                        )

                [] ->
                    Ok
                        (transpiledResult.declarations
                            |> ElmSyntaxToRust.rustDeclarationsToModuleString
                        )
