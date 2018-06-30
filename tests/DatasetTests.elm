module DatasetTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Dataset exposing (..)


-- findOne
-- findMany


type alias Person =
    { id : Int, name : String }


config : Config Person Int
config =
    { getId = (\item -> item.id)
    }


getIds =
    List.map .id


putOneTests : Test
putOneTests =
    describe "putOne"
        [ test "it adds" <|
            \_ ->
                let
                    result =
                        putOne
                            config
                            (Person 1 "Sam")
                            []
                            |> getIds
                in
                    Expect.equal result [ 1 ]
        , test "it replaces an existing one" <|
            \_ ->
                let
                    result =
                        putOne
                            config
                            (Person 1 "Sam")
                            [ Person 1 "Sally" ]
                in
                    Expect.equal result [ Person 1 "Sam" ]
        , test "it adds new at the end" <|
            \_ ->
                let
                    result =
                        putOne
                            config
                            (Person 1 "Sam")
                            [ Person 2 "Sally" ]
                in
                    Expect.equal result [ Person 2 "Sally", Person 1 "Sam" ]
        ]


putManyTests =
    describe "putMany"
        [ test "it updates existing ones" <|
            \_ ->
                let
                    result =
                        putMany
                            config
                            [ Person 1 "Zam", Person 2 "Zally" ]
                            [ Person 1 "Sam", Person 2 "Sally", Person 3 "Tess" ]
                in
                    Expect.equal
                        result
                        [ Person 1 "Zam", Person 2 "Zally", Person 3 "Tess" ]
        , test "it puts new ones at the end" <|
            \_ ->
                let
                    result =
                        putMany
                            config
                            [ Person 1 "Sam", Person 2 "Sally" ]
                            [ Person 2 "S", Person 3 "Kim" ]
                in
                    Expect.equal
                        result
                        [ Person 2 "Sally", Person 3 "Kim", Person 1 "Sam" ]
        ]


deleteManyTests =
    describe "deleteMany"
        [ test "it deletes" <|
            \_ ->
                let
                    result =
                        deleteMany
                            config
                            [ 1 ]
                            [ Person 1 "Sam", Person 2 "Sally" ]
                in
                    Expect.equal
                        result
                        [ Person 2 "Sally" ]
        ]


findOneTests =
    describe "findOne" [
        test "it finds" <|
            \_ ->
                let
                    result =
                        findOne
                            config
                            1
                            [ Person 1 "Sam", Person 2 "Sally" ]
                in
                    Expect.equal
                        result
                        (Just <| Person 1 "Sam")
    ]
