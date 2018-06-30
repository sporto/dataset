module DatasetTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dataset exposing (..)


-- putOne
-- putMany
-- deleteOne
-- deleteMany
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

putOneTests: Test
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
