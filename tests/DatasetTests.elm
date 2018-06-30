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


putOneTests =
    describe "putOne"
        [ test "it adds" <|
            \_ ->
                let
                    result =
                        putOne config { id = 1, name = "Sam" } []
                            |> getIds
                in
                    Expect.equal result [ 1 ]
        ]
