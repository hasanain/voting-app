module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, li, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { candidates : List String
    , ballots : List (List String)
    }


init : Model
init =
    { candidates =
        [ "Tony Stark"
        , "John Wick"
        , "Steve Rogers"
        , "Wally West"
        ]
    , ballots =
        [ [ "Tony Stark", "John Wick" ]
        , [ "Wally West", "Steve Rogers", "John Wick", "Tony Stark" ]
        , [ "Steve Rogers", "Wally West", "Tony Stark" ]
        , [ "Tony Stark", "John Wick", "Steve Rogers" ]
        , []
        , [ "Tony Stark" ]
        ]
    }


tallyVotes : String -> Maybe String -> Int -> Int
tallyVotes c v p =
    case v of
        Just vote ->
            if vote == c then
                p + 1

            else
                p

        Nothing ->
            p


runFPTPElection : Model -> List ( String, Int )
runFPTPElection { candidates, ballots } =
    let
        initialCount =
            List.map (\n -> ( n, 0 )) candidates

        firstVotes =
            List.map
                (\r ->
                    case r of
                        v :: _ ->
                            Just v

                        _ ->
                            Nothing
                )
                ballots

        tally =
            List.map
                (\c -> ( c, List.foldl (tallyVotes c) 0 firstVotes ))
                candidates
    in
    List.reverse <| List.sortBy (\( k, v ) -> v) tally


type Msg
    = NoOp


update msg model =
    model


candidateView : ( String, Int ) -> Html Msg
candidateView ( k, v ) =
    let
        element =
            li [] [ text <| k ++ ", " ++ String.fromInt v ]
    in
    element


rankedCandidatesView : Model -> List (Html Msg)
rankedCandidatesView model =
    let
        electionResult =
            runFPTPElection model
    in
    List.map candidateView electionResult


view model =
    div [] (rankedCandidatesView model)
