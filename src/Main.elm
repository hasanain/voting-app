module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h2, h3, li, text)
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
        , [ "Wally West", "Steve Rogers", "Tony Stark", "John Wick" ]
        , [ "Steve Rogers", "Wally West", "John Wick" ]
        , [ "Tony Stark", "John Wick", "Steve Rogers" ]
        , [ "John Wick" ]
        , [ "Tony Stark" ]
        , [ "John Wick", "Tony Stark" ]
        , [ "John Wick", "Tony Stark" ]
        , [ "Wally West" ]
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



-- instant runoff
-- run one round of first past the post
-- if any candidate has a majority or there is only one candidate left -> End the election with the current ranking
-- else run another round of first past the post with
--  everyone except the candidate with the least vote
--  each candidate who voted for the least person's second choice
-- TODO: what happens when you have every candidate has the same amount of votes


runInstantRunoffElection : Model -> List (List ( String, Int ))
runInstantRunoffElection ({ candidates, ballots } as election) =
    let
        roundResult =
            runFPTPElection election

        hasMajority =
            List.any (\( _, v ) -> v > List.length ballots // 2) roundResult

        singleCandidateLeft =
            List.length roundResult == 1

        noCandidatesLeft =
            List.length roundResult == 0

        justVotes =
            List.map (\( _, v ) -> v) roundResult

        leastVotes =
            List.foldl
                min
                (List.length ballots)
                justVotes

        candidatesWithLeastVotes =
            List.map (\( c, _ ) -> c) <|
                List.filter
                    (\( c, v ) -> v == leastVotes)
                    roundResult

        everyoneHasTheSameVotes =
            List.length candidatesWithLeastVotes == List.length roundResult
    in
    if hasMajority || singleCandidateLeft || noCandidatesLeft || everyoneHasTheSameVotes then
        [ roundResult ]

    else
        roundResult :: (runInstantRunoffElection <| removeCandidates election candidatesWithLeastVotes)


removeCandidates : Model -> List String -> Model
removeCandidates model candidates =
    case candidates of
        c :: cs ->
            removeCandidates (removeCandidate model c) cs

        [] ->
            model


removeCandidate : Model -> String -> Model
removeCandidate model candidate =
    let
        filterCandidates =
            List.filter (\c -> c /= candidate)

        candidates =
            filterCandidates model.candidates

        ballots =
            List.map filterCandidates model.ballots
    in
    { model | candidates = candidates, ballots = ballots }


runFPTPElection : Model -> List ( String, Int )
runFPTPElection { candidates, ballots } =
    let
        firstVotes =
            List.map
                (\b ->
                    case b of
                        v :: _ ->
                            Just v

                        [] ->
                            Nothing
                )
                ballots

        tally =
            List.map
                (\c -> ( c, List.foldl (tallyVotes c) 0 firstVotes ))
                candidates
    in
    List.reverse <| List.sortBy (\( _, v ) -> v) tally


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


instantRunoffView : Model -> List (Html Msg)
instantRunoffView model =
    let
        electionResults =
            List.reverse <| runInstantRunoffElection model

        labels =
            List.indexedMap
                (\i _ ->
                    if i == 0 then
                        h3 [] [ text "Final Round (Result)" ]

                    else
                        h3 [] [ text <| "Round " ++ String.fromInt (List.length electionResults - i) ]
                )
                electionResults

        resultsView =
            List.map (List.map candidateView) electionResults

        resultsWithLabels =
            List.map2 (\l r -> l :: r) labels resultsView
    in
    List.concatMap (\x -> x) resultsWithLabels


view model =
    div []
        [ h2 [] [ text "First Past the post" ]
        , div [] (rankedCandidatesView model)
        , h2 [] [ text "Instant Runoff" ]
        , div [] (instantRunoffView model)
        ]
