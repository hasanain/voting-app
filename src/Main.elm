module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, h2, h3, li, ol, text)
import Html.Attributes exposing (style)
import Html.Events exposing (keyCode, on, onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { candidates : List String
    , ballots : List (List String)
    , activeBallot : List String
    }


init : Model
init =
    let
        candidates =
            [ "Tony Stark"
            , "John Wick"
            , "Steve Rogers"
            , "Wally West"
            ]
    in
    { candidates = candidates
    , ballots =
        []
    , activeBallot = candidates
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


moveItemUp : List a -> a -> List a
moveItemUp l i =
    case l of
        i1 :: i2 :: r ->
            if i2 == i then
                i2 :: i1 :: r

            else if i1 == i then
                i1 :: i2 :: r

            else
                i1 :: moveItemUp (i2 :: r) i

        [ i1 ] ->
            [ i1 ]

        [] ->
            []


moveItemDown : List a -> a -> List a
moveItemDown l i =
    case l of
        i1 :: i2 :: r ->
            if i1 == i then
                i2 :: i1 :: r

            else
                i1 :: moveItemDown (i2 :: r) i

        [ i1 ] ->
            [ i1 ]

        [] ->
            []


type Msg
    = MoveCandidateUp String
    | MoveCandidateDown String
    | RemoveCandidate String
    | ResetCandidates
    | AddVote
    | NoOp


update msg model =
    case msg of
        MoveCandidateUp c ->
            { model | activeBallot = moveItemUp model.activeBallot c }

        MoveCandidateDown c ->
            { model | activeBallot = moveItemDown model.activeBallot c }

        AddVote ->
            if List.length model.activeBallot > 0 then
                { model | activeBallot = model.candidates, ballots = model.activeBallot :: model.ballots }

            else
                model

        RemoveCandidate c ->
            { model | activeBallot = List.filter (\bc -> bc /= c) model.activeBallot }

        ResetCandidates ->
            { model | activeBallot = model.candidates }

        NoOp ->
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


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Candidates" ]
        , div [] [ div [] (List.map text <| List.intersperse ", " model.candidates) ]
        , h2 [] [ text "Ballots" ]
        , ol [] <| List.map (\b -> li [] (List.map text <| List.intersperse ", " b)) model.ballots
        , h2 [] [ text "Add vote" ]
        , addVoteView model.activeBallot
        , h2 [] [ text "First Past the post" ]
        , div [] (rankedCandidatesView model)
        , h2 [] [ text "Instant Runoff" ]
        , div [] (instantRunoffView model)
        ]


addVoteView : List String -> Html Msg
addVoteView candidates =
    let
        children =
            List.map
                (\c ->
                    div
                        (focusedBallotEntryStyle
                            ++ ballotEntryStyle
                        )
                        [ button [ onClick <| RemoveCandidate c ] [ text "➖" ]
                        , button [ onClick <| MoveCandidateUp c ] [ text "⬆" ]
                        , button [ onClick <| MoveCandidateDown c ] [ text "⬇" ]
                        , text c
                        ]
                )
                candidates
    in
    div [] <| (children ++ [ div [] [ button [ onClick AddVote ] [ text "Vote" ], button [ onClick ResetCandidates ] [ text "Reset" ] ] ])


ballotEntryStyle =
    [ style "padding" "5px", style "margin" "5px" ]


focusedBallotEntryStyle =
    [ style "border" "1px solid black" ]
