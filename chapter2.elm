module Main (..) where

import Dict exposing (..)
import Html


{-
Agents as described in chapter 2 of Russel and Norvig's AI book.
Goal is to design different agents that efficiently clean an area
of 2 tiles (A and B). The possible actions are to suck or move to
the left or right.
-}
-- An AI agent is really just a type of mapping from Percepts to Actions.


type alias Agent =
  { agent | run : Percept -> Action }



-- Possible actions the robotic vacuum cleaner can take: suck, move left,
-- move right, or wait


type Action
  = Suck
  | MoveRight
  | MoveLeft
  | Rest



-- A perception of the current environment


type alias Percept =
  ( Location, Status )


type Location
  = A
  | B


type Status
  = Clean
  | Dirty



-- A lookup table mapping percepts to actions. Since Elm only supports Dicts
-- with built-in types for the keys, I convert the values to Strings using
-- the show function


type alias Table =
  Dict String Action



-- A rule is a function that maps a percept to True or False, accompanied by
-- an action to take if the function evaluates to True


type alias Rule =
  ( Percept -> Bool, Action )



-- An AI agent based on a lookup table. Adds the current percept to all
-- previously encountered percepts, for a simple memory system


tableDrivenAgent : Table -> Agent
tableDrivenAgent table percept =
  Dict.get (Debug.log "percept" <| toString percept) table
    |> Maybe.withDefault Rest


tableVacuumAgent : Agent
tableVacuumAgent =
  tableDrivenAgent
    <| Dict.fromList
        [ ( "[(A,Clean)]", MoveRight )
        , ( "[(A,Dirty)]", Suck )
        , ( "[(B,Clean)]", MoveLeft )
        , ( "[(B,Dirty)]", Suck )
        , ( "[(A,Clean),(A,Clean)]", MoveRight )
        , ( "[(A,Dirty),(A,Clean)]", Suck )
        , ( "[(A,Clean),(A,Clean),(A,Clean)]", MoveRight )
        , ( "[(A,Dirty),(A,Clean),(A,Clean)]", Suck )
        ]



-- A simple reflex agent with no memory. Just acts based on current status


reflexVacuumAgent : Agent
reflexVacuumAgent ( location, status ) =
  if (status == Dirty) then
    Suck
  else
    case location of
      A ->
        MoveRight

      B ->
        MoveLeft



-- A generalized simple reflex agent. This could be used to define arbitrary agents
-- that are not bound to the vacuum world problem. This could be done in a more
-- succinct way, but I'm trying to be faithful to the models described in the
-- book. As such, I defined 3 inner helper functions, but only ruleMatch is interesting


simpleReflexAgent : List Rule -> Agent
simpleReflexAgent rules percept =
  let
    interperetInput ( location, status ) =
      ( location, status )

    ruleMatch state rules =
      case ( List.head rules, List.tail rules ) of
        ( Just ( f, a ), Just rs ) ->
          if f state then
            ( (\_ -> True), a )
          else
            ruleMatch state rs

        _ ->
          ( (\_ -> False), Rest )

    ruleAction rule =
      snd rule

    state =
      interperetInput percept

    rule =
      ruleMatch state rules
  in
    ruleAction rule



-- A reimplementation of the reflex vacuum agent based on the generalized simple
-- reflex agent template


reflexVacuumAgent2 : Agent
reflexVacuumAgent2 =
  let
    rules =
      [ ( (\( location, status ) -> status == Dirty), Suck )
      , ( (\( location, status ) -> location == A), MoveRight )
      , ( (\( location, status ) -> location == B), MoveLeft )
      ]
  in
    simpleReflexAgent rules



-- A function to run a simple test based on 2 successive inputs to an agent and
-- output the results


testAgent : Agent -> Html.Html
testAgent agent =
  let
    x =
      agent ( A, Clean )

    y =
      agent ( B, Clean )
  in
    toString x |> Html.text


main : Html.Html
main =
  Html.div
    []
    [ Html.div [] [ Html.text "tableVacuumAgent: ", (testAgent tableVacuumAgent) ]
    , Html.div [] [ Html.text "reflexVacuumAgent: ", (testAgent reflexVacuumAgent) ]
    , Html.div [] [ Html.text "reflexVacuumAgent2: ", (testAgent reflexVacuumAgent2) ]
    ]
