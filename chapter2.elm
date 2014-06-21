import Automaton (..)
import Dict (..)

{-
Agents as described in chapter 2 of Russel and Norvig's AI book.
Goal is to design different agents that efficiently clean an area
of 2 tiles (A and B). The possible actions are to suck or move to
the left or right.
-}

-- An AI agent is really just a type of mapping from Percepts to Actions.
-- I will use an Automaton to represent them
type Agent = Automaton Percept Action

-- Possible actions the robotic vacuum cleaner can take: suck, move left,
-- move right, or wait
data Action = Suck | MoveRight | MoveLeft | Rest

-- A perception of the current environment
type Percept = (Location, Status)
data Location = A | B
data Status = Clean | Dirty

-- A lookup table mapping percepts to actions. Since Elm only supports Dicts
-- with built-in types for the keys, I convert the values to Strings using
-- the show function
type Table = Dict String Action

-- A rule is a function that maps a percept to True or False, accompanied by
-- an action to take if the function evaluates to True
type Rule = ((Percept -> Bool), Action)

-- An AI agent based on a lookup table. Adds the current percept to all
-- previously encountered percepts, for a simple memory system
tableDrivenAgent : Table -> Agent
tableDrivenAgent table =
    hiddenState (table, []) (\percept (table', percepts) ->
        ((table', percept :: percepts), lookup (percept :: percepts) table'))

lookup : [Percept] -> Table -> Action
lookup percepts table = maybe Rest id <| get (show percepts) table

tableVacuumAgent = tableDrivenAgent <| fromList [ ("[(A,Clean)]", MoveRight)
                                                , ("[(A,Dirty)]", Suck)
                                                , ("[(B,Clean)]", MoveLeft)
                                                , ("[(B,Dirty)]", Suck)
                                                , ("[(A,Clean),(A,Clean)]", MoveRight)
                                                , ("[(A,Dirty),(A,Clean)]", Suck)
                                                , ("[(A,Clean),(A,Clean),(A,Clean)]", MoveRight)
                                                , ("[(A,Dirty),(A,Clean),(A,Clean)]", Suck)
                                                ]

-- A simple reflex agent with no memory. Just acts based on current status
reflexVacuumAgent : Agent
reflexVacuumAgent =
    pure (\(location, status) ->
        if (status == Dirty) then Suck
        else case location of
            A -> MoveRight
            B -> MoveLeft)

-- A generalized simple reflex agent. This could be used to define arbitrary agents
-- that are not bound to the vacuum world problem. This could be done in a more
-- succinct way, but I'm trying to be faithful to the models described in the
-- book. As such, I defined 3 inner helper functions, but only ruleMatch is interesting
simpleReflexAgent : [Rule] -> Agent
simpleReflexAgent rules =
    pure (\percept ->
        let interperetInput (l, s) = (l, s)
            ruleMatch state rules' = if (length rules' == 0) then ((\_ -> False), Rest)
                                     else let (r::rs) = rules'
                                              (f, a)  = r in
                                          if   (f state)    then ((\_ -> True), a)
                                          else ruleMatch state rs
            ruleAction rule        = snd rule
            state                  = interperetInput percept
            rule                   = ruleMatch state rules in
        ruleAction rule)

-- A reimplementation of the reflex vacuum agent based on the generalized simple
-- reflex agent template
reflexVacuumAgent2 : Agent
reflexVacuumAgent2 =
    let rules = [ ((\(location, status) -> status == Dirty), Suck)
                , ((\(location, status) -> location == A), MoveRight)
                , ((\(location, status) -> location == B), MoveLeft)
                ] in
    simpleReflexAgent rules

-- A function to run a simple test based on 2 successive inputs to an agent and
-- output the results
testAgent : Agent -> Element
testAgent agent = let x = (step (A, Clean) agent) in
                  asText <| (snd x, snd <| step (A, Dirty) (fst x))

main = flow down [ plainText "tableVacuumAgent: " `beside` (testAgent tableVacuumAgent)
                 , plainText "reflexVacuumAgent: " `beside` (testAgent reflexVacuumAgent)
                 , plainText "reflexVacuumAgent2: " `beside` (testAgent reflexVacuumAgent2)
                 ]
