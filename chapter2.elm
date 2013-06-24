import open Automaton
import open Dict

{-
Agents as described in chapter 2 of Russel and Norvig's AI book.
Goal is to design different agents that efficiently clean an area
of 2 tiles (A and B). The possible actions are to suck or move to 
the left or right.
-}

-- Possible actions the robotic vacuum cleaner can take: suck, move left, 
-- move right, or wait
data Action = Suck | MoveRight | MoveLeft | Rest
            
-- A perception of the current environment            
type Percept = (Location, State)
data Location = A | B
data State = Clean | Dirty

-- A lookup table mapping percepts to actions. Since Elm only supports Dicts
-- with built-in types for the keys, I convert the values to Strings using
-- the show function
type Table = Dict String Action

-- An AI agent based on a lookup table. Adds the current percept to all
-- previously encountered percepts, for a simple memory system
tableDrivenAgent : Table -> Automaton Percept Action
tableDrivenAgent table = 
    hiddenState (table, []) (\percept (table', percepts) -> 
        ((table', percept :: percepts), lookup' (percept :: percepts) table'))
    
lookup' : [Percept] -> Table -> Action
lookup' percepts table = maybe Rest id <| lookup (show percepts) table

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
reflexVacuumAgent : Automaton Percept Action
reflexVacuumAgent =
    pure (\(location, state) ->
        if (state == Dirty) then Suck
        else case location of
            A -> MoveRight
            B -> MoveLeft)


-- A function to run a simple test based on 2 successive inputs to an agent and
-- output the results
testAgent : Automaton Percept Action -> Element
testAgent agent = let x = (step (A, Clean) agent) in
                  asText <| (snd x, snd <| step (A, Dirty) (fst x))
                                           
main = (testAgent tableVacuumAgent) `above` (testAgent reflexVacuumAgent)