-- Regular Expression to Deterministic Finite Automaton


-- Regular expressions needs and alphabet, and has a set of Operations
-- a . b <- concatenation
-- a + b <- pick one of a or b
-- a*   <-  iteration
-- e <- epsilon, empty word
-- () <- grouping

data Regex = Lit Char
            | It Regex
            | Con Regex Regex
            | Or Regex Regex
            | Epsilon
            | Grp Regex

showRegex :: Regex -> String
showRegex (Lit x) = [x]
showRegex Epsilon = "e"
showRegex (It x) = "(" ++ showRegex x ++ ")*"
showRegex (Con x y) = "(" ++ showRegex x ++ " . " ++ showRegex y ++ ")"
showRegex (Or x y) = "(" ++ showRegex x ++ " + " ++ showRegex y ++ ")"
showRegex (Grp regex) = "(" ++ showRegex regex ++ ")"

instance Show Regex where
    show regex = showRegex regex

-- Nondeterministic generalized finite automaton
-- Is a tuple of five, 
-- (Alphabet, States, Input states, Output states, Transition functions) where
-- transition functions is a tuple of 3, (current state, input, output state)
type State = Int
type Input = Char
type Transition = (State, Input, State)
type NGFA = ([Char], [State], [State], [State], [Transition])


-- There will be needed some helper functions, eg. concatenate automatons

-- Merge states
-- From first `x` get rid of output states, keep input states
-- From second `y` get rid of input states, keep output states
-- Add epsilon transition from first `x` output state to second `y` input state 

concatAutomaton:: NGFA -> NGFA -> NGFA
concatAutomaton (a1, s1, i1, o1, t1) (a2, s2, i2, o2, t2) = (
    a1 ++ a2,
    s1 ++ [x + (maximum s1 + 1) | x <- s2],
    i1,
    [x + (maximum s1 + 1) | x <- o2],
    t1 ++ fromOutToIn o1 [x + (maximum s1 + 1) | x <- i2] ++ replaceTransitions t2 (maximum s1 + 1)
    )

-- Add input state, and epsilon transitions to `x` and `y`
orBetweenAutomatons :: NGFA -> NGFA -> NGFA
orBetweenAutomatons (a1, s1, i1, o1, t1) (a2, s2, i2, o2, t2) = (
    a1 ++ a2,
    [0] ++ [x + 1 | x <- s1] ++ [x + (maximum [x + 1 | x <- s1] + 1) | x <- s2],
    [0],
    (map (+1) o1) ++ [x + (maximum [x + 1 | x <- s1] + 1) | x <- o2],
    fromOutToIn [0] (map (+1) i1) ++ fromOutToIn [0] [x + 2 + (maximum s1) | x <- i2] ++ replaceTransitions t1 1 ++ replaceTransitions t2 (maximum [x + 1 | x <- s1] + 1)
    )


-- Add input state, make it output state and epsilon transition to input state and from output state to 
-- the added state
iterateAutomaton :: NGFA -> NGFA
iterateAutomaton (a1, s1, i1, o1, t1) = (
    a1,
    0: (map (+1) s1),
    [0],
    [0] ++ (map (+1) o1),
    fromOutToIn [0] (map (+1) i1) ++ fromOutToIn (map (+1) o1) [0] ++ replaceTransitions t1 1
    )


-- if regex is epsilon, then NGFA is 1 state, that is input and output output, no need for transition functions or whatever
-- if regex is literal, we need 2 states, 1 transition, from start to finish
regexToNgfa :: Regex -> NGFA
regexToNgfa Epsilon = (['e'], [0], [0], [0], [])
regexToNgfa (Lit a) = ([a], [0, 1], [0], [1], [(0, a, 1)])
regexToNgfa (It x) = iterateAutomaton (regexToNgfa x)
regexToNgfa (Con x y) = concatAutomaton (regexToNgfa x) (regexToNgfa y)
regexToNgfa (Or x y) = orBetweenAutomatons (regexToNgfa x) (regexToNgfa y)
regexToNgfa (Grp x) = regexToNgfa x

sample1 = regexToNgfa (Lit 'a')
sample2 = regexToNgfa (Lit 'b')


fromOutToIn :: [State] -> [State] -> [Transition]
fromOutToIn out inp = [(x, 'e', y) | x <- out, y <- inp]

replaceTransitions :: [Transition] -> Int -> [Transition]
replaceTransitions t toAdd = [(x + toAdd, y, c + toAdd) | (x, y, c) <- t]

replace :: [Int] -> Int -> Int -> [Int]
replace [] _ _ = []
replace (x:xs) a b = if x == a then b : replace xs a b else x : replace xs a b