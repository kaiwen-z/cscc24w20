module Orlang where

import Control.Applicative

import OrlangDef

-------------
-- Primitives
-------------

instance MonadOrlang OM where
    get X = MkOM (\s@(MkVariables x y) -> [(s, x)])
    get Y = MkOM (\s@(MkVariables x y) -> [(s, y)])
    set X n = MkOM (\(MkVariables x y) -> [(MkVariables n y, ())])
    set Y n = MkOM (\(MkVariables x y) -> [(MkVariables x n, ())])

instance Functor OM where
    fmap f p = p >>= \a -> return (f a)

instance Applicative OM where
    pure = return
    p <*> q = p >>= \f -> q >>= \a -> return (f a)

-- data OM a = MkOM (Variables -> [(Variables, a)])

-- unOM :: OM a -> Variables -> [(Variables, a)]
-- unOM (MkOM f) = f

instance Monad OM where
    return a = MkOM (\f -> [(f, a)])
    MkOM f >>= k = MkOM (\var -> case f var of
      [] -> []
      ((a, v):_) -> 
        let inp (v, a) = (unOM (k a)) v
        in (f var) >>= inp)
-- >>= (bind) explanation:
--
-- Let s be the start state.
-- k should receive a new state and an answer from f s.
-- But f s gives you a list of many new states and answers!
-- So you will be calling k for each pair!
-- Moreoever, each call gives you a list too...
--
-- Example:
-- Suppose f maps s to [(s1, a1), (s2, a2)]
--         unOM (k a1) maps s1 to [(t1, b1), (t2, b2)]
--         unOM (k a2) maps s2 to [(t3, b3)]
-- then the final list is [(t1, b1), (t2, b2), (t3, b3)]
--
-- Hint: There is a slick way using list's >>=.  What does it do again?

instance Alternative OM where
    empty = MkOM (\f -> [])
    MkOM f <|> MkOM g = MkOM (\var -> (unOM (MkOM f) var ++ unOM (MkOM g) var))

-- <|> (alternative) Hint: Recall
--
--        (x:=x+1) or (x:=x+2)
--
-- So f is like x:=x+1, g is like x:=x+2.
-- What should happen if x=0 initially?


---------------------------------------------
-- Interpreter entry points specialized to OM
---------------------------------------------

-- Run program, x=0 and y=0 initially, get final variable values.
run0 :: Cmd -> [Variables]
run0 cmd = run cmd (MkVariables 0 0)

-- Run program, you provide initial values.
run :: Cmd -> Variables -> [Variables]
run cmd s0 = map fst (unOM (interp cmd) s0)


-----------------------------------------
-- Interpreter generalized to MonadOrlang
-----------------------------------------

evalBool :: MonadOrlang m => ExprBool -> m Bool
evalBool (Not e) = fmap not (evalBool e)
evalBool (Cmp rel e1 e2) =
    liftA2 cmp (evalNum e1) (evalNum e2)
  where
    cmp = case rel of
            Lt -> (<)
            Leq -> (<=)
            Eq -> (==)
            Neq -> (/=)
evalBool (Logic And e1 e2) =
  -- A bit more careful to short-circuit, not needed for this assignment, but
  -- more future-proof.
  evalBool e1
  >>= \b1 -> if b1 then evalBool e2 else return False
evalBool (Logic Or e1 e2) =
  -- ditto
  evalBool e1
  >>= \b1 -> if b1 then return True else evalBool e2

evalNum :: MonadOrlang m => ExprNum -> m Integer
evalNum (LitNum n) = return n
evalNum (Var X) = get X
evalNum (Var Y) = get Y
evalNum (Neg e) = fmap negate (evalNum e)
evalNum (Arith op e1 e2) = liftA2 f (evalNum e1) (evalNum e2)
  where
    f = case op of
          Add -> (+)
          Sub -> (-)
          Mul -> (*)

-- data Cmd
--   = Assign Var ExprNum          -- x := e
--   | Seq [Cmd]                   -- { cmd; cmd; ... }, sequential compound
--   | While ExprBool [Cmd]        -- while x>0 { cmd; cmd; ... }
--   | Alt Cmd Cmd                 -- cmd or cmd, nondeterminism
--   | Assert ExprBool             -- eg, assert x>0
--   deriving (Eq, Show)
interp :: MonadOrlang m => Cmd -> m ()
interp (Assign var expr) = 
  evalNum expr >>= set var

-- <|> (alternative)
interp (Alt cmd1 cmd2) = 
  interp cmd1 <|> interp cmd2

-- bool cond
interp (Assert bool) = 
  evalBool bool
  >>= \assert -> 
    case assert of
      True -> pure ()
      False -> empty

-- interp (Seq es) based on SemanticsII, Lecture 11
-- credit to Albert Lai
interp (Seq cmd) = go cmd
  where
    go [] = pure ()
    go [e] = interp e
    go (e:es) = interp e *> go es

-- First interpret commands
-- Then evaluate loop condition
-- If loop condition true -> loop another iteration via recursive call
-- If loop condition false -> exit
interp (While bool cmd) = 
  interp (Seq cmd) >>= 
    (\x1 -> 
      (evalBool bool >>= 
        (\x2 -> 
          case x2 of
            True -> interp (While bool cmd)
            False -> pure ())))

