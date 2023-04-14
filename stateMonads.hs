{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
import Control.Monad.RWS (MonadState(state), when)
import Control.Applicative

-- state monads are used to create immutability

newtype State s a = State (s-> (a,s))

runState :: State s a-> s -> (a,s)
runState (State f) state = f state


addOneToState :: State Int () 
addOneToState = State f
 where 
     f state = ((),state+1)

addOneToStateTwice :: State Int ()
addOneToStateTwice = State f 
 where 
    f state = let (_, newstate) = runState addOneToState state
            in runState addOneToState newstate

--- now we are creating and instance for State s 

instance Functor (State s) where
    -- fmap :: (a->b) -> State s a -> State s b
    fmap fab stateF = State f
     where 
        f state = let (a, newstate) = runState stateF state
                  in (fab a, newstate)

statePlusFive :: State Int Int
statePlusFive = State (\s -> (s+5,s+5))

statePlusFiveEven :: State Int Bool
statePlusFiveEven = fmap even statePlusFive

statePlusFiveTwice :: State Int Int
statePlusFiveTwice = fmap (+) statePlusFive <*> statePlusFive

instance Applicative (State s) where
    pure x = State (\oldstate -> (x,oldstate))
   -- <*> ::State s(a-> b) State s a -> State s b
    stateAB <*> stateA = State f
     where 
        f oldstate = let (f,newstate1)= runState stateAB oldstate
                         (a,newstate2) = runState stateA newstate1
                    in ( f a, newstate2)

stateApExample1 :: State s Int
stateApExample1 = fmap (+) (pure 5) <*> (pure 7)

--                = fmap (+) ((State(\oldstate -> (5,oldstate))) <*> (pure 7)
--                = State (\oldstate -> ((5+),oldstate)) <*> State (\oldstate -> (7,oldstate))
--                = let (f,newstate1)= runState State (\oldstate -> ((5+),oldstate)) oldstate
--                         (a,newstate2) = runState State (\oldstate -> (7,oldstate)) newstate1
--                    in ( f a, newstate2)
--                  = let (f, newstate1) = (5+,oldstate)
--                        (a, newstate2) = (7,newstate1)
--                        in (f a, newstate2)

--                  = let (f,newstate1) =( 5+,0)
 --                       (a,newstate2) =(7,newstate1)
--                        in ( f a newstate2)
--                  = (12,0)

stateApExample2 :: State Int Int
stateApExample2 = liftA2 (*) statePlusFive statePlusFive

instance Monad (State s) where
  stateA >>= next = State f
   where 
    f state = let (x,newstate) = runState stateA state
                  stateB = next x
                in runState stateB newstate 


stateMonadExample :: State Int Int
stateMonadExample = do
    num1 <- statePlusFive
    num2 <- statePlusFive
    return (num1* num2)

-- get will return the current state as output
get :: State s s 
get = State (\s -> (s,s))

gets :: (s->a) -> State s a 
gets f = State (\s -> (f s ,s))

-- put replaces the current value of the state
put :: s -> State s ()
put s = State(\_ -> ((),s)) 

modify :: (s->s) -> State s ()
modify f = State (\s -> ((), f s))

type MyGlobalState = (Int,Int)

imperativeExample :: State MyGlobalState ()
imperativeExample = do
    (counter,x) <- get 
    if (x<5) 
         then do put (counter*(x+1), (x+1)) 
                 imperativeExample
          else return ()  

imperativeExampleBetter :: State MyGlobalState ()
imperativeExampleBetter = do
    x <- gets snd
    if (x<5) then do modify (\(counter,x) -> (counter, x+1))
                     modify (\(counter,x)-> (counter*x,x))
                     imperativeExampleBetter
                else do return ()
