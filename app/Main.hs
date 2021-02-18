module Main where
import Control.Applicative
import System.Environment

data F s x = F { run_f :: (x, s) }

instance Functor (F s) where
  fmap f (F (x, s)) = F (f x, s)

data G s a = G { run_g :: (s -> a) }

instance Functor (G s) where
  fmap f (G k) = G (f . k)

data T s x = T { run_t :: G s (F s x) }

instance Functor (T s) where
  fmap f (T k) = T $ fmap (fmap f) k

phi :: ((F s x) -> a) -> (x -> (G s a))
phi f = \x -> G $ \s -> f (F (x, s))

psi :: (x -> (G s a)) -> ((F s x) -> a)
psi g = \(F (x, s)) -> run_g (g x) s

eta :: x -> (G s (F s x))
eta = phi id

epsilon :: (F s (G s a)) -> a
epsilon = psi id

mu :: (T s (T s x)) -> T s x
mu ttx = T $ fmap epsilon $ fmap (fmap run_t) (run_t ttx)

instance Monad (T s) where
  return = fmap T $ eta
  m >>= f = mu $ (fmap f) m

instance Applicative (T s) where
  pure = return
  g <*> m = g >>= flip fmap m

construct_t :: (s -> (x, s)) -> T s x
construct_t f = T $ fmap F (G f)

get_t :: T s s
get_t = construct_t $ \s -> (s, s)

put_t :: s -> T s ()
put_t s = construct_t $ \_ -> ((), s)

gate x = do
  state <- get_t
  put_t (not state)
  return $ if state then (x + 1) else x

five_gates x = return x >>= gate >>= gate >>= gate >>= gate >>= gate

run_state t_monad s = run_f $ run_g (run_t t_monad) s

main = do
  input <- fmap (read . head) getArgs
  putStrLn $ show . fst $ run_state (five_gates (input :: Int)) False