module GetOpt.Options where

import GetOpt.Data(Option(..), OptionParser, Parser(..))

instance Functor Option where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Option mv l s dv hm p) = Option mv l s (fmap f dv) hm (fmap f . p)

meta :: String -> Mod a
meta v = Mod $ \x -> x { metavar = v }

long :: String -> Mod a
long v = Mod $ \x -> x { longName = v }

short :: Char -> Mod a
short v = Mod $ \x -> x { shortName = v }

value :: a -> Mod a
value v = Mod $ \x -> x { defaultValue = Just v }

help :: String -> Mod a
help v = Mod $ \x -> x { helpMessage = v }


newtype Mod a = Mod (Option a -> Option a)

instance Semigroup (Mod a) where
    -- (<>) a -> a -> a
    (<>) (Mod a) (Mod b) = Mod (a . b)

option :: OptionParser a -> Mod a -> Parser a
option p (Mod m) = OptParser (fmap const (m $ def p)) (DefParser ())
   where
       def = Option "VAR" "" ' ' Nothing "No help message set."