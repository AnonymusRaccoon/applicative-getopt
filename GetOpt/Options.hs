module GetOpt.Options where

import GetOpt.Data( Option(..), OptionParser )
import GetOpt.Parsers ( Parser(..) )

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

optionMatch :: Option a -> String -> Bool
optionMatch opt [d, s] = d == '-' && s == shortName opt
optionMatch opt (d:d1:l) = d == '-' && d1 ==  '-' && l == longName opt
optionMatch _ _ = False

auto :: Read a => OptionParser a
auto str = case reads str of
                [(v, "")] -> Just v
                _         -> Nothing