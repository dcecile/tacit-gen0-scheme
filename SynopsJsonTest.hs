{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -fwarn-tabs #-}

import Prelude hiding (sequence)
import Control.Applicative
import qualified Synops as P
import Data.Char
import qualified Data.Foldable as F
import System.Environment
--import System.Vacuum.Cairo

token = P.token
notToken = P.match . (/=)
parse = P.parseList

--run :: forall t. P (Str Char) t -> String -> t
             
--exec :: P (Str Char) b -> String -> (b, [Error Char Char Int])

parseFromFile p f = readFile f >>= return . parse p
noProblem = maybe (error "Nothing") id


string = foldl (<*) (pure ()) . map token
oneOf = foldl (<|>) empty . map token
hexDigit = oneOf "0123456789abcdefABCDEF"
x `sepBy` y = liftA2 (:) x (many (y *> x)) <|> pure []


data Value =
    VObject [(String, Value)]
    | VArray [Value]
    | VString String
    | VNumber String
    | VBool Bool
    | VNull
    deriving (Show)

{-whitespace :: P.Parser ()
keyword :: String -> a -> P.Parser a
literalChar :: P.Parser Char
literalString :: P.Parser String
doc, value, stringv, number, true, false, nullv, array, object
    :: P.Parser Value
    -- :: Stream s m Char => ParsecT s u m Value-}

whitespace = oneOf " \n\t" *> whitespace <|> pure ()

keyword s x = string s *> pure x

literalChar :: P.Parser Char Char
literalChar = (token '\\' *> escapeChoices) <|> notToken '"' -- <?> "any character"
    where
    escapes =
        [ ('"', '"')
        , ('\\', '\\')
        , ('/', '/')
        , ('b', '\b')
        , ('f', '\f')
        , ('n', '\n')
        , ('r', '\r')
        , ('t', '\t')
        ]
    escapeChar (l, r) = token l *> pure r
    escapeChoices = foldr (<|>) rawUnicode . map escapeChar $ escapes
    rawUnicode =
        token 'u'
        *>
        ((\a b c d -> chr $
            16^(3::Integer) * digitToInt a + 
            16^(2::Integer) * digitToInt b + 
            16^(1::Integer) * digitToInt c + 
            16^(0::Integer) * digitToInt d)
        <$> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit)

{-
literalChar = satisfy t >>= f
    where
    t '"' = False
    t _ = True
    f '\\' = anyChar >>= g
    f c = return c
    g '"' = return '"'
    g '\\' = return '\\'
    g '/' = return '/'
    g 'b' = return '\b'
    g 'f' = return '\f'
    g 'n' = return '\n'
    g 'r' = return '\r'
    g 't' = return '\t'
    g 'u' = do
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return $ chr $
            16^3 * digitToInt a + 
            16^2 * digitToInt b + 
            16^1 * digitToInt c + 
            16^0 * digitToInt d
    g _ = parserFail "unexpected escape char"
-}

literalString =
    (token '"' {-<?> "opening quote"-}) *>
        many literalChar
    <* (token '"' {-<?> "ending quote"-}) 

doc = whitespace *> value -- <* eof

value = (stringv <|> number <|> true <|> false <|> nullv <|> array <|> object)
        <* whitespace

number = VNumber <$> some (oneOf $ ['0'..'9'] ++ "-.e")


stringv = VString <$> literalString

true = keyword "true" $ VBool True
false = keyword "false" $ VBool False
nullv = keyword "null" $ VNull

array =
    token '[' *> whitespace *>
        (VArray <$> sepBy value (token ',' *> whitespace))
    <* token ']'

object =
    token '{' *> whitespace *>
        (VObject <$> sepBy
                (liftA2 (,)
                    (literalString <* whitespace)
                    (token ':' *> whitespace *> value))
                (token ',' *> whitespace))
    <* token '}'

_decode :: String -> Value
_decode s = noProblem (parse doc s)

data Stats = Stats
    { objects :: Integer
    , arrays :: Integer
    , numbers :: Integer
    , strings :: Integer
    , booleans :: Integer
    , nulls :: Integer
    }

zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0

dispatch :: Stats -> Value -> Stats
dispatch s (VObject _j) = F.foldr' (flip dispatch) (s {objects = 1 + objects s})
                           . map snd
                           $ _j
dispatch s (VArray j)   = F.foldr' (flip dispatch) (s {arrays = 1 + arrays s}) j
dispatch s (VString _)  = s {strings = 1 + strings s}
dispatch s (VNumber _)  = s {numbers = 1 + numbers s}
dispatch s (VBool _) = s {booleans = 1 + booleans s}
dispatch s (VNull)      = s {nulls = 1 + nulls s}

results :: Stats -> String
results (Stats o a n s b nu) =
    foldl (\x y -> x . (' ' :) . shows y) ("synops" ++) [o, a, n, s, b, nu] ""

main :: IO ()
main =
    getArgs
    >>= parseFromFile doc . head
    >>= putStrLn . results . dispatch zeroStats . noProblem




