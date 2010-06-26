{-# LANGUAGE ExistentialQuantification #-}

module Synops where

import Control.Applicative
import System.Vacuum.Cairo

data Path c x =
  End x
  | forall y. Continue (c -> Bool) (c -> y) (Parser c (y -> x))
data Parser c x = Parser [Path c x]

instance Functor (Path c) where
  fmap f (End x) = End $ f x
  fmap f (Continue t f' n) = Continue t f' $ fmap (f .) n

instance Functor (Parser c) where
  fmap f (Parser p) = Parser (map (fmap f) p)

instance Applicative (Parser c) where
  pure c = Parser [End c]
  Parser f <*> x = foldr (<|>) empty $ map follow f
    where
    follow (End f') = fmap f' x
    follow (Continue t f' n) = Parser . (: []) . Continue t f' $
      flip <$> n <*> x

instance Alternative (Parser c) where
  empty = Parser []
  Parser p <|> Parser p' = Parser (p ++ p')

parseOne :: Parser c x -> c -> Parser c x
parseOne (Parser p) c = 
  Parser $ concat [(\(Parser n') -> n') (($ f c) <$> n) | Continue t f n <- p, t c]

parseEnd (Parser p) =
  finish [x | End x <- p]
  where
  finish [x] = Just x
  finish _ = Nothing

parseList :: Parser c x -> [c] -> Maybe x
parseList p [] = parseEnd p
parseList p (c:cs) = parseList (parseOne p c) cs  

examples = mapM_ putStrLn
  [ f ( Parser
      [ End (1 :: Integer)
      , Continue (== 'a') (const 4) (Parser [End (* 2)])
      ]
    , "a")
  , f ( pure 'b' <|> token 'a'
    , "")
  , f ( pure 'b' <|> token 'a'
    , "a")
  , f ( pure (1 :: Integer) <|> (* 2) <$> (token 'a' *> pure 4)
    , "a")
  , f ( pure (1 :: Integer) <|> (* 2) <$> (token 'a' *> pure 4)
    , "b")
  , f ( pure (1 :: Integer) <|> (* 2) <$> (token 'a' *> pure 4)
    , "")
  , f ( many (token 'a')
    , "aaaaaaa")
  ]
  where
  f (p, d) = show $ parseList p d

match f = Parser [Continue f id (pure id)]
token c = match (== c) 



