{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module SynopsCps where

import Control.Applicative

data MaybeS x =
  NothingS
  | JustS !x

maybeS d _ (NothingS) = d
maybeS _ f (JustS x) = f x

data Parser c x = Parser (forall y. ([c] -> x -> MaybeS y) -> [c] -> MaybeS y)

instance Functor (Parser c) where
  f `fmap` Parser p = Parser $ \s i ->
    p (\i' -> s i' . f) i

instance Applicative (Parser c) where
  pure x = Parser $ \s i -> s i x
  Parser f <*> ~(Parser x) = Parser $ \s i ->
    f (\i' f' -> x (\i'' x' -> s i'' $! f' x') i') i

instance Alternative (Parser c) where
  empty = Parser $ \_ _ -> NothingS
  Parser a <|> Parser b = Parser $ \s i ->
    maybeS (b s i) JustS (a s i)

parseList :: Parser c x -> [c] -> Maybe x
parseList (Parser p) c = maybeS Nothing Just $ p f c
  where
  f [] x = JustS x
  f _ _ = NothingS

examples = mapM_ putStrLn
  [ f ( pure 'b' <|> token 'a'
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
  , f ( (token 'a' *> token 'a' *> token 'a' *> pure 3) <|>
          (token 'a' *> token 'a' *> pure 2)
    , "aaa")
  , f ( (token 'a' *> token 'a' *> token 'a' *> pure 3) <|>
          (token 'a' *> token 'a' *> pure 2)
    , "aa")
  , f ( liftA2 (,) (pure 'a' <|> token 'b') (token 'c')
    , "bc")
  , f ( liftA2 (,) (pure 'a' <|> token 'b') (token 'c')
    , "c")
  , f ( liftA2 (,) (many $ token 'b') (token 'c')
    , "c")
  , f ( liftA2 (,) (many $ token 'b') (token 'c')
    , "bbbbc")
  ]
  where
  f (p, d) = show $ parseList p d

match t = Parser $ \s i -> case i of
  (x:xs) | t x -> s xs x
  _ -> NothingS
token t = match ({-# SCC "testing" #-} (== t))

exclude t x = Parser $ \s i -> case i of
  (x:xs) | t x -> s xs x
  _ -> NothingS

