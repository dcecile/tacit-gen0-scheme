import Data.Char
f x = putChar x >> putStrLn "" >> return (generalCategory x)
