{-# LANGUAGE NamedFieldPuns #-}
module Main where

import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.ANSI.Leijen

-- | Variable in an expression.
type Var = String

-- | Expression.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
  deriving Show

data Alphabet = Alphabet { a :: String,
                           b :: String,
                           c :: String,
                           d :: String,
                           e :: String,
                           f :: String,
                           g :: String,
                           h :: String,
                           i :: String,
                           j :: String,
                           k :: String,
                           l :: String,
                           m :: String,
                           n :: String,
                           o :: String,
                           p :: String,
                           q :: String,
                           r :: String,
                           s :: String,
                           t :: String,
                           u :: String,
                           v :: String,
                           w :: String,
                           x :: String,
                           y :: String,
                           z :: String
                         } deriving Show

exAlpha = Alphabet "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

pAlpha :: Alphabet -> Doc
pAlpha Alphabet{a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z} = do
  vsep [ string a
       , string b
       , string c
       , string d
       , string e
       , string f
       , string g
       , string h
       , string i
       , string j
       , string k
       , string l
       , string m
       , string n
       , string o
       , string p
       , string q
       , string r
       , string s
       , string t
       , string u
       , string v
       , string w
       , string x
       , string y
       , string z
       ]

-- | Very primitive, for illustration only!
--
-- Spaces for sums, but no spaces for products.
-- Soft breaks before operators.
prettyPrint :: Exp -> Doc
prettyPrint e = pFunc 10 e

-- | Pretty-print inside a precedence context to avoid parentheses.
-- Consider + to be 6, * to be 7.
pFunc :: Int -> Exp -> Doc
pFunc _ (N n) = int n
pFunc _ (V v) = text v
pFunc prec (Plus e1 e2) = maybeParens (prec < 7)
  (pFunc 7 e1 </> char '+' <+> pFunc 7 e2)
pFunc prec (Times e1 e2) = maybeParens (prec < 6)
  (pFunc 6 e1 <//> char '*' <> pFunc 6 e2)

maybeParens :: Bool -> Doc -> Doc
maybeParens True = parens
maybeParens False = id

-- usage examples
{-
*Main> :r
[1 of 1] Compiling Main             ( /home/cody/try-wlpprintansi/src/Main.hs, inte
rpreted )
Ok, one module loaded.
*Main> N 1
N 1
*Main> prettyPrint (N 1)
1
*Main> :t putDoc
putDoc :: Doc -> IO ()
*Main> putDoc (prettyPrint (N 1))
1*Main> let exp = Plus (N 1) (N 3)
*Main> putDoc (prettyPrint exp)
1 + 3*Main> 
-}

main :: IO ()
main = do
  putStrLn "hello world"
