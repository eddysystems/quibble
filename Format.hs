-- | Neural network reformatting

module Format
  ( format
  ) where

import Regular
import Search
import Util
import Data.Char
import Data.List
import Data.Ord
import Language.JavaScript.Parser

data RNN a = RNN { step :: Maybe a -> (Score,RNN a) }

format :: String -> JSNode -> String
format src js = best  where
  best = snd $ minimumBy (comparing fst) results

  results :: [(Score,String)]
  results = take 1000 $ beamSearch 16 $ map (score nn) (expand $ regular js)

  -- TODO
  s = 1.7
  _ = src
  nn = RNN $ \_ -> (s,nn)

score :: RNN a -> NTree a -> Tree Score a
score _ NLeaf = Leaf -- TODO: step EOF
score nn (NNode x es) = Node x $ map f es where
  f t = second (flip score t) (step nn $ label t)

-- Expand a regular expression into a (possibly infinite) lazy trie
expand :: Reg -> [NTree Char]
expand r = expand' r [NLeaf]

-- expand' r t is expand r followed by t
expand' :: Reg -> [NTree Char] -> [NTree Char]
expand' REmpty r = r
expand' (RSingle c) r = [NNode c r]
-- TODO: Missing byte order mark (http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
expand' RSpace r   = map (flip NNode r) " \t\v\f\xa0\n\r" -- Newlines legal!
-- TODO: Missing line separator and paragraph separator (http://www.ecma-international.org/ecma-262/5.1/#sec-7.3)
expand' RNewline r = map (flip NNode r) "\n\r"
expand' (RCat x y) r = expand' x $ expand' y r
expand' (RStar x) r = ys where ys = r ++ expand' x ys
expand' (RMaybe x) r = r ++ expand' x r

-- Represent all possible reformattings of some Javascript as a regular expression
regular :: JSNode -> Reg
regular js = regs $ tokens js []

regs :: [Tok] -> Reg
regs = trail . sep . smash . strip where
  strip = trimWhile isWhite where
    isWhite (White _) = True
    isWhite (Black _) = False

  smash :: [Tok] -> [Tok]
  smash [] = []
  smash (White x : xs) = case smash xs of
    White y : z -> White (x ++ y) : z
    z -> z
  smash (Black x : xs) = Black x : smash xs

  white0 = RStar RSpace
  white1 = RCat RSpace white0
  line = RCat RNewline white0

  trail :: Reg -> Reg
  trail x = RCat white0 $ RCat x $ white0

  sep :: [Tok] -> Reg
  sep [] = REmpty
  sep (Black x           : xs@(Black y : _)) = RCat (reg x) $ RCat (white (safe x y) " ") (sep xs)
  sep (Black x : White s : xs@(Black y : _)) = RCat (reg x) $ RCat (white (safe x y) s)   (sep xs)
  sep (Black _ : White _ : White _ : _) = error "consecutive whitespace"
  sep (Black x : White _ : []) = RCat (reg x) (RStar RSpace)
  sep (Black x : []) = reg x
  sep (White _ : _) = error "unexpected whitespace"

  -- Can these tokens be juxtaposed with no whitespace?
  -- TODO: Doesn't handle general unicode
  safe :: String -> String -> Bool
  safe x y = not (isAlphaNum (last x) && isAlphaNum (head y))

  isLine c = c == '\n' || c == '\r'

  white :: Bool -> String -> Reg
  white _ s | any isLine s = line
  white True _ = white0
  white False _ = white1

  reg :: String -> Reg
  reg = foldr1 RCat . map RSingle

data Tok
  = Black String
  | White String

-- For showsPrec trick
type Toks = [Tok] -> [Tok]

class Tokens a where
  tokens :: a -> Toks

instance Tokens a => Tokens [a] where
  tokens [] r = r
  tokens (x : xs) r = tokens x $ tokens xs r

instance Tokens JSNode where
  tokens (NT t _ cs) = tokens t . tokens cs
  tokens (NN t) = tokens t

instance Tokens CommentAnnotation where
  tokens (CommentA _ s) = (Black s :)
  tokens (WhiteSpace _ s) = (White s :)
  tokens NoComment = id

instance Tokens Node where
  tokens = t where
    b :: String -> Toks
    b x r = Black x : r

    ts = tokens

    -- t is mostly copied from rn in language-javascript Printer.hs
    t :: Node -> Toks
    -- Terminals
    t (JSIdentifier s)      = b s
    t (JSDecimal i)         = b i
    t (JSLiteral l)         = b l
    t (JSHexInteger i)      = b i
    t (JSOctal i)           = b i
    t (JSStringLiteral s l) = b $ s:l++[s]
    t (JSRegEx s)           = b s
    -- Nonterminals
    t (JSArguments lb xs rb)                    = ts $ [lb] ++ xs ++ [rb]
    t (JSArrayLiteral lb xs rb)                 = ts $ [lb] ++ xs ++ [rb]
    t (JSBlock lb x rb)                         = ts $ lb ++ x ++ rb
    t (JSBreak b x1s as)                        = ts $ [b]++x1s++[as]
    t (JSCallExpression _ os xs cs)             = ts $ os ++ xs ++ cs
    t (JSCase ca x1 c x2s)                      = ts $ [ca,x1,c]++x2s
    t (JSCatch c lb x1 x2s rb x3)               = ts $ [c,lb,x1]++x2s++[rb,x3]
    t (JSContinue c xs as)                      = ts $ [c]++xs++[as]
    t (JSDefault d c xs)                        = ts $ [d,c]++xs
    t (JSDoWhile d x1 w lb x2 rb x3)            = ts $ [d,x1,w,lb,x2,rb,x3]
    t (JSElision c)                             = ts [c]
    t (JSExpression xs)                         = ts xs
    t (JSExpressionBinary _ lhs op rhs)         = ts $ lhs ++ [op] ++ rhs
    t (JSExpressionParen lb e rb)               = ts $ [lb,e,rb]
    t (JSExpressionPostfix _ xs op)             = ts $ xs ++ [op]
    t (JSExpressionTernary cond h v1 c v2)      = ts $ cond ++[h] ++ v1 ++ [c] ++ v2
    t (JSFinally f x)                           = ts $ [f,x]
    t (JSFor f lb x1s s1 x2s s2 x3s rb x4)      = ts $ [f,lb]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]
    t (JSForIn f lb x1s i x2 rb x3)             = ts $ [f,lb]++x1s++[i,x2,rb,x3]
    t (JSForVar f lb v x1s s1 x2s s2 x3s rb x4) = ts $ [f,lb,v]++x1s++[s1]++x2s++[s2]++x3s++[rb,x4]
    t (JSForVarIn f lb v x1 i x2 rb x3)         = ts $ [f,lb,v,x1,i,x2,rb,x3]
    t (JSFunction f x1 lb x2s rb x3)            = ts $ [f,x1,lb]++x2s++[rb,x3]
    -- f (JSFunctionBody xs)                    = ts $ xs
    t (JSFunctionExpression f x1s lb x2s rb x3) = ts $ [f] ++ x1s ++ [lb] ++ x2s ++ [rb,x3]
    t (JSIf i lb x1 rb x2s x3s)                 = ts $ [i,lb,x1,rb]++x2s++x3s
    t (JSLabelled l c v)                        = ts $ [l,c,v]
    t (JSMemberDot xs dot n)                    = ts $ xs ++ [dot,n]
    t (JSMemberSquare xs lb e rb)               = ts $ xs ++ [lb,e,rb]
    t (JSObjectLiteral lb xs rb)                = ts $ [lb] ++ xs ++ [rb]
    t (JSOperator n)                            = ts $ [n]
    t (JSPropertyAccessor s n lb1 ps rb1 b)     = ts $ [s,n,lb1] ++ ps ++ [rb1,b]
    t (JSPropertyNameandValue n colon vs)       = ts $ [n,colon] ++ vs
    t (JSReturn r xs as)                        = ts $ [r] ++ xs ++ [as]
    -- t (JSSourceElements    xs)               = ts $ xs
    t (JSSourceElementsTop xs)                  = ts $ xs
    -- t (JSStatementBlock lb x rb)             = ts $ [lb,x,rb]
    -- t (JSStatementList xs)                   = ts $ xs
    t (JSSwitch s lb x rb x2)                   = ts $ [s,lb,x,rb,x2]
    t (JSThrow t x)                             = ts $ [t,x]
    t (JSTry t x1 x2s)                          = ts $ [t,x1]++x2s
    t (JSUnary _ n)                             = ts $ [n]
    t (JSVarDecl x1 x2s)                        = ts $ [x1]++x2s
    t (JSVariables n xs as)                     = ts $ [n]++xs++[as]
    t (JSWhile w lb x1 rb x2)                   = ts $ [w,lb,x1,rb,x2]
    t (JSWith w lb x1 rb x2s)                   = ts $ [w,lb,x1,rb]++x2s