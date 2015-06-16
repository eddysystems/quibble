-- | Special purpose regular expressions

module Regular (
  Reg(..)
  ) where
  
data Reg =
    REmpty
  | RSingle !Char
  | RSpace -- space or newline
  | RNewline
  | RCat Reg Reg
  | RStar Reg
  | RMaybe Reg

instance Show Reg where
  showsPrec _ REmpty = id
  showsPrec _ (RSingle c) = \r -> if elem c "()*?" then '\\':c:r else c:r
  showsPrec _ RSpace = (' ' :)
  showsPrec _ RNewline = ("\\n" ++)
  showsPrec p (RCat x y) = showParen (p > 1) $ showsPrec 1 x . showsPrec 1 y
  showsPrec p (RStar x)  = showParen (p > 2) $ showsPrec 2 x . ('*' :)
  showsPrec p (RMaybe x) = showParen (p > 2) $ showsPrec 2 x . ('?' :)
