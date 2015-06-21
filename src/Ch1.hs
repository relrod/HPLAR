module Ch1 where

data Expression =
    Const Integer
  | Var String
  | Add Expression Expression
  | Mul Expression Expression
  deriving (Eq, Show)

show' :: Expression -> String
show' (Var s) = s
show' (Const n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ " + " ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ " * " ++ show' e2 ++ ")"

simp1 :: Expression -> Expression
simp1 (Add (Const e1) (Const e2)) = Const (e1 + e2)
simp1 (Mul (Const e1) (Const e2)) = Const (e1 * e2)
simp1 (Add (Const 0) e2) = e2
simp1 (Add e1 (Const 0)) = e1
simp1 (Mul (Const 1) e2) = e2
simp1 (Mul e1 (Const 1)) = e1
simp1 e = e

simp :: Expression -> Expression
simp (Add e1 e2) = simp1 (Add (simp e1) (simp e2))
simp (Mul e1 e2) = simp1 (Mul (simp e1) (simp e2))
simp x = simp1 x

matches :: String -> Char -> Bool
matches s c = any (== c) s

space, punct, symb, numeric, alphanum :: Char -> Bool
space = matches " \t\n\r"
punct = matches "()[]{},"
symb = matches "~`!@#$%^&*()_+-=|\\:;<>.?/"
numeric = matches "0123456789"
alphanum = matches "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexwhile :: (Char -> Bool) -> String -> (String, String)
lexwhile predicate (x:xs) | predicate x = let (tok, rest) = lexwhile predicate xs in (x : tok, rest)
lexwhile _ i = ("", i)

lex' :: String -> [String]
lex' i =
  case snd (lexwhile space i) of
    [] -> []
    x:xs -> let predicate = if alphanum x then alphanum
                            else if symb x then symb
                                 else const False
                (tok, rest) = lexwhile predicate xs
            in (x:tok) : lex' rest

parseExpr :: [String] -> (Expression, [String])
parseExpr s =
  case parseProduct s of
    (e1, "+":i1) -> let (e2, i2) = parseExpr i1 in (Add e1 e2, i2)
    (e1, i1) -> (e1, i1)

parseProduct :: [String] -> (Expression, [String])
parseProduct s =
  case parseAtom s of
    (e1, "*":i1) -> let (e2, i2) = parseProduct i1 in (Mul e1 e2, i2)
    (e1, i1) -> (e1, i1)

parseAtom :: [String] -> (Expression, [String])
parseAtom [] = error "Expected expression at end of input"
parseAtom ("(":i1) =
  case parseExpr i1 of
    (e2, ")":i2) -> (e2, i2)
    _ -> error "Expected closing parenthesis"
parseAtom (tok:i1) = if all numeric tok
                     then (Const (read tok), i1)
                   else (Var tok, i1)

makeParser :: ([String] -> (Expression, [String])) -> String -> Expression
makeParser f s =
  case f (lex' s) of
    (e1, []) -> e1
    _ -> error "Unparsed input"

mainParser :: String -> Expression
mainParser = makeParser parseExpr
