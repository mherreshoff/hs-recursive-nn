-- Printer / parser for MatrixExprs.
module ParseMatExprs(exprOfStr, exprOfStr', parseExpr) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator (between, chainl1)
import Text.Parsec.Error(ParseError)
import MatrixGrammar

type Parser = Parsec String ()

-- Attempt to parse `s` as a MatrixExpr.
-- Returns (Right expr) if this goes right, and (Left error) if this goes wrong.
exprOfStr :: [Char] -> Either ParseError MatrixExpr
exprOfStr s = parse parseExpr "exprOfStr" s

-- Either returns a nice expression, or throws a runtime parse error.
-- Use at testing or fiddling-with time; not in solid code.
exprOfStr' :: [Char] -> MatrixExpr
exprOfStr' s = case exprOfStr s of
  Left err -> error $
              "Failed to parse \n    \"" ++ show s
              ++ "\"\nas a matrix expression: " ++ show err
  Right expr -> expr

-- Toplevel Parsec parser. Use for building further parsers, I guess?
parseExpr :: Parser MatrixExpr
parseExpr = expr

-- Toplevel expression. A sum of one or more terms.
expr = chainl1 term plus_op <?> "matrix expression"
  where plus_op = do sym "+"; return Sum

-- Term. The product of one or more "transatom"s.
term = chainl1 transatom mul_op <?> "term of matrix expression"
  where mul_op = do
          s <- (string "*" <|> string ".*")
          spaces
          case s of
           "*" -> return MatrixMultiply
           ".*" -> return ElementwiseMultiply
       
-- Transatom. 
transatom = try( do m <- atom; sym "'"; return $ Transpose m )
        <|> atom
        <?> "possibly-transposed atom of matrix expression"

-- Atom. A bottom-most expression in the matrix expression tree.
atom = do sym "rowsum"; x <- parens expr; return $ RowSum x
   <|> do sym "colsum"; x <- parens expr; return $ ColSum x
   <|> do sym "rowrep"; sym "("; n <- integer; sym ","; x <- expr; sym ")";
          return $ RowRepeat n x
   <|> do sym "colrep"; sym "("; n <- integer; sym ","; x <- expr; sym ")";
          return $ ColRepeat n x
   <|> parens expr
   <|> do s <- variable; return $ Variable s
   <?> "atom of matrix expression"

-- Whitespace-eating tokenizers, low-level parsing pieces.
-- TODO: read constant matrices.

sym :: [Char] -> Parser ()
sym s = try( do string s; spaces )

inSyms :: [Char] -> [Char] -> Parser a -> Parser a
inSyms begin end p = do sym begin; out <- p; sym end; spaces; return out

parens = inSyms "(" ")"
braces = inSyms "[" "]"

integer :: Parser Int
integer = do s <- many1 digit; spaces; return (read s)

variable :: Parser String
variable = do head <- letter
              tail <- many (alphaNum <|> (oneOf "_"))
              spaces
              return (head : tail)

