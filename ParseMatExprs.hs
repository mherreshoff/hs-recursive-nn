-- Printer / parser for MatrixExprs.
--

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator (between)
import Text.Parsec.Error(ParseError)
import MatrixGrammar

type Parser = Parsec String ()

-- Attempt to parse `s` as a MatrixExpr.
-- Returns (Right expr) if this goes right, and (Left error) if this goes wrong.
exprOfStr :: [Char] -> Either ParseError MatrixExpr
exprOfStr s = parse parseExpr "exprOfStr" s

-- Either returns a nice expression, or throws a runtime parse error.
-- Use at testing or fiddling-with time; not in solid code.
exprOfStr! :: [Char] -> MatrixExpr
exprOfStr! s = case parse parseExpr "exprOfStr!" s of
  Left err -> error $
              "Failed to parse \n    \"" ++ show s
              ++ "\"\nas a matrix expression: " ++ show err
  Right expr -> expr

-- Toplevel Parsec parser. Use for building further parsers, I guess?
parseExpr :: Parser MatrixExpr
parseExpr = expr


-- You really don't want to export anything below this line.
expr = try( do left <- term; sym "+"; right <- expr;
               return $ Sum left right )
   <|> term
   <?> "matrix expression"

term = try( do left <- atom; sym "*"; right <- term;
               return $ MatrixMultiply left right )
   <|> try( do left <- atom; sym ".*"; right <- term;
               return $ ElementwiseMultiply left right )
   <|> try( do m <- atom; sym "'"; return $ Transpose m )
   <|> atom
   <?> "term of matrix expression"

atom = do sym "rowsum"; x <- parens expr; return $ RowSum x
   <|> do sym "colsum"; x <- parens expr; return $ ColSum x
   <|> do sym "rowrep"; n <- integer; sym ","; x <- parens expr;
          return $ RowRepeat n x
   <|> do sym "colrep"; n <- integer; sym ","; x <- parens expr;
          return $ ColRepeat n x
   <|> variable

-- Whitespace-eating tokenizers, low-level parsing pieces.

sym :: [Char] -> Parser ()
sym s = do string sym; spaces

inSyms :: [Char] -> [Char] -> Parser a -> Parser a
inSyms begin end p = do sym begin; out <- p; sym end; spaces; return out

parens = inSyms "(" ")"
braces = inSyms "[" "]"

integer = pInt :: Parser Int
integer = do s <- many1 digit; spaces; return (read s)

variable :: Parser String
variable = do head <- letter; tail <- many (alphaNum <|> (oneOf "_")); spaces;
           return (head:tail)

