-- Tests for the ParseMatExprs module
module ParseMatExprs_Test where

import Test.HUnit
import ParseMatExprs(exprOfStr)
import MatrixGrammar

is repr expr = repr ~: Right expr ~=? (exprOfStr repr)
fails repr =
  repr ~: assertBool
    ("Expression \"" ++ repr ++ "\" expected not to parse. ")
    isLeft
  where isLeft = case exprOfStr repr of
                  Left _ -> True
                  Right _ -> False

parser_tests = test [
  is "A" (Variable "A"),
  is "Z" (Variable "Z"),
  fails "@",
  fails "%*$(@",
  fails "[]",
  is "A + B" $ Sum (Variable "A") (Variable "B"),
  is "A * B" $ MatrixMultiply (Variable "A") (Variable "B"),
  is "A .* B" $ ElementwiseMultiply (Variable "A") (Variable "B"),
  is "A+B" $ Sum (Variable "A") (Variable "B"),
  is "a + b * c + d" $ Sum (Sum a (MatrixMultiply b c)) d,
  is "A .* A" $ ElementwiseMultiply (Variable "A") (Variable "A"),
  is "rowsum(B)" $ RowSum (Variable "B"),
  is "colsum(x)" $ ColSum (Variable "x"),
  is "rowrep(3, Z)" $ RowRepeat 3 (Variable "Z"),
  is "a_Long_Name_with_100s_of_Chars" $ Variable "a_Long_Name_with_100s_of_Chars",
  is "x'" $ Transpose (Variable "x"),
  is "(a + b * c)'" $
   Transpose (Sum (Variable "a")  (MatrixMultiply (Variable "b") (Variable "c"))),
  is "(a + b * c)'" $ Transpose (Sum a (MatrixMultiply b c) ),
  is "a + b * c'" $ Sum a (MatrixMultiply b (Transpose c)),
  is "a + (b * c)'" $ Sum a (Transpose (MatrixMultiply b c)),
  is "a' + (b * c)" $ Sum (Transpose a) (MatrixMultiply b c),
  is "a' + b * c" $ Sum (Transpose a) (MatrixMultiply b c),
  is "a' + b .* c" $ Sum (Transpose a) (ElementwiseMultiply b c),
  is "rowsum(b')" $ RowSum (Transpose b),
  is "rowsum(b)'" $ Transpose (RowSum b),
  is "colsum(rowsum(b))'" $ Transpose (ColSum (RowSum b)),
  is "colsum(rowsum(b)')" $ ColSum (Transpose (RowSum b)),
  is "colsum(rowsum(b'))" $ ColSum (RowSum (Transpose b)),
  is "colrep(7, rowsum(b))" $ ColRepeat 7 (RowSum b),
  is "colsum(rowrep(3, b))" $ ColSum (RowRepeat 3 b),
  is "a * b .* c" $ ElementwiseMultiply (MatrixMultiply a b) c,
  is "a .* b * c" $ MatrixMultiply (ElementwiseMultiply a b) c,
  is "a + b .* c" $ Sum a (ElementwiseMultiply b c),
  fails ""
  ] where
  a = Variable "a"
  b = Variable "b"
  c = Variable "c"
  d = Variable "d"

               
main = runTestTT parser_tests
