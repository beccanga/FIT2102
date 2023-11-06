module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}
-- | BNF Grammar, referenced from : https://homepage.cs.uiowa.edu/~slonnegr/plf/Book/Chapter5.pdf
-- BNF Grammar is also included in my assignment report
-- <variable> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "x" | "y" | "z" 
-- <variableBuilder> ::= <variable>* | <variable>*
-- <parameter> ::= "(" <variableBuilder> ")"
-- <expressionBody> ::= "(" <variableBuilder> | <parameter> ")"
-- <alternate> ::= <expressionBody> | <expr> | <variableBuilder> | <parameter> 
-- <expr>  ::= "(" "/" <variable>* | <alternate> ")"
-- <longExpression> ::= "(" <expr> | <expressionBody> ")"
-- <longLambdaP> ::= <longExpression>
-- <shortExpressionBody> ::= <variableBuilder> | <parameter>
-- <shortExpr> ::= "(" "/" <variable>* | <shortAlternate> ")" | "(" <variableBuilder> ")"
-- <shortExpression> ::= "(" <shortExpr> | <shortExpressionBody> | <shortExpr> ")"
-- <shortAlternate> ::=  <shortExpressionBody> | <shortExpr> | <variableBuilder> | <parameter> 
-- <shortLambdaP> ::= <shortExpression> | <longExpression> 
-- <lambdaP> ::= <longExpression> | <shortExpression>

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "xx"
-- UnexpectedChar 'x'

-- <variable> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
--                "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" 
variable :: Parser Char
variable = is 'a' ||| is 'b' ||| is 'c' ||| is 'd' ||| is 'e' ||| is 'f' ||| is 'g' ||| is 'h' 
        ||| is 'i' ||| is 'j' ||| is 'k' ||| is 'l' ||| is 'm' ||| is 'n' ||| is 'o' ||| is 'p' 
        ||| is 'q' ||| is 'r' ||| is 's' ||| is 't' ||| is 'u' ||| is 'v' ||| is 'w' ||| is 'x' 
        ||| is 'y' ||| is 'z'

-- to receive single variable or multiple variables
-- <variableBuilder> ::= <variable>* | <variable>*
variableBuilder :: Parser Builder 
variableBuilder = do 
                singleVar <- variable 
                listVar <- list variable
                pure $ foldl ap (term singleVar) (term <$> listVar)

-- <parameter> ::= "(" <variableBuilder> | <expressionBody> ")"
parameter :: Parser Builder 
parameter = do
    _ <- is '('
    var <- variableBuilder
    _ <- is ')'
    pure $ var
    ||| do
    _ <- is '('
    expr <- expressionBody
    _ <- is ')'
    pure $ expr

-- <expressionBody> ::= "(" <variableBuilder> . <parameter> ")"
expressionBody :: Parser Builder 
expressionBody = do 
               var <- variableBuilder
               param <- parameter 
               pure $ ap (var) (param) 

-- <expr>  ::= "(" "λ" <variable>* . <alternate> ")"
expr :: Parser Builder
expr = do 
    _ <- is '('
    _ <- is 'λ'
    listVar <- list variable
    _ <- is '.'
    alt <- alternate
    _ <- is ')'
    pure $ foldr ($) alt (lam <$> listVar)

-- <longExpression> ::= (<expr> . <expressionBody>)
longExpression :: Parser Builder 
longExpression = do 
               body <- expr
               exprBody <- expressionBody
               pure $ ap (body) (exprBody) 
               ||| expr

-- <alternate> ::= <expressionBody> | <expr> | <variableBuilder> | <parameter> 
alternate :: Parser Builder
alternate = expressionBody ||| expr ||| variableBuilder ||| parameter 

-- <longLambdaP> ::= <longExpression>
longLambdaP :: Parser Lambda
longLambdaP = build <$> longExpression

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy

-- <shortExpressionBody> ::= <variableBuilder> | <parameter>
shortExpressionBody :: Parser Builder 
shortExpressionBody = do
                    var <- variableBuilder
                    param <- parameter 
                    pure $ ap (var) (param)
                    ||| shortExpression

-- <shortExpr> ::= "(" "λ" <variable>* . <shortAlternate> ")" | "(" <variableBuilder> ")"
shortExpr :: Parser Builder
shortExpr = do 
        _ <- is 'λ'
        var <- list variable
        _ <- is '.'
        alt <- shortAlternate
        pure $ foldr ($) alt (lam <$> var)
        ||| do 
        _ <- is '('
        shortE <- shortExpr
        _ <- is ')'
        pure $ shortE

-- <shortExpression> ::= (<shortExpr> . <shortExpressionBody> | <shortExpr>)
shortExpression :: Parser Builder 
shortExpression = do 
                shortBody <- shortExpr
                shortExprBody <- shortExpression
                pure $ ap (shortBody) (shortExprBody)
                ||| shortExpr

-- <shortAlternate> ::=  <shortExpressionBody> | <shortExpr> | <variableBuilder> | <parameter> 
shortAlternate :: Parser Builder
shortAlternate = shortExpressionBody ||| shortExpr ||| variableBuilder ||| parameter 

-- <shortLambdaP> ::= <shortExpression> | <longExpression> 
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> shortExpression ||| longExpression

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "xx"
-- UnexpectedChar 'x'

-- <lambdaP> ::= <longExpression> | <shortExpression>
lambdaP :: Parser Lambda
lambdaP = build <$> longExpression ||| shortExpression 

{-|
    Part 2
-}

-- retrieved from the course notes - Parser Combinators
-- https://tgdwyer.github.io/parsercombinators/
op :: Char -> Parser Char
op c = do
    spaces 
    is c
    pure c

-- retrieved/referenced from the course notes - Parser Combinators
-- https://tgdwyer.github.io/parsercombinators/
-- chain :: Parser a -> Parser (a->a->a) -> Parser a
-- chain p op = p >>= rest
--    where
--    rest a = (do
--                f <- op
--                b <- p
--                rest (f a b)
--             ) ||| pure a
chain :: Parser Builder -> Parser Builder -> Parser Builder
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest ((f) `ap` (a) `ap` (b))
            ) ||| pure a

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)\t_.t
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
--
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

-- retrieved from course notes: https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- AND = λxy. IF x  y FALSE
andStatement :: Parser Builder
andStatement = do 
            _ <- spaces
            _ <- string "and"
            pure $ ('x' `lam` 'y' `lam` (ifBuilder) `ap` (term 'x') `ap` (term 'y') `ap` (falseBuilder))

-- retrieved from course notes: https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- OR = λxy. IF x TRUE y 
orStatement :: Parser Builder 
orStatement = do 
            _ <- spaces
            _ <- string "or"
            pure $ ('x' `lam` 'y' `lam` (ifBuilder) `ap` (term 'x') `ap` (trueBuilder) `ap` (term 'y'))

-- retrieved from course notes: https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- NOT = λx. IF x FALSE TRUE
notStatement :: Parser Builder 
notStatement = do 
            _ <- spaces
            _ <- string "not"
            pure $ ('x' `lam` (ifBuilder) `ap` (term 'x') `ap` (falseBuilder) `ap` (trueBuilder))

-- retrieved from course notes: https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- IF = λbtf.b t f
ifStatement :: Parser Builder 
ifStatement = do 
            _ <- spaces 
            _ <- string "if"
            pure $ (ifBuilder)

-- IF = λbtf.b t f    
ifBuilder :: Builder 
ifBuilder = ('b' `lam` 't' `lam` 'f' `lam` (term 'b') `ap` (term 't') `ap` (term 'f'))

-- <alternateLogics> ::= <andStatement> | <orStatement> | <notStatement> | <ifStatement> 
alternateLogics :: Parser Builder
alternateLogics = andStatement ||| orStatement ||| notStatement ||| ifStatement

-- retrieved from course notes: https://tgdwyer.github.io/lambdacalculus/#church-encodings
-- TRUE = λxy.x    
trueStatement :: Parser Builder
trueStatement = do 
              _ <- spaces
              _ <- string "True"
              pure $ (trueBuilder)

-- Builder for TRUE statement
trueBuilder :: Builder
trueBuilder = (boolToLam True)

-- FALSE = λxy.y
falseStatement :: Parser Builder 
falseStatement = do 
               _ <- spaces
               _ <- string "False"
               pure $ (falseBuilder)

-- Builder for FALSE statement
falseBuilder :: Builder
falseBuilder = (boolToLam False)

-- <trueFalse> ::= <trueStatement> | <falseStatement>
trueFalse :: Parser Builder
trueFalse = trueStatement ||| falseStatement

-- <trueFalseExpression> ::= <trueFalse> ' ' <alternateLogics> ' ' <logicExpression>
trueFalseExpression :: Parser Builder 
trueFalseExpression = do
                    truefalse <- trueFalse
                    _ <- spaces
                    alt <- alternateLogics
                    _ <- spaces
                    logic <- logicExpression
                    pure $ (alt) `ap` (truefalse) `ap` (logic)

-- <thenElseExpression> ::= ' ' <alternateLogics> <logicExpression> "then" <logicExpression> "else" <logicExpression>
thenElseExpression :: Parser Builder
thenElseExpression = do
                alt <- alternateLogics
                _ <- spaces
                logic <- logicExpression
                _ <- string " then "
                logic2 <- logicExpression
                _ <- string " else "
                logic3 <- logicExpression
                pure $ (alt) `ap` (logic) `ap` (logic2) `ap` (logic3)

-- <logicExpression> ::= <thenElsePression> | <trueFalseExpression> | <alternateLogics> ' ' <logicExpression> | <trueFalse>
logicExpression :: Parser Builder 
logicExpression = do
                thenElseExpression
                ||| do
                trueFalseExpression
                ||| do
                alt <- alternateLogics
                _ <- spaces
                logic <- logicExpression
                pure $ (alt) `ap` (logic)
                ||| trueFalse

logicP :: Parser Lambda
logicP = build <$> logicExpression 

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

-- retrieved from the course notes - Parser Combinators
-- https://tgdwyer.github.io/parsercombinators/
integer :: Parser Char 
integer = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

integerBuilder :: Parser Builder 
integerBuilder = do 
        _ <- spaces 
        int <- integer
        listInteger <- list integer
        pure $ intToLam (read (foldl (++) [int] [listInteger]))

-- succ = λnfx.f(nfx)
succHelper :: Builder
succHelper = ('n' `lam` 'f' `lam` 'x' `lam` (term 'f') `ap` ((term 'n') `ap` (term 'f') `ap` (term 'x')))

-- pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
predHelper :: Builder
predHelper = (('n' `lam` 'f' `lam` 'x' `lam` (term 'n') `ap` ('g' `lam` 'h' `lam` (term 'h') `ap` ((term 'g') `ap` (term 'f'))) `ap` ('u' `lam` (term 'x')) `ap` ('u' `lam` (term 'u'))))

-- x + y = add = λxy.y succ m
-- x - y = minus = λxy.y pred x
operation :: Parser Builder
operation = do 
        _ <- spaces 
        _ <- op '+'
        pure $ ('x' `lam` 'y' `lam` (term 'y') `ap` (succHelper) `ap` (term 'x'))
        ||| do 
        _ <- spaces 
        _ <- op '-'
        pure $ ('x' `lam` 'y' `lam` (term 'y') `ap` (predHelper) `ap` (term 'x'))
      
arithmeticExpression :: Parser Builder
arithmeticExpression = chain (integerBuilder) (operation) 

basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> arithmeticExpression

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- x * y = multiply = λxyf.x(yf)
-- x ** y = exp = λxy.yx 
moreOperation :: Parser Builder 
moreOperation =  do
            _ <- spaces 
            _ <- op '*' 
            pure $ ('x' `lam` 'y' `lam` 'f' `lam` (term 'x') `ap` ((term 'y') `ap` (term 'f')))
            ||| do  
            _ <- string "**"
            pure $ ('x' `lam` 'y' `lam` (term 'y') `ap` (term 'x'))
            ||| do
            _ <- is '('
            brackets <- chain (integerBuilder) (moreOperation)
            _ <- is ')'
            pure $ brackets
            ||| integerBuilder

moreAriExpression :: Parser Builder 
moreAriExpression = moreOperation ||| operation 

moreArithmetic :: Parser Builder 
moreArithmetic = chain (integerBuilder) (moreAriExpression)

arithmeticP :: Parser Lambda
arithmeticP = build <$> moreArithmetic

-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True
--
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

complexCalcP :: Parser Lambda
complexCalcP = build <$> logicExpression ||| arithmeticExpression


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = undefined

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = undefined


-- | Exercise 2

-- | Implement your function(s) of choice below!
