module Chapter13Ex where
    import Chapter13ParserExample
    import Chapter13Calculator
    import Control.Applicative

    -- 1. Define a parse comment :: Parse () for ordinary Haskell comments that 
    -- begin with the symboll -- and extend to the end of the current line, which is
    -- represented by the control character '\n'.
    -- -> So that means 2*-- then anything(letters/numbers/signs?) then '\n'

    comment :: Parser String
    comment = do symbol "--"
                 t <- many (sat (/= '\n'))
                 char '\n'
                 return t

    -- 2. Using our second grammar for arirthmetic expression, draw the two possible 
    -- parse trees for the expression 2+3+4
    --  
    --    + 
    --   / \
    --  2   + 
    --     / \
    --    3   4
    --
    --      +
    --     / \
    --    +   4
    --   / \
    --  2   3
    --
    -- 3. Using our third grammar for arithmetic expressions, draw the pasrse trees for
    -- the expressions 2+3, 2*3*4 and (2+3) +4
    -- 
    --  2+3
    --      +
    --     / \
    --    2   4
    --
    --  2*3*4 -> assume left associative
    --      *
    --     / \
    --    *   4
    --   / \
    --  2   3
    --
    --  (2+3) + 4:
    --      +
    --     / \
    --    *   4
    --   / \
    --  2   3

    -- 4. Explain why the final simplification of the grammar for arithmetic expressoins
    -- has a dramatic effect on the efficiency of the resulting parse. Hint: begin by
    -- considering how an expression comprising of a single number would be parsed if
    -- this simplification step had not been made.
    
    -- expr :: = term + expr | term simplifies to expr ::= term (+ expr| epsilon)
    -- Without the simplification, the parse would alway's try to parse this as (term + expr).
    -- this would involve parsing the term, and then parsing the expr. If there is no more expr,
    -- this will fail, and the parse will try (term) which involves parsing the term again.
    -- By simplifying to term (+ expr | epsilon) we can reuse the parsed term.

    -- 5. Define a suitable type Expr for arithmetic expressions and modfiy the parser for 
    -- expressions to have type expr:: Parser Expr

    data Express = SUM Express Express | MUL Express Express | Number Int

    _number :: Parser Express
    _number = Number <$> natural

    _expr :: Parser Express
    _expr = do t <- _term
               do symbol "+"
                  e <- _expr
                  return (SUM t e) --evaluate the expression
                <|> return t

    _term :: Parser Express
    _term = do f <- _factor 
               do  symbol "*"
                   t <- _term
                   return (MUL f t)
                <|> return f
    
    _factor :: Parser Express
    _factor = do symbol "("
                 e <- _expr 
                 symbol ")"
                 return e
               <|> _number
    
    -- 6. Extend the parser expr :: Parser Int to support substraction and division, 
    -- and to use integer values rather than natural numbers, based upon the following revision to the grammar.
    --
    -- expr   :: = term (+ expr | - expr | eps)
    -- term   ::= factor(*term | / term | eps)
    -- factor ::= (expr) | int
    -- int    ::= ... | -1 | 0 | 1 | ...
