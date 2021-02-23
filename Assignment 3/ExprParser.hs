module ExprParser where

import Control.Applicative

import ExprDef
import ParserLib

mainParser :: Parser Expr
mainParser = whitespaces *> expr <* eof

expr :: Parser Expr
expr = operation
    where
        -- Bind right associating operators {||, &&}
        operation = chainr1 ands (operator "||" *> pure (Prim2 Or))
        ands = chainr1 (nextop <|> lastop) (operator "&&" *> pure (Prim2 And))
        -- Parse == op in monadic style (liftA4)
        nextop = lastop
            >>= \b -> operator "==" *> pure (Prim2 EqNat)
            >> lastop
            >>= \e -> pure b
            >> return (Prim2 EqNat b e)
        -- Bind left associating operators {+, -}
        lastop = chainl1 unary ((operator "+" *> pure (Prim2 Add)) <|> (operator "-" *> pure (Prim2 Sub)))
        -- Bind unary operators {!, -} with highest precedence
        unary =
            fmap (Prim1 Not) (operator "!" *> unary) <|>
            fmap (Prim1 Neg) (operator "-" *> unary) <|>
            atom
        -- recursive call
        atom =
            fmap LitNat natural <|>
            fmap Var var <|>
            (openParen *> expr <* closeParen)
        -- There are no identifiers reserved in this language
        var = identifier []
