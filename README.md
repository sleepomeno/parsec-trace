# parsec-trace

Manages a tree tracing the hierarchical run of successful parsers in the Parsec user state.

## What is it for?

Sometimes it is not trivial to understand when/why the parser state changes when you have parsers that depend partly on each other. For that reason, *parsec-trace* instruments Parsec parsers in such a way that a tree in the parser state is managed to trace the hierarchy of successful parsers. In addition, hooks for tracing information dependent on the parser state are provided on entering/exiting a parser.

## Motivating example

```haskell
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction  #-}

module UseParsecTrace where

import Text.Parsec hiding (parse)
import qualified Text.Parsec.Trace.Tree as T
import Control.Monad.Identity

-- Parsec user state with a TraceTree
data MyState = MyState {
    importantInt :: Int
  , trace        :: T.TraceTree String } 

-- Tell me how to get to the tree/modify the tree
instance T.HasTraceTree MyState String where
    getTrace x = trace x
    modTrace f x = let t = trace x in x { trace = f t }

-- Specify the parsing configuration (you could use the provided lenses, too)
conf :: T.TraceConfig Expression String MyState Identity
conf = let logEn :: MyState -> Identity String
           logEn = logF "On Enter: "
           logEx = logF "On Exit: "
           logF caption = return . (caption ++ ) . show . importantInt
       in T.setTraceFunc show $ T.setLogEnter logEn $ T.setLogExit logEx T.defaultTraceConfig

-- Use the resulting function to instrument your parsers
withTrace = T.traceWith conf

data Expression = Expr1 | Expr2 | Expr3 | Expr4  deriving (Eq, Show)

parseExpr1 = withTrace $ do
    string "expr1"
    char ' '
    try parseExpr2 <|> parseExpr3
    return $ Expr1 

parseExpr2 = withTrace $ do
    string "expr2"
    modifyState $ \x -> x { importantInt = importantInt x + 1 }
    return $ Expr2

parseExpr4 = withTrace $ do
    parseExpr2
    T.logP $ \(MyState importantInt _) -> return $ "in parseExpr4: " ++ show importantInt
    char ' '
    parseExpr3
    modifyState $ \x -> x { importantInt = importantInt x - 2 }
    return Expr4

parseExpr3 = withTrace $ do
    string "expr3"
    return Expr3

anyParser = try parseExpr1 <|> try parseExpr4 <|> try parseExpr2 <|> parseExpr3

myparserWithState :: ParsecT String MyState Identity ([Expression], MyState)
myparserWithState = do
    result <- anyParser `sepBy1` char ' '
    s <- getState 
    return (result, s)

parse text = do
    result <- return . runIdentity $ runPT myparserWithState (MyState 0 T.initialTraceTree) "" text
    case result of
        Left e  -> print e
        Right (_, s) -> putStrLn . T.drawTraceTree' $ s

```
As a result, state transitions can thus be traced more easily than by using "ad-hoc putStrLn-style", even more so in pure Parsec parsers with an underlying Identity monad.

```haskell
parse "expr1 expr2 expr3 expr2 expr3"
```

```
> |
> +- Expr1
> |  |
> |  +- On Enter: 0
> |  |
> |  +- Expr2
> |  |  |
> |  |  +- On Enter: 0
> |  |  |
> |  |  `- On Exit: 1
> |  |
> |  `- On Exit: 1
> |
> +- Expr3
> |  |
> |  +- On Enter: 1
> |  |
> |  `- On Exit: 1
> |
> `- Expr4
>    |
>    +- On Enter: 1
>    |
>    +- Expr2
>    |  |
>    |  +- On Enter: 1
>    |  |
>    |  `- On Exit: 2
>    |
>    +- in parseExpr4: 2
>    |
>    +- Expr3
>    |  |
>    |  +- On Enter: 2
>    |  |
>    |  `- On Exit: 2
>    |
>    `- On Exit: 0
```

