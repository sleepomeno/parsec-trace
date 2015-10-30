{-|
Module      : Text.Parsec.Trace.Tree
Description : Add a tree tracing the successful parsers in the Parsec user state 
Copyright   : (c) Gregor Riegler, 2015
License     : MIT
Maintainer  : gregor.riegler@gmail.com
Stability   : experimental

Sometimes it is not trivial to understand when/why the parser state
changes when you have parsers that depend partly on each other. For
that reason, /parsec-trace/ instruments Parsec parsers in such a way
that a tree in the parser state is managed to
trace the hierarchy of successful parsers. In addition, hooks for
tracing information dependent on the parser state are provided on entering/exiting a parser.

@
\{\-\# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction  \#\-\}

module UseParsecTrace where

import Text.Parsec hiding (parse)
import qualified Text.Parsec.Trace.Tree as T

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
       in
           T.setTraceFunc show $ T.setLogEnter logEn $ T.setLogExit logEx T.defaultTraceConfig

-- Use the resulting function to instrument your parsers
withTrace = T.traceWith conf

data Expression = Expr1 | Expr2 | Expr3 | Expr4  deriving (Eq, Show)

parseExpr1 = withTrace $ do
    string "expr1"
    char ' '
    try parseExpr2 \<|\> parseExpr3
    return $ Expr1 

parseExpr2 = withTrace $ do
    string "expr2"
    modifyState $ \x -> x { importantInt = importantInt x + 1 }
    return $ Expr2

parseExpr3 = withTrace $ do
    string "expr3"
    return Expr3

parseExpr4 = withTrace $ do
    parseExpr2
    T.logP $ \(MyState importantInt _) -> return $ "in parseExpr4: " ++ show importantInt
    char ' '
    parseExpr3
    modifyState $ \x -> x { importantInt = importantInt x - 2 }
    return Expr4

anyParser = try parseExpr1 \<|\> try parseExpr4 \<|\> try parseExpr2 \<|\> parseExpr3

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
@

As a result, state transitions can thus be traced more easily than by
using "ad-hoc putStrLn-style", even more so in pure Parsec parsers with
an underlying Identity monad.

> parse "expr1 expr2 expr3 expr2 expr3"

> 
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

-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables  #-}

module Text.Parsec.Trace.Tree (
    TraceTree
  -- * HasTraceTree
  , HasTraceTree
  , getTrace
  , modTrace

  -- * Trace configuration
  , TraceConfig
  , defaultTraceConfig

  -- ** Setting fields of the trace configuration
  , setTraceFunc
  , setLogEnter
  , setLogExit

  -- ** Lenses for modifying a trace configuration
  , _traceFunc
  , _logEnter
  , _logExit

  -- * Provide tracing to parsers
  , traceWith
  , initialTraceTree
  , logP

  -- * Processing the user state providing the trace tree (after parsing)
  , drawTraceTree
  , drawTraceTree'
  , getTraceTree
    ) where

import Prelude hiding (last)

import Text.Parsec
import Data.Tree
import Data.Foldable
import Data.String
import Control.Monad.Trans
import Data.Maybe

-- type TraceTree a = TreePos Full a
type TraceTree a = MyTreePos a

-- | Position which does not point to a tree (e.g., it is between two trees).
-- data MyEmpty a    = MyE deriving (Read,Show,Eq)

-- -- | Position which points to a tree.
-- newtype MyFull a  = MyF { unMyF :: Tree a } deriving (Read,Show,Eq)


-- TODO show with quickqueck, that my mytreepos implementation is
                   -- equivalent to rosezipper --> maybe even
                   -- equational reasoning?

mytree1 = MyTree 1 [mytree2, mytree3]
mytree2 = MyTree 2 [mytree4, mytree5]
mytree3 = MyTree 3 [mytree6]
mytree4 = MyTree 4 []
mytree5 = MyTree 5 []
mytree6 = MyTree 6 []
mytree7 = MyTree 7 [mytree8]
mytree8 = MyTree 8 []

mytreepos1 = MyTreePos mytree1 [] []


data MyTree a = MyTree a (MyForest a) deriving (Show)
type MyForest a = [MyTree a]
data MyTreePos a = MyTreePos
  { 
    content :: MyTree a
  , siblings :: MyForest a
  , myparents  :: [(a, MyForest a)]
  } deriving (Show)

myinsert :: a -> MyTreePos a -> MyTreePos a
myinsert x (MyTreePos (MyTree c f) sibl parents) = MyTreePos (MyTree x []) f ((c, sibl):parents) 

myparent :: MyTreePos a -> Maybe (MyTreePos a)
myparent (MyTreePos c s p) = case p of
  (a, f) : ps -> Just $ MyTreePos (MyTree a (s ++ [c])) f ps
  []          -> Nothing

-- | An instance of 'HasTraceTree' somehow refers to a 'TraceTree'
--
--  Make your Parsec user state type an instance of this class.
class (IsString a) => HasTraceTree t a | t -> a where
  -- | Get the 'TraceTree' from the parser state 't'
  getTrace :: t -> TraceTree a
  -- | Modify the 'TraceTree' stored in the parser state 't'
  modTrace :: (TraceTree a -> TraceTree a) -> t -> t

-- | 'e' is the value type of your parsers
--
--   's' is the 'IsString' instance type of the tree values
--
--   'u' is the Parsec user state
--
--   'm' is the underlying monad of your Parsec parsers
data TraceConfig e s u m = TraceConfig { traceFunc   :: e -> s
                                       , logEnter    :: Maybe (u -> m s)
                                       , logExit     :: Maybe (u -> m s) }

setTraceFunc :: (IsString s ) =>
                (e -> s)                  -- ^ How to produce a tree value given the result of a parser
             -> TraceConfig e s u m       
             -> TraceConfig e s u m
setTraceFunc f conf = conf { traceFunc = f }

setLogEnter :: (IsString s ) =>
               (u -> m s)  -- ^ Logging action that is run at the start of a parser
            -> TraceConfig a s u m -> TraceConfig a s u m
setLogEnter lEn conf = conf { logEnter = Just lEn }

setLogExit :: (IsString s) =>
              (u -> m s)   -- ^ Logging action that is run at the end of the parser
           -> TraceConfig a s u m -> TraceConfig a s u m
setLogExit lEx conf = conf { logExit = Just lEx }

_traceFunc :: (Functor f, IsString s) =>
                ((a -> s) -> f (a -> s)) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_traceFunc f (TraceConfig t lEn lEx) = fmap (\t' -> TraceConfig t' lEn lEx) (f t)

_logEnter :: (Functor f, IsString s) =>
             (Maybe (u -> m s) -> f (Maybe (u -> m s))) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_logEnter f (TraceConfig t lEn lEx) = fmap (\lEn' -> TraceConfig t lEn' lEx) (f lEn)

_logExit :: (Functor f, IsString s) =>
            (Maybe (u -> m s) -> f (Maybe (u -> m s))) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_logExit f (TraceConfig t lEn lEx) = fmap (TraceConfig t lEn) (f lEx)

-- | The value that can be used on initialisation of the Parsec user state
initialTraceTree :: (IsString s) => TraceTree s
-- initialTraceTree = fromTree (Node (fromString "") [])
initialTraceTree = MyTreePos (MyTree (fromString "") []) [] []

-- | Default configuration which logs nothing on entering/exiting and ignores the parser values
-- Manipulate this default configuration with setters as 'setLogEnter' or lenses as '_logEnter'
defaultTraceConfig :: (IsString s) => TraceConfig e s u m
defaultTraceConfig = TraceConfig (const (fromString "")) Nothing Nothing

-- | Apply this function to your 'TraceConfig' to get a function which adds tracing to a parser
-- 
-- @
--    trace = traceWith (setTraceFunc show defaultTraceConfig)
-- 
--    myparser = trace $ string "parseSomething"
-- @
traceWith :: (Monad m, IsString s, HasTraceTree u s) => TraceConfig e s u m -> ParsecT t u m e -> ParsecT t u m e
traceWith (TraceConfig traceParser logEnter logExit) p = tracedWithLog traceParser p logEnter logExit
   where
    tracedWithLog :: (Monad m, IsString s, HasTraceTree u s) =>
                     (expr -> s) ->
                     ParsecT t u m expr ->
                     Maybe (u -> m s) ->
                     Maybe (u -> m s) ->
                     ParsecT t u m expr
    tracedWithLog f p logInit logExit = do
        -- modifyT $ insert (Node (fromString "") []) . last . children
        modifyT $ myinsert (fromString "") 

        forM_ logInit logP

        result <- p

        -- modifyT $ modifyTree (\t -> t { rootLabel = f result })
        let myModifyTree f (MyTreePos c s p) = (MyTreePos (f c) s p )
        modifyT $ myModifyTree (\(MyTree c s) -> MyTree (f result) s )

        forM_ logExit logP

        -- modifyT $ fromJust . parent
        modifyT $ fromJust . myparent

        return result

mytree :: MyTree a -> (a, MyForest a)
mytree (MyTree v c) =  (v, c)


-- myToTree :: MyTree a -> Tree a
-- myToTree (MyTree a s) = Node a (map myToTree s)

-- myroot :: MyTreePos a -> Tree a
-- myroot pos = let (MyTreePos c _ _) = go pos in myToTree c
myroot pos = let (MyTreePos c _ _) = go pos in c
   where
    go pos = maybe pos go (myparent pos)

drawTraceTree :: HasTraceTree t a => (a -> String) -> t -> String
-- drawTraceTree f = drawTree . fmap f . tree . getTrace 
drawTraceTree f = drawTree . fmap f . unfoldTree mytree . myroot . getTrace

drawTraceTree' :: HasTraceTree t String => t -> String
-- drawTraceTree' = drawTree . tree . getTrace 
drawTraceTree' = drawTree . unfoldTree mytree . myroot . getTrace 

getTraceTree :: (HasTraceTree t s, IsString s) => t -> Tree s
-- getTraceTree = tree . getTrace 
getTraceTree = unfoldTree mytree . myroot . getTrace

modifyT :: (Monad m, HasTraceTree u s) => (TraceTree s -> TraceTree s) -> ParsecT t u m ()
modifyT = modifyState . modTrace

-- | Use 'logP' to log a monadic value in the 'TraceTree' as a leaf of the current parser
logP :: (Monad m, HasTraceTree u s, IsString s) => (u -> m s) -> ParsecT t u m () 
logP action = do
  s <- getState
  result <- lift $ action s
  -- modifyT $ fromJust . parent . insert (Node result []) . last . children
  modifyT $ fromJust . myparent . myinsert result

  

