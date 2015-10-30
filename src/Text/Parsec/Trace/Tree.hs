{-|
Module      : Text.Parsec.Trace.Tree
Description : Add a tree tracing the successful parsers in the Parsec user state 
Copyright   : (c) Gregor Riegler, 2015
License     : MIT
Maintainer  : gregor.riegler@gmail.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables  #-}

module Text.Parsec.Trace.Tree (
    TraceTree
  -- * Classes
  , HasTraceTree
  , getTrace
  , modTrace

  -- * Trace configuration
  , TraceConfig
  , defaultTraceConfig

  -- ** Setting fields of the trace configuration
  , setTraceFunction
  , setLogEnter
  , setLogExit

  -- ** Lenses for modifying a trace configuration
  , _traceParser
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
import Data.String
import Data.Tree.Zipper
import Control.Monad.Trans
import Data.Maybe

type TraceTree a = TreePos Full a

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
data TraceConfig e s u m = TraceConfig { traceParser :: e -> s
                                       , logEnter    :: Maybe (u -> m s)
                                       , logExit     :: Maybe (u -> m s) }

setTraceFunction :: (IsString s ) =>
                    (e -> s)                  -- ^ How to produce a tree value given the result of a parser
                 -> TraceConfig e s u m       
                 -> TraceConfig e s u m
setTraceFunction f conf = conf { traceParser = f }

setLogEnter :: (IsString s ) =>
               (u -> m s)  -- ^ Logging action that is run at the start of a parser
            -> TraceConfig a s u m -> TraceConfig a s u m
setLogEnter lEn conf = conf { logEnter = Just lEn }

setLogExit :: (IsString s) =>
              (u -> m s)   -- ^ Logging action that is run at the end of the parser
           -> TraceConfig a s u m -> TraceConfig a s u m
setLogExit lEx conf = conf { logExit = Just lEx }

_traceParser :: (Functor f, IsString s) =>
                ((a -> s) -> f (a -> s)) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_traceParser f (TraceConfig t lEn lEx) = fmap (\t' -> TraceConfig t' lEn lEx) (f t)

_logEnter :: (Functor f, IsString s) =>
             (Maybe (u -> m s) -> f (Maybe (u -> m s))) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_logEnter f (TraceConfig t lEn lEx) = fmap (\lEn' -> TraceConfig t lEn' lEx) (f lEn)

_logExit :: (Functor f, IsString s) =>
            (Maybe (u -> m s) -> f (Maybe (u -> m s))) -> TraceConfig a s u m -> f (TraceConfig a s u m)
_logExit f (TraceConfig t lEn lEx) = fmap (TraceConfig t lEn) (f lEx)

-- | The value that can be used on initialisation of the Parsec user state
initialTraceTree :: (IsString s) => TraceTree s
initialTraceTree = fromTree (Node (fromString "Program") [])

-- | Default configuration which logs nothing on entering/exiting and ignores the parser values
-- Manipulate this default configuration with setters as 'setLogEnter' or lenses as '_logEnter'
defaultTraceConfig :: (IsString s) => TraceConfig e s u m
defaultTraceConfig = TraceConfig (const (fromString "")) Nothing Nothing

-- | Apply this function to your 'TraceConfig' to get a function which adds tracing to a parser
-- 
-- @
--    trace = traceWith (setTraceFunction show defaultTraceConfig)
-- 
--    myparser = trace $ do { string "parseSomething" }
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
        modifyState . modTrace $ insert (Node (fromString "") []) . last . children

        _ <- case logInit of { Nothing -> return (); Just init -> logP init }

        result <- p

        modifyState . modTrace $ modifyTree (\t -> t { rootLabel = f result })

        case logExit of { Nothing -> return (); Just exit -> logP exit }

        modifyState . modTrace $ fromJust . parent

        return result

drawTraceTree :: HasTraceTree t a => (a -> String) -> t -> String
drawTraceTree f = drawTree . fmap f . tree . getTrace 

drawTraceTree' :: HasTraceTree t String => t -> String
drawTraceTree' = drawTree . tree . getTrace 

getTraceTree :: (HasTraceTree t s, IsString s) => t -> Tree s
getTraceTree = tree . getTrace 

-- | Use 'logP' to log a monadic value in the 'TraceTree' as a leaf of the current parser
logP :: (Monad m, HasTraceTree u s, IsString s) => (u -> m s) -> ParsecT t u m () 
logP action = do
  s <- getState
  result <- lift $ action s
  modifyState . modTrace $ fromJust . parent . insert (Node result []) . last . children

  

