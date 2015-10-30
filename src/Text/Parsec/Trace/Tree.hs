-- {-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction  #-}

module Text.Parsec.Trace.Tree (
    TraceTree
  , TraceConfig
  , HasTraceTree
  , defaultTraceConfig
  , getTrace
  , modTrace
  , setTraceFunction
  , setLogEnter
  , setLogExit
  , _traceParser
  , _logEnter
  , _logExit

  , log
  , drawTraceTree
  , getTraceTree
  , traceWith
  , initialTraceTree
    ) where

import Prelude hiding (last,log)

import Text.Parsec hiding (parse)
import Data.Tree
import Data.Tree.Zipper
import Control.Monad.State
import Data.Maybe

type TraceTree = TreePos Full String

class HasTraceTree t where
  getTrace :: t -> TraceTree
  modTrace :: (TraceTree -> TraceTree) -> t -> t

data TraceConfig a u m = TraceConfig { traceParser :: a -> String
                                     , logEnter    :: Maybe (u -> m String)
                                     , logExit     :: Maybe (u -> m String) }

-- setTraceFunction :: (a -> String) -> TraceConfig t u m -> TraceConfig a u m
setTraceFunction f conf = conf { traceParser = f }

-- setLogEnter :: (u -> m String) -> TraceConfig a u m -> TraceConfig a u m
setLogEnter lEn conf = conf { logEnter = Just lEn }

-- setLogExit :: (u -> m String) -> TraceConfig a u m -> TraceConfig a u m
setLogExit lEx conf = conf { logExit = Just lEx }

_traceParser :: Functor f => ((a -> String) -> f (a -> String)) -> TraceConfig a u m -> f (TraceConfig a u m)
_traceParser f (TraceConfig t lEn lEx) = fmap (\t' -> TraceConfig t' lEn lEx) (f t)

_logEnter :: Functor f => (Maybe (u -> m String) -> f (Maybe (u -> m String))) -> TraceConfig a u m -> f (TraceConfig a u m)
_logEnter f (TraceConfig t lEn lEx) = fmap (\lEn' -> TraceConfig t lEn' lEx) (f lEn)

_logExit :: Functor f => (Maybe (u -> m String) -> f (Maybe (u -> m String))) -> TraceConfig a u m -> f (TraceConfig a u m)
_logExit f (TraceConfig t lEn lEx) = fmap (TraceConfig t lEn) (f lEx)

initialTraceTree = fromTree (Node "Program" [])

                                     
defaultTraceConfig :: Show a => TraceConfig a u m
defaultTraceConfig = TraceConfig show Nothing Nothing


log :: (Monad m, HasTraceTree u) => (u -> m String) -> String -> ParsecT s u m () 
log action caption = do
  s <- getState
  result <- lift $ (caption ++) <$> action s
  modifyState . modTrace $ fromJust . parent . insert (Node result []) . last . children

tracedWithLog f p logInit logExit = do
  modifyState . modTrace $ insert (Node "" []) . last . children

  case logInit of { Nothing -> return (); Just init -> log init "On Enter: " }

  result <- p

  modifyState . modTrace $ modifyTree (\t -> t { rootLabel = f result })

  case logExit of { Nothing -> return (); Just exit -> log exit "On Enter: " }

  modifyState . modTrace $ fromJust . parent

  return result
  

traceWith (TraceConfig traceParser logEnter logExit) p = tracedWithLog traceParser p logEnter logExit

drawTraceTree :: HasTraceTree s => s -> String
drawTraceTree = drawTree . tree . getTrace 

getTraceTree :: HasTraceTree s => s -> Tree String
getTraceTree = tree . getTrace 
