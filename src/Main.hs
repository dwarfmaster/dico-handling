{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import DicoXml
import DicoFrames
import FrameNet
import FCG
import GrammarGeneration
import FrameJSON
import qualified Data.Text.Lazy.IO as TIO

framenetpath = "/home/luc/school/ens/annee2/s2/code/framenet/fndata-1.6/"
out          = "lexicon.lisp"
pruning      = ["green", "boat"]

main :: IO ()
main = framenetDictionnary framenetpath
   >>= TIO.writeFile "grammar.json" . dicoJSON

