{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import DicoXml
import DicoFrames
import Drawing
import FrameNet
import FCG
import GrammarGeneration

framenetpath = "/home/luc/school/ens/annee2/s2/code/framenet/fndata-1.6/"
out          = "grammar.lisp"

main :: IO ()
main = framenetToFCG framenetpath out

