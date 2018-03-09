{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import DicoXml
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.GraphViz
import Data.GraphViz
import Data.Graph.Inductive.PatriciaTree

graphFromLexie :: Lexie -> Gr String String
graphFromLexie lex = mkGraph nodes edges
 where iden  = identificateur lex
       nodes = mconcat [ [ iden ]
                       , map show $ synonyms lex
                       , map (show . ll_link) $ lexical_links lex
                       , map actant_name $ actants lex
                       , mconcat $ map (map show . actant_reals) $ actants lex
                       ]
       edges = mconcat
               [ [ (show s, iden, "synonym") | s <- synonyms lex ]
               , [ (show $ ll_link ll, iden, ll_family ll) | ll <- lexical_links lex ]
               , [ (actant_name a, iden, "Actant")  | a  <- actants lex ]
               , mconcat [ [ (show lid, actant_name a, "") | lid <- actant_reals a ]
                         | a <- actants lex ]
               ]

main :: IO ()
main = do
    lexie <- readLexie ("polluer", "1a") "out/polluer.xml"
    gr    <- simpleGraphDiagram TwoPi $ graphFromLexie lexie
    defaultMain gr

