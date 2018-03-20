{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import DicoXml
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.GraphViz
import Diagrams.TwoD.Text
import Linear.V2
import Data.GraphViz
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe

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

approximateEnvelopeRadius :: (Enveloped a, V a ~ V2, Ord (N a))
                          => Integer -> a -> Maybe (N a)
approximateEnvelopeRadius n d = appEnvelope (getEnvelope d) >>= \env ->
    return $ minimum $ fmap (env . angle . (f n)) [0 .. n-1]
 where f :: Floating f => Integer -> Integer -> f
       f n k = 2 * pi * (fromInteger k) / (fromInteger n)

fitInRadius :: (Enveloped a, V a ~ V2, Ord (N a), Floating (N a), Transformable a)
            => N a -> a -> Maybe a
fitInRadius radius diagram =
    approximateEnvelopeRadius 50 diagram >>= \actual_radius -> 
        let ratio = radius / actual_radius in
        return $ scale ratio diagram

-- Unsafe
textInCircle :: (TypeableFloat n, Renderable (Text n) b)
             => n -> String -> QDiagram b V2 n Any
textInCircle radius str = fromJust $ fitInRadius radius $ text str

main :: IO ()
main = do
    lexie  <- readLexie ("polluer", "1a") "out/polluer.xml"
    layout <- layoutGraph TwoPi $ graphFromLexie lexie
    let drawing :: Diagram B
        drawing = drawGraph
                    (\nd -> place $ scale 5 $ text nd)
                    (\n1 p1 n2 p2 edg p -> arrowBetween' (opts p) p1 p2)
                    layout
        opts p  = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)
    defaultMain drawing

