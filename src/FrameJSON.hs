{-# LANGUAGE OverloadedStrings #-}

module FrameJSON (dicoJSON) where

import           Language.ASTMonad
import           Language.ASTMonad.Json
import           Language.ASTMonad.Json.Renderer
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as TB
import           FrameNet
import qualified Data.Map               as M

dicoJSON :: Dictionnary -> T.Text
dicoJSON dico = TB.toLazyText $ renderJson $ buildAST jsonDico dico JsonEnvironment

jsonDico :: Json Dictionnary
jsonDico = sequence_ . map jsonFrame
       =<< getParam (M.elems . dico_frames) 

jsonFrame :: Frame -> Json a
jsonFrame (Frame name _ fes _ _) = "frame" `isArray` do
    "name" `is` (TB.fromString name)
    sequence_ $ map jsonFE fes

jsonFE :: FE -> Json a
jsonFE (FE name _ _ _) = "fe" `is` (TB.fromString name)

