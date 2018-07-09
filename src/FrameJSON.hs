{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module FrameJSON (dicoJSON) where

import           Text.JSON
import           Text.JSON.Pretty
import qualified Text.PrettyPrint as PP
import qualified Data.Text.Lazy   as T
import           FrameNet
import qualified Data.Map         as M
import           Control.Arrow

data AnyJSON where
    AnyJSON :: JSON a => a -> AnyJSON
instance JSON AnyJSON where
    readJSON jsv = fmap AnyJSON $ (readJSON jsv :: Result String)
    showJSON (AnyJSON x) = showJSON x

dicoJSON :: Dictionnary -> T.Text
-- dicoJSON = T.pack . encode . toJSON
dicoJSON = T.pack . PP.render . pp_value . showJSON . toJSON

toJSON :: Dictionnary -> [JSObject AnyJSON]
toJSON = map frameJSON . M.elems . dico_frames

frameJSON :: Frame -> JSObject AnyJSON
frameJSON (Frame name id fes rels _) =
    toJSObject
    [ ( "name" , AnyJSON name )
    , ( "id"   , AnyJSON id   )
    , ( "fes"  , feJSON  fes  )
    , ( "rels" , relJSON rels )
    ]

feJSON :: [FE] -> AnyJSON
feJSON = AnyJSON . map (fe_name &&& fe_id)

relJSON :: [(RelType,Int)] -> AnyJSON
relJSON = AnyJSON . map snd . filter (relPred . fst)

relPred :: RelType -> Bool
relPred rel = rel `elem` [ InheritsFrom
                         , SubframeOf
                         , Uses
                         , IsInchoativeOf
                         , IsCausativeOf
                         , Precedes
                         , PerspectiveOn
                         ]

