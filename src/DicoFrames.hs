{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE RecordWildCards           #-}

module DicoFrames ( RelationType(..), FrameId, Frame(..), FrameDico(..)
                  , lookup_frame, lookup_lid, children_for
                  , getFrameDico, readFrameDico
                  ) where

import           Text.XML.HXT.Core
import           Data.Monoid
import qualified Data.Map               as M
import           Data.Maybe
import           DicoXml                (LexieId, throwXml)


--  ____      _       _   _                 
-- |  _ \ ___| | __ _| |_(_) ___  _ __  ___ 
-- | |_) / _ \ |/ _` | __| |/ _ \| '_ \/ __|
-- |  _ <  __/ | (_| | |_| | (_) | | | \__ \
-- |_| \_\___|_|\__,_|\__|_|\___/|_| |_|___/
--                                          

data RelationType = Causative
                  | Inchoative
                  | UsedBy
                  | Uses
                  | InheritedBy
                  | Inherits
                  | SubframeOf
                  | HasSubframe
                  | Perspectivized
                  | PerspectiveOn
                  | PropertyOf
                  | HasProperty
                  | Precedes
                  | PrecededBy
                  | Opposes
                  | SeeAlso
                  deriving (Show,Eq)

read_relation :: String -> Maybe RelationType
read_relation str = lookup str dico
 where dico = [ ( "Is causative of"      , Causative      )
              , ( "Is inchoative of"     , Inchoative     )
              , ( "Is used by"           , UsedBy         )
              , ( "Uses"                 , Uses           )
              , ( "Is inherited by"      , InheritedBy    )
              , ( "Inherits from"        , Inherits       )
              , ( "Is subframe of"       , SubframeOf     )
              , ( "Has subframe"         , HasSubframe    )
              , ( "Is prespectivized in" , Perspectivized )
              , ( "Perspective on"       , PerspectiveOn  )
              , ( "Is a property of"     , PropertyOf     )
              , ( "Has property"         , HasProperty    )
              , ( "Precedes"             , Precedes       )
              , ( "Is preceded by"       , PrecededBy     )
              , ( "Opposes"              , Opposes        )
              , ( "See also"             , SeeAlso        )
              ]


--  _____                         
-- |  ___| __ __ _ _ __ ___   ___ 
-- | |_ | '__/ _` | '_ ` _ \ / _ \
-- |  _|| | | (_| | | | | | |  __/
-- |_|  |_|  \__,_|_| |_| |_|\___|
--                                

type FrameId = String
data Frame = Frame
           { fid       :: FrameId
           , statut    :: Int
           , cores     :: [(Int, String)]
           , actants   :: [(Int,String)]
           , lexies    :: [LexieId]
           , relations :: [(RelationType,FrameId)]
           }
           deriving (Show,Eq)
data FrameDico = FrameDico (M.Map String Frame) (M.Map LexieId [Frame])
                 deriving (Show,Eq)

lookup_frame :: FrameDico -> FrameId -> Maybe Frame
lookup_frame (FrameDico frd _) = (flip M.lookup) frd

lookup_lid :: FrameDico -> LexieId -> [Frame]
lookup_lid (FrameDico _ frs) lid = M.findWithDefault [] lid frs

children_for :: RelationType -> Frame -> [FrameId]
children_for rtype (Frame { relations = rel, .. }) =
    map snd $ filter ((== rtype) . fst) rel

--  ____                _             
-- |  _ \ __ _ _ __ ___(_)_ __   __ _ 
-- | |_) / _` | '__/ __| | '_ \ / _` |
-- |  __/ (_| | |  \__ \ | | | | (_| |
-- |_|   \__,_|_|  |___/_|_| |_|\__, |
--                              |___/ 

getElem :: ArrowXml a => String -> a XmlTree (Int,String)
getElem name =
     isElem >>> hasName name
 >>> hasAttr "no"
 >>> ( (getAttrValue "no" >>^ read)
   &&& (getChildren >>> isText >>> getText)
     )

getChildrenList :: ArrowXml a => String -> a XmlTree b -> a XmlTree [b]
getChildrenList name chd =
     isElem >>> hasName name
 >>> ( (getChildren >>> chd) 
    >. id
     )

getElems :: ArrowXml a => String -> a XmlTree [(Int,String)]
getElems name = getChildrenList (name <> "s") $ getElem name

getCoreFEs :: ArrowXml a => a XmlTree [(Int,String)]
getCoreFEs = getElems "Core-FE"

getActants :: ArrowXml a => a XmlTree [(Int,String)]
getActants = getElems "Actant"

getLexie :: ArrowXml a => a XmlTree LexieId
getLexie =
     isElem >>> hasName "Lexie"
 >>> (getAttrValue "identificateur"  &&& getAttrValue "no_acception")

getLexies :: ArrowXml a => a XmlTree [LexieId]
getLexies = getChildrenList "Lexies" getLexie

getRelation :: ArrowXml a => a XmlTree (RelationType,FrameId)
getRelation =
     isElem >>> hasName "Relation"
 >>> ( (getAttrValue "nom" >>^ fromJust . read_relation)
   &&& (getChildren >>> isText >>> getText)
     )

getRelations :: ArrowXml a => a XmlTree [(RelationType,FrameId)]
getRelations = getChildrenList "Relations" getRelation

getFrame :: ArrowXml a => a XmlTree Frame
getFrame =
     isElem >>> hasName "Frame"
 >>> ( ( getAttrValue "identificateur"  )
   &&& ( getAttrValue "statut" >>^ read )
   &&& ( getChildren >>> getCoreFEs     )
   &&& ( getChildren >>> getActants     )
   &&& ( getChildren >>> getLexies      )
   &&& ( getChildren >>> getRelations   )
     )
 >>^ \(fid, (st, (fes, (acts, (lexs, rels)))))
         -> Frame fid st fes acts lexs rels

mkFrameDico :: [Frame] -> FrameDico
mkFrameDico frs = FrameDico
                     (M.fromList $ map (\fr -> (fid fr, fr)) frs)
                     (M.fromListWith (<>) llist)
 where llist = mconcat $ map (\fr -> map (,[fr]) $ lexies fr) frs

getFrameDico :: ArrowXml a => a XmlTree FrameDico
getFrameDico =
     getChildrenList "Frames" getFrame
 >>^ mkFrameDico

readFrameDico :: FilePath -> IO FrameDico
readFrameDico path = do
    ds <- runX $ readDocument [ withValidate no ] path
              /> getFrameDico
    case ds of
     []  -> throwXml $ "File " <> path <> " is no frame dictionnary"
     d:_ -> return d

