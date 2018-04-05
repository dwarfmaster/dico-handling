
module FrameNet ( CoreType(..), FE(..), RelType(..), Frame(..)
                , LexUnit(..), FrameRelationType(..), RelId
                , FEBinding(..), FrameRelation(..), Dictionnary(..)
                , framenetDictionnary
                , lookupFrame, lookupRel, lookupRelId
                ) where

import           System.Directory
import           Text.XML.HXT.Core
import           Data.Char         (toLower)
import           Data.Map          (Map)
import qualified Data.Map          as M
import qualified Text.Read         as TR
import qualified Data.Maybe        as Mb
import           Data.Monoid
import           Control.Monad     ((>=>))

data CoreType = Core | ExtraThematic | CoreUnexpressed | Peripheral
              deriving (Show,Eq,Ord)
readCT :: Monad m => String -> m CoreType
readCT "Core"             = return Core
readCT "Core-Unexpressed" = return CoreUnexpressed
readCT "Extra-Thematic"   = return ExtraThematic
readCT "Peripheral"       = return Peripheral
readCT _                  = fail "Invalid core type"

data FE = FE { fe_name     :: String
             , fe_id       :: Int
             , fe_coreType :: CoreType
             , fe_semType  :: Maybe String
             }
             deriving (Show)

data RelType = InheritsFrom
             | IsInheritedBy
             | PerspectiveOn
             | IsPerspectivizedIn
             | Uses
             | IsUsedBy
             | SubframeOf
             | HasSubframe
             | Precedes
             | IsPrecededBy
             | IsInchoativeOf
             | IsCausativeOf
             | SeeAlso
             deriving (Show,Eq,Ord)
readRT :: Monad m => String -> m RelType
readRT = readRT' . (map toLower)
readRT' :: Monad m => String -> m RelType
readRT' "inherits from"        = return InheritsFrom
readRT' "is inherited by"      = return IsInheritedBy
readRT' "perspective on"       = return PerspectiveOn
readRT' "is perspectivized in" = return IsPerspectivizedIn
readRT' "uses"                 = return Uses
readRT' "is used by"           = return IsUsedBy
readRT' "subframe of"          = return SubframeOf
readRT' "has subframe(s)"      = return HasSubframe
readRT' "precedes"             = return Precedes
readRT' "is preceded by"       = return IsPrecededBy
readRT' "is inchoative of"     = return IsInchoativeOf
readRT' "is causative of"      = return IsCausativeOf
readRT' "see also"             = return SeeAlso
readRT' _                      = fail "Invalid frame relation type"

data LexUnit = LU { lu_name :: String
                  , lu_id   :: Int
                  , lu_lex  :: String
                  }
                  deriving (Show)

data Frame = Frame { frame_name :: String
                   , frame_id   :: Int
                   , frame_fes  :: [FE]
                   , frame_rels :: [(RelType,Int)]
                   , frame_lus  :: [LexUnit]
                   }
                   deriving (Show)

data FrameRelationType = FRT_Inheritance
                       | FRT_Subframe
                       | FRT_Using
                       | FRT_SeeAlso
                       | FRT_InchoativeOf
                       | FRT_CausativeOf
                       | FRT_Precedes
                       | FRT_PerspectiveOn
                       deriving (Show,Eq,Ord)
readFRT :: Monad m => String -> m FrameRelationType
readFRT "Inheritance"    = return FRT_Inheritance
readFRT "Subframe"       = return FRT_Subframe
readFRT "Using"          = return FRT_Using
readFRT "See also"       = return FRT_SeeAlso
readFRT "Inchoative of"  = return FRT_InchoativeOf
readFRT "Causative of"   = return FRT_CausativeOf
readFRT "Precedes"       = return FRT_Precedes
readFRT "Perspective on" = return FRT_PerspectiveOn
readFRT _                = fail "Invalid frame relation type"

data FEBinding = FEB { feb_subid   :: Int
                     , feb_subname :: String
                     , feb_supid   :: Int
                     , feb_supname :: String
                     }
                     deriving (Show)

data FrameRelation = FR { fr_type     :: FrameRelationType
                        , fr_superid  :: Int
                        , fr_subid    :: Int
                        , fr_bindings :: [FEBinding]
                        }
                        deriving (Show)

type RelId = (FrameRelationType, Int, Int)
mkRelId :: RelType -> Int -> Int -> RelId
mkRelId InheritsFrom       sub sup = (FRT_Inheritance,   sub, sup)
mkRelId IsInheritedBy      sub sup = (FRT_Inheritance,   sup, sub)
mkRelId PerspectiveOn      sub sup = (FRT_PerspectiveOn, sub, sup)
mkRelId IsPerspectivizedIn sub sup = (FRT_PerspectiveOn, sup, sub)
mkRelId Uses               sub sup = (FRT_Using,         sub, sup)
mkRelId IsUsedBy           sub sup = (FRT_Using,         sup, sub)
mkRelId SubframeOf         sub sup = (FRT_Subframe,      sub, sup)
mkRelId HasSubframe        sub sup = (FRT_Subframe,      sup, sub)
mkRelId Precedes           sub sup = (FRT_Precedes,      sub, sup)
mkRelId IsPrecededBy       sub sup = (FRT_Precedes,      sup, sub)
mkRelId IsInchoativeOf     sub sup = (FRT_InchoativeOf,  sub, sup)
mkRelId IsCausativeOf      sub sup = (FRT_CausativeOf,   sub, sup)
-- TODO order has no meaning for see also
mkRelId SeeAlso            sub sup = (FRT_SeeAlso,       sub, sup)

data Dictionnary = Dico { dico_frames :: Map Int Frame
                        , dico_rels   :: Map RelId FrameRelation
                        }
                        deriving (Show)

unmaybeM :: Monad m => String -> Maybe a -> m a
unmaybeM msg Nothing  = fail msg
unmaybeM _   (Just x) = return x

lookupFrame :: Monad m => Dictionnary -> Int -> m Frame
lookupFrame (Dico frs _) fid = unmaybeM ("No frame with id " <> show fid)
                                       $ M.lookup fid frs

lookupRelId :: Monad m => Dictionnary -> RelId -> m FrameRelation
lookupRelId (Dico _ rls) rid = unmaybeM ("No relation with id " <> show rid)
                                       $ M.lookup rid rls

lookupRel :: Monad m => Dictionnary -> Int -> RelType -> Int -> m FrameRelation
lookupRel dico sub rel sup = lookupRelId dico $ mkRelId rel sub sup

framenetDictionnary :: FilePath -> IO Dictionnary
framenetDictionnary path = (runX >=> unlist) $
     ( (constA (path <> "/frame/") >>> parseFrames >. buildDico)
   &&& (constA (path <> "/frRelation.xml") >>> parseFRs >. buildFRDico)
     )
 >>^ uncurry Dico
  where unlist []  = fail "No dictionnary found"
        unlist [h] = return h
        unlist _   = fail "More than one dictionnary found"

buildDico :: [Frame] -> Map Int Frame
buildDico frs = M.fromList $ map (\fr -> (frame_id fr, fr)) frs

parseFrames :: IOSLA (XIOState ()) FilePath Frame
parseFrames =
     ( (arr id) &&& (arrIO listDirectory >>> unlistA) )
 >>> arr (uncurry (<>))
 >>> parseFrameFrom

parseFrameFrom :: IOSLA (XIOState ()) FilePath Frame
parseFrameFrom = readFromDocument [ withValidate no ] >>> parseFrame

readA :: (ArrowList a, Read b) => a String b
readA = arr TR.readMaybe >>> unmaybeA

unmaybeA :: ArrowList a => a (Maybe b) b
unmaybeA = arr Mb.maybeToList >>> unlistA

parseFrame :: ArrowXml a => a XmlTree Frame
parseFrame = deep $
     isElem >>> hasName "frame"
 >>> ( (getAttrValue "name" &&& (getAttrValue "ID" >>> readA))
   &&& ((getChildren >>> parseFE ) >. id)
   &&& ((getChildren >>> parseRel) >. id)
   &&& ((getChildren >>> parseLU ) >. id)
     )
 >>^ \((name,fid), (fes, (rels, lus)))
        -> Frame { frame_name = name
                 , frame_id   = fid
                 , frame_fes  = fes
                 , frame_rels = rels
                 , frame_lus  = lus
                 }

parseFE :: ArrowXml a => a XmlTree FE
parseFE =
     isElem >>> hasName "FE"
 >>> ( (getAttrValue "coreType" >>> arr readCT >>> unmaybeA)
   &&& (getAttrValue "name")
   &&& (getAttrValue "ID" >>> readA)
   &&& ( (getChildren >>> isElem >>> hasName "semType" >>> getAttrValue "name")
      >. Mb.listToMaybe
       )
     )
 >>^ \(ct, (name, (fid, semt))) -> FE { fe_name     = name
                                      , fe_id       = fid
                                      , fe_coreType = ct
                                      , fe_semType  = semt
                                      }

parseRel :: ArrowXml a => a XmlTree (RelType,Int)
parseRel =
     isElem >>> hasName "frameRelation"
 >>> ( (getAttrValue "type" >>> arr readRT >>> unmaybeA)
   &&& (getChildren >>> isElem >>> hasName "relatedFrame"
                    >>> getAttrValue "ID" >>> readA
       )
     )

parseLU :: ArrowXml a => a XmlTree LexUnit
parseLU =
     isElem >>> hasName "lexUnit"
 >>> ( (getAttrValue "name")
   &&& (getAttrValue "ID" >>> readA)
   &&& (getChildren >>> isElem >>> hasName "lexeme"
                    >>> getAttrValue "name"
       )
     )
 >>^ \(name, (lid, lexm)) -> LU { lu_name = name
                                , lu_id   = lid
                                , lu_lex  = lexm
                                }

buildFRDico :: [FrameRelation] -> Map RelId FrameRelation
buildFRDico frs = M.fromList
                    $ map (\fr -> ((fr_type fr, fr_subid fr, fr_superid fr), fr))
                          frs

parseFRs :: IOSLA (XIOState ()) FilePath FrameRelation
parseFRs = readFromDocument [withValidate no] >>> parseFrameRelation
 
parseFrameRelation :: ArrowXml a => a XmlTree FrameRelation
parseFrameRelation = deep $
     isElem >>> hasName "frameRelationType"
 >>> ( (getAttrValue "name" >>> arr readFRT >>> unmaybeA)
   &&& (getChildren >>> isElem >>> hasName "frameRelation"
                    >>> ( (getAttrValue "supID" >>> readA)
                      &&& (getAttrValue "subID" >>> readA)
                      &&& ((getChildren >>> parseFEB) >. id)
                        )
       )
     )
 >>^ \(rt,(supid,(subid,febs))) -> FR { fr_type     = rt
                                      , fr_superid  = supid
                                      , fr_subid    = subid
                                      , fr_bindings = febs
                                      }

parseFEB :: ArrowXml a => a XmlTree FEBinding
parseFEB =
     isElem >>> hasName "FERelation"
 >>> ( (getAttrValue "subID" >>> readA)
   &&& (getAttrValue "subFEName")
   &&& (getAttrValue "supID" >>> readA)
   &&& (getAttrValue "superFEName")
     )
 >>^ \(subid, (subname, (supid, supname))) -> FEB { feb_subid   = subid
                                                  , feb_subname = subname
                                                  , feb_supid   = supid
                                                  , feb_supname = supname
                                                  }

