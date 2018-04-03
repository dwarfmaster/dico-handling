
module FrameNet where

import           System.Directory
import           Text.XML.HXT.Core
import           Data.Char         (toLower)
import           Data.Map          (Map)
import qualified Data.Map          as M

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

