{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module FCG where

import           Data.Monoid
import           Data.String
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy         as T

data LispExpr = LispLst   LispExpr [LispExpr]
              | LispQuote LispExpr
              | LispVar   String
              deriving (Show)

depth :: LispExpr -> Integer
depth (LispLst x []) = depth x
depth (LispLst x l)  = (+) 1 $ max (depth x) $ maximum $ map depth l
depth (LispQuote l)  = depth l
depth (LispVar _)    = 0

print_lisp :: LispExpr -> T.Text
print_lisp = TB.toLazyText . (print_lisp' 0)

print_lisp' :: Int -> LispExpr -> TB.Builder
print_lisp' sft (LispLst name [])     = "(" <> print_lisp' sft name <> ")"
print_lisp' sft l@(LispLst name args) = "("
                                <> foldr (\(s,arg) acc -> print_lisp' s arg
                                                       <> sep
                                                       <> acc)
                                         ""
                                         ( (sft,name) : map (nsft,) args )
                                <> ")"
 where sep   = if depth l <= 1 then " " else ("\n" <> shift)
       shift = mconcat $ take sft $ repeat ("  " :: TB.Builder)
       nsft  = sft + 1
print_lisp' sft (LispQuote expr)      = "'" <> print_lisp' sft expr
print_lisp' _   (LispVar str)         = TB.fromString str

class GenLisp a where
    toLisp :: a -> LispExpr
instance GenLisp LispExpr where
    toLisp = id
instance IsString LispExpr where
    fromString = LispVar

data AnyShow where
    AnyShow :: Show a => a -> AnyShow
deriving instance Show AnyShow
instance GenLisp AnyShow where
    toLisp (AnyShow x) = LispVar $ show x

data AnyLisp where
    AnyLisp :: GenLisp l => l -> AnyLisp
instance GenLisp AnyLisp where
    toLisp (AnyLisp l) = toLisp l

data Predicate where
    Predicate :: String -> [AnyLisp] -> Predicate
instance GenLisp Predicate where
    toLisp (Predicate name args) = LispLst (LispVar name) $ map toLisp args

data Var = Var String
         deriving (Show)
instance GenLisp Var where
    toLisp (Var name) = LispVar $ "?" <> name

-- list argument is assumed not empty
data SetOfPredicate = SetOfPred [Predicate]
instance GenLisp SetOfPredicate where
    toLisp (SetOfPred (ph:ps)) = LispLst (toLisp ph) $ map toLisp ps
    toLisp (SetOfPred [])      = error "Empty set of predicates"

data Hform = Hstring Var String | Hmeets String String
instance GenLisp Hform where
    toLisp (Hstring var str) = LispLst (LispVar "HASH")
                                     [ LispVar "form"
                                     , LispLst (LispLst
                                           (LispVar "string")
                                           [ toLisp var
                                           , LispVar $ "\"" <> str <> "\""
                                           ]
                                         ) []
                                     ]
    toLisp (Hmeets str1 str2) = LispLst (LispVar "HASH")
                                     [ LispVar "form"
                                     , LispLst (LispLst
                                           (LispVar "meets")
                                           [ LispVar $ "\"" <> str1 <> "\""
                                           , LispVar $ "\"" <> str2 <> "\""
                                           ]
                                         ) []
                                     ]

data Sequence where
    Seq :: GenLisp l => [l] -> Sequence
instance GenLisp Sequence where
    toLisp (Seq [])    = error "Empty sequence"
    toLisp (Seq (h:t)) = LispLst (toLisp h) $ map toLisp t

data Hmeaning = Hmeaning SetOfPredicate
instance GenLisp Hmeaning where
    toLisp (Hmeaning set) = LispLst (LispVar "HASH")
                                    [ LispVar "meaning"
                                    , toLisp set
                                    ]

data UnitEntry where
    UnitE :: GenLisp l => String -> l -> UnitEntry
    Hashm :: Hmeaning -> UnitEntry
    Hashf :: Hform    -> UnitEntry
instance GenLisp UnitEntry where
    toLisp (UnitE name arg) = LispLst (LispVar name) [toLisp arg]
    toLisp (Hashm m)        = toLisp m
    toLisp (Hashf f)        = toLisp f

data TransientUnit = TUnit Var [UnitEntry]
instance GenLisp TransientUnit where
    toLisp (TUnit var entries) = LispLst (toLisp var) $ map toLisp entries

data LockUnit = LUnit Var [UnitEntry] [UnitEntry]
instance GenLisp LockUnit where
    toLisp (LUnit var prod syntax) = LispLst (toLisp var)
                                           $ map toLisp prod
                                          <> [LispVar "--"]
                                          <> map toLisp syntax

data Construction = Construction String [TransientUnit] [LockUnit]
instance GenLisp Construction where
    toLisp (Construction name (tu:tunits) lunits) =
        LispLst (LispVar "def-fcg-cxn")
                [ LispVar name
                , LispLst
                      (toLisp tu)
                    $ map toLisp tunits
                   <> [LispVar "<-"]
                   <> map toLisp lunits
                ]
    toLisp (Construction _ [] _) = error "Empty transient unit list in context"

data Grammar = Grammar String [Construction]
instance GenLisp Grammar where
    toLisp (Grammar name cxns) =
        LispLst "def-fcg-constructions"
              $ (LispVar name) : map toLisp cxns

predarg :: String -> [AnyLisp]
predarg = return . AnyLisp . LispVar

