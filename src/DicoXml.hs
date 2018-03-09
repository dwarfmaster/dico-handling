{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module DicoXml 
       ( LexieId, Dico
       , InfoGrammName(..), InfoGrammVerb(..), InfoGrammAdj(..), InfoGramm(..)
       , Actant(..), LexicalLink(..), Context(..), Lexie(..)
       , XmlException(..), throwXml
       , readLexie, buildDico, dicoLookup
       ) where

import           System.Directory
import           Text.XML.HXT.Core
import           Data.Monoid
import qualified Data.Map               as M
import           Data.List              (isInfixOf)
import           Data.Foldable
import           Control.Exception.Base

--  _   _ _   _ _     
-- | | | | |_(_) |___ 
-- | | | | __| | / __|
-- | |_| | |_| | \__ \
--  \___/ \__|_|_|___/
--                    
applyOnCollection :: (Monoid m) => IOSLA (XIOState ()) XmlTree b 
                                -> (FilePath -> [b] -> m) 
                                -> FilePath 
                                -> IO m
applyOnCollection filt mono dir = do
    files <- listDirectory dir
    (flip foldMap) files $ \filename -> do
        let file = dir <> filename
        l <- runX $ readDocument [ withValidate no ] file >>> filt
        return $ mono file l

-- Map from (word,acceptation) to file it is stored in
type LexieId = (String,String)
type Dico = M.Map LexieId FilePath


--  ___        __        ____                               
-- |_ _|_ __  / _| ___  / ___|_ __ __ _ _ __ ___  _ __ ___  
--  | || '_ \| |_ / _ \| |  _| '__/ _` | '_ ` _ \| '_ ` _ \ 
--  | || | | |  _| (_) | |_| | | | (_| | | | | | | | | | | |
-- |___|_| |_|_|  \___/ \____|_|  \__,_|_| |_| |_|_| |_| |_|
--                                                          
data InfoGrammName = Name
                   | Male   InfoGrammName
                   | Female InfoGrammName
                   | Plural InfoGrammName
instance Show InfoGrammName where
    show Name       = "n."
    show (Male   x) = show x <> " m."
    show (Female x) = show x <> " f."
    show (Plural x) = show x <> " pl."
read_name :: String -> InfoGrammName
read_name s = if isInfixOf "pl" s then Plural gender
                                  else gender
     where gender =      if isInfixOf "m" s then Male Name
                    else if isInfixOf "f" s then Female Name
                    else                         Name

data InfoGrammAdj = Adjective
                  | Locutif InfoGrammAdj
instance Show InfoGrammAdj where
    show Adjective   = "adj."
    show (Locutif x) = "loc. " <> show x
read_adj :: String -> InfoGrammAdj
read_adj s = if isInfixOf "loc" s then Locutif Adjective else Adjective

data InfoGrammVerb = Verb
                   | Transitive   InfoGrammVerb
                   | Intransitive InfoGrammVerb
                   | Pronominal   InfoGrammVerb
instance Show InfoGrammVerb where
    show Verb = "v."
    show (Transitive   x) = show x <> " tr."
    show (Intransitive x) = show x <> " intr."
    show (Pronominal   x) = show x <> " pron."
read_verb :: String -> InfoGrammVerb
read_verb s = if isInfixOf "pron" s then Pronominal tr else tr
 where tr =      if isInfixOf "intr" s then Intransitive Verb
            else if isInfixOf "tr"   s then Transitive Verb
            else                            Verb

data InfoGramm = Nm InfoGrammName | Adj InfoGrammAdj | Vb InfoGrammVerb
instance Show InfoGramm where
    show (Nm  x) = show x
    show (Adj x) = show x
    show (Vb  x) = show x
read_gramm :: String -> InfoGramm
read_gramm s = if isInfixOf "adj" s then Adj $ read_adj  s
          else if isInfixOf "v"   s then Vb  $ read_verb s
          else                           Nm  $ read_name s


--  _              _      
-- | |    _____  _(_) ___ 
-- | |   / _ \ \/ / |/ _ \
-- | |__|  __/>  <| |  __/
-- |_____\___/_/\_\_|\___|
--                       
data Actant = Actant
    { actant_name    :: String
    , actant_example :: String
    , actant_reals   :: [LexieId]
    }
    deriving (Show)
data LexicalLink = LexicalLink
    { ll_family      :: String
    , ll_description :: String
    , ll_function    :: String
    , ll_link        :: LexieId
    } deriving (Show)
data Context = CtxCat Context Context
             | CtxText String
             | CtxCirc String Context
             | CtxAct  String Context
             | CtxFnSyntax String Context
             | CtxGrSyntax String Context
             | CtxHead     String
             | CtxLexie    String
             | CtxEmpty -- Shouldn't happen
             deriving (Show)
data Lexie = Lexie
    { identificateur :: String
    , acception      :: String
    , grammatics     :: InfoGramm
    , definition     :: String
    , domain         :: String
    , actants        :: [Actant]
    , synonyms       :: [LexieId]
    , lexical_links  :: [LexicalLink]
    , contexts       :: [Context]
    }
    deriving (Show)
type LexieSetter a = a -> Endo Lexie
set_grammatics :: LexieSetter InfoGramm
set_definition :: LexieSetter String
set_domain     :: LexieSetter String
add_actant     :: LexieSetter Actant
add_synonym    :: LexieSetter LexieId
add_llink      :: LexieSetter LexicalLink
add_context    :: LexieSetter Context
set_grammatics g = Endo $ \l -> l { grammatics     = g }
set_definition d = Endo $ \l -> l { definition     = d }
set_domain     d = Endo $ \l -> l { domain         = d }
add_synonym    s = Endo $ \l -> l { synonyms       = s : synonyms      l }
add_llink      k = Endo $ \l -> l { lexical_links  = k : lexical_links l }
add_context    c = Endo $ \l -> l { contexts       = c : contexts      l }

comb_actant :: Actant -> Actant -> Actant
comb_actant (Actant n1 e1 r1) (Actant _ e2 r2) = Actant n1 e (r1 <> r2)
 where e = if null e1 then e2 else e1
update_actants :: Actant -> [Actant] -> [Actant]
update_actants a [] = [a]
update_actants a@(Actant n _ _) (f@(Actant n' _ _) : l) = if n == n'
                                                          then comb_actant a f : l
                                                          else f : update_actants a l
add_actant     a = Endo $ \l -> l { actants = update_actants a $ actants l }

--  _____ _ _ _                
-- |  ___(_) | |_ ___ _ __ ___ 
-- | |_  | | | __/ _ \ '__/ __|
-- |  _| | | | ||  __/ |  \__ \
-- |_|   |_|_|\__\___|_|  |___/
--                             

getLexie :: ArrowXml a => LexieId -> a XmlTree Lexie
getLexie (iden,acc) =
     inLexie (iden,acc)
 >>> ( wrap getGrammatics
   <&> wrap getDefinition
   <&> wrap getDomain
   <&> wrap getActant
   <&> wrap getSynonym
   <&> wrap getLLink
   <&> wrap getContext
     )
  >. id
 >>> isA (not . null)
 >>> arr (appEndo . fold)
 >>^ ($ defaultLexie)
 where defaultLexie = Lexie
                    { identificateur = iden
                    , acception      = acc
                    , grammatics     = undefined
                    , definition     = ""
                    , domain         = ""
                    , actants        = []
                    , synonyms       = []
                    , lexical_links  = []
                    , contexts       = []
                    }
       a <&> b = (a &&& b) >>^ uncurry mappend
       eid     = Endo id
       wrap ar = (getChildren >>> ar) `orElse` constA eid

inLexie :: ArrowXml a => LexieId -> a XmlTree XmlTree
inLexie (name,accep) = 
     isElem >>> hasName "vocable"
 >>> hasAttrValue "identificateur" (== name)
  /> isElem >>> hasName "lexie"
 >>> hasAttrValue "numero-acception" (== accep)

getGrammatics :: ArrowXml a => a XmlTree (Endo Lexie)
getGrammatics =
     isElem >>> hasName "information-grammaticale"
  /> isText >>> getText
 >>^ set_grammatics . read_gramm

getDefinition :: ArrowXml a => a XmlTree (Endo Lexie)
getDefinition =
   ( isElem >>> hasName "definition"
 //> ( ( isText >>> getText )
   <+> ( hasAttr "lemme" >>> getAttrValue "lemme" )
     )
   )
   >. id
 >>> isA (not . null)
 >>^ set_definition . mconcat

getDomain :: ArrowXml a => a XmlTree (Endo Lexie)
getDomain =
     isElem >>> hasName "domain"
  /> isText >>> getText
 >>^ set_domain

getActant :: ArrowXml a => a XmlTree (Endo Lexie)
getActant =
     ( getActantNameEx <+> getActantReal )
  >. id
 >>> isA (not . null)
 >>^ foldMap add_actant

getActantNameEx :: ArrowXml a => a XmlTree Actant
getActantNameEx =
     isElem >>> hasName "structure-actancielle"
  /> isElem >>> hasName "role"
 >>> ( getAttrValue "nom"
       &&&
       ( getChildren
     >>> isElem >>> hasName "tt"
      /> isText >>> getText
       )
     )
 >>^ \(name,example) -> Actant name example []

getActantReal :: ArrowXml a => a XmlTree Actant
getActantReal =
     isElem >>> hasName "realisations"
  /> isElem >>> hasName "role"
 >>> ( getAttrValue "nom"
       &&&
       ( getChildren >>> isElem >>> hasName "realisation"
     >>> ( getAttrValue "identificateur"
       &&& getAttrValue "numero-acception" )
       )
     )
 >>^ \(name,iden) -> Actant name "" [iden]

getSynonym :: ArrowXml a => a XmlTree (Endo Lexie)
getSynonym =
     isElem >>> hasName "synonyms"
  /> isElem >>> hasName "synonym"
 >>> (getAttrValue "identificateur" &&& getAttrValue "numero-acception")
 >>^ add_synonym

getLLink :: ArrowXml a => a XmlTree (Endo Lexie)
getLLink =
     isElem >>> hasName "liens-lexicaux"
  /> isElem >>> hasName "famille"
 >>> ( getAttrValue "nom"
       &&&
       ( getChildren
     >>> isElem >>> hasName "lien-lexical"
     >>> ( getValue "explication-ra"
       &&& getValue "explication-tt"
       &&& getValue "fonction-lexicale"
       &&& ( getChildren >>> isElem >>> hasName "lien"
         >>> (getAttrValue "identificateur" &&& getAttrValue "numero-acception")
           )
         )
       )
     )
 >>^ (\(name, (era, (ett, (alex, lid)))) -> add_llink $
                      LexicalLink name (era <> " (" <> ett <> ")") alex lid)
 where getValue :: ArrowXml a => String -> a XmlTree String
       getValue key = getChildren >>> (
                    ( isElem >>> hasName key
                   /> ( ( isText >>> getText )
                    <+> ( isElem >>> hasName "role-ref"
                      >>> ( ( hasAttr "lemme" >>> getAttrValue "lemme" )
                            `orElse`
                            ( getAttrValue "nom" )
                          )
                        )
                    <+> ( isElem >>> hasName "lexie-ref" >>> constA "~" )
                      ) )
                   >. id
                  >>> isA (not . null)
                  >>^ mconcat
                    )

getContext :: ArrowXml a => a XmlTree (Endo Lexie)
getContext = 
     ( isElem >>> hasName "contextes"
    /> isElem >>> hasName "contexte"
    /> getContexts >>> arr add_context
     )
  >. id
 >>> isA (not . null)
 >>^ fold

getAContext :: ArrowXml a => a XmlTree Context
getAContext = getCtxText
          <+> getCtxCirc
          <+> getCtxAct
          <+> getCtxFnStx
          <+> getCtxGrStx
          <+> getCtxHead
          <+> getCtxLexie

getContexts :: ArrowXml a => a XmlTree Context
getContexts = 
     getAContext
  >. id
 >>> isA (not . null)
 >>^ cat
 where cat :: [Context] -> Context
       cat []     = CtxEmpty
       cat [x]    = x
       cat (x:xs) = CtxCat x $ cat xs

getCtxText :: ArrowXml a => a XmlTree Context
getCtxText = isText >>> getText 
         >>> isA (not . (all (`elem` "\n\t ")))
         >>> arr CtxText

getCtxCirc :: ArrowXml a => a XmlTree Context
getCtxCirc =
     isElem >>> hasName "participant"
 >>> hasAttrValue "type" (== "Circ")
 >>> (getAttrValue "role" &&& (getChildren >>> getContexts))
 >>^ uncurry CtxCirc

getCtxAct :: ArrowXml a => a XmlTree Context
getCtxAct =
     isElem >>> hasName "participant"
 >>> hasAttrValue "type" (== "Act")
 >>> (getAttrValue "role" &&& (getChildren >>> getContexts))
 >>^ uncurry CtxAct

getCtxFnStx :: ArrowXml a => a XmlTree Context
getCtxFnStx =
     isElem >>> hasName "fonction-syntaxique"
 >>> (getAttrValue "nom" &&& (getChildren >>> getContexts))
 >>^ uncurry CtxFnSyntax

getCtxGrStx :: ArrowXml a => a XmlTree Context
getCtxGrStx =
     isElem >>> hasName "groupe-syntaxique"
 >>> (getAttrValue "nom" &&& (getChildren >>> getContexts))
 >>^ uncurry CtxGrSyntax

getCtxHead :: ArrowXml a => a XmlTree Context
getCtxHead =
     isElem >>> hasName "realisation"
  /> isText >>> getText
 >>^ CtxHead

getCtxLexie :: ArrowXml a => a XmlTree Context
getCtxLexie =
     isElem >>> hasName "lexie-att"
  /> isText >>> getText
 >>^ CtxLexie

--  ____                _               
-- |  _ \ ___  __ _  __| | ___ _ __ ___ 
-- | |_) / _ \/ _` |/ _` |/ _ \ '__/ __|
-- |  _ <  __/ (_| | (_| |  __/ |  \__ \
-- |_| \_\___|\__,_|\__,_|\___|_|  |___/
--         

data XmlException = XmlException String deriving (Show)
instance Exception XmlException
throwXml :: String -> IO a
throwXml msg = throwIO $ XmlException msg

readLexie :: LexieId -> FilePath -> IO Lexie
readLexie lid path = do
    l <- runX $ readDocument [ withValidate no ] path
             /> getLexie lid
    case l of
     []  -> throwXml $ "Lexie " <> show lid <> " not present in file " <> path
     x:_ -> return x


--  ____  _           
-- |  _ \(_) ___ ___  
-- | | | | |/ __/ _ \ 
-- | |_| | | (_| (_) |
-- |____/|_|\___\___/ 
--                    

findWordAndAcceptations :: ArrowXml a => a XmlTree (String,String)
findWordAndAcceptations = deep
    $ isElem 
  >>> hasName "vocable"
  >>> ( ( getAttrValue "identificateur"
        )
        &&& 
        ( getChildren
      >>> isElem
      >>> hasName "lexie"
      >>> getAttrValue "numero-acception"
        )
      )

buildDico :: FilePath -> IO Dico
buildDico = applyOnCollection findWordAndAcceptations
                            $ \file -> M.fromList . fmap (,file)

dicoLookup :: Dico -> LexieId -> IO Lexie
dicoLookup dico lid = case M.lookup lid dico of
                       Just path -> readLexie lid path
                       Nothing   -> throwXml $ "Lexie " <> show lid <> " not present"

