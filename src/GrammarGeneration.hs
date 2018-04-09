{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module GrammarGeneration 
       ( generateRelationsScheme, generateLUs
       , generateGrammar, loadGrammarFromFramenet
       , writeGrammar, framenetToFCG
       ) where

import           FCG
import           FrameNet
import           Data.Monoid         ((<>))
import qualified Data.Map            as M
import           Data.Map            (Map)
import           Data.List           (union)
import qualified Data.Set            as S
import           Control.Monad.State
import qualified Data.Text.Lazy.IO   as TIO
import           Data.Char           (isSpace)


--   ____                 _     
--  / ___|_ __ __ _ _ __ | |__  
-- | |  _| '__/ _` | '_ \| '_ \ 
-- | |_| | | | (_| | |_) | | | |
--  \____|_|  \__,_| .__/|_| |_|
--                 |_|          

class (Ord id, Show id) => Graph g id node | g -> id node where
    glookup :: g -> id   -> Maybe node
    gnexts  :: g -> node -> [id]
    gnodes  :: g -> [id]

dfs :: forall g id node m. (Graph g id node, Monoid m)
    => g
    -> (id -> node -> m)
    -> id
    -> State (S.Set id, m) ()
dfs graph f nodeid = do
    sn <- seen nodeid
    if sn
      then return ()
      else case glookup graph nodeid of
        Nothing  -> fail $ "Node " <> show nodeid <> " not in graph"
        Just val -> do
            insert val nodeid
            sequence_ $ fmap (dfs graph f) $ gnexts graph val
 where seen :: id -> State (S.Set id, m) Bool
       seen nd = get >>= \s -> return $ S.member nd $ fst s
       insert :: node -> id -> State (S.Set id, m) ()
       insert label nd = do
           (sns, cc) <- get
           put (S.insert nd sns, (f nd label) <> cc)

connectedFrom :: forall g id node a
               . (Graph g id node, Ord a)
              => g
              -> (id -> node -> a)
              -> [id]
              -> [S.Set a]
connectedFrom graph f start = filter (not . S.null)
                            $ fst
                            $ (flip runState) (S.empty, S.empty)
                            $ sequence
                            $ fmap runDfs start
 where makeMonoid :: id -> node -> S.Set a
       makeMonoid ident = S.singleton . (f ident)
       runDfs :: id -> State (S.Set id, S.Set a) (S.Set a)
       runDfs ident = do
           dfs graph makeMonoid ident
           (sns, cc) <- get
           put (sns, S.empty)
           return cc

connected :: (Graph g id node, Ord a)
          => g
          -> (id -> node -> a)
          -> [S.Set a]
connected graph f = connectedFrom graph f $ gnodes graph

--  ____      _       _   _                 
-- |  _ \ ___| | __ _| |_(_) ___  _ __  ___ 
-- | |_) / _ \ |/ _` | __| |/ _ \| '_ \/ __|
-- |  _ <  __/ | (_| | |_| | (_) | | | \__ \
-- |_| \_\___|_|\__,_|\__|_|\___/|_| |_|___/
--                                          

generateRelationsScheme :: Monad m => Dictionnary -> m [Construction]
generateRelationsScheme dico@(Dico _ rels) =
    liftM mconcat
  $ sequence
  $ fmap (chooseBind dico)
  $ M.keys rels

chooseBind :: Monad m => Dictionnary -> RelId -> m [Construction]
chooseBind dico rid = do
    (FR reltype _ _ _) <- lookupRelId dico rid
    case reltype of
     FRT_Inheritance   -> generateBind "inherits"    dico rid >>= (return . (:[]))
     FRT_Subframe      -> generateBind "subframe"    dico rid >>= (return . (:[]))
     FRT_Using         -> generateBind "using"       dico rid >>= (return . (:[]))
     FRT_InchoativeOf  -> generateBind "inchoative"  dico rid >>= (return . (:[]))
     FRT_CausativeOf   -> generateBind "causative"   dico rid >>= (return . (:[]))
     FRT_Precedes      -> generateBind "precedes"    dico rid >>= (return . (:[]))
     FRT_PerspectiveOn -> generateBind "perspective" dico rid >>= (return . (:[]))
     _                 -> return []

unSpace :: String -> String
unSpace = map $ \c ->      if isSpace c then '_'
                      else if c == '('  then '-'
                      else if c == ')'  then '-'
                      else if c == '\'' then '-'
                      else                   c

generateBind :: Monad m => String -> Dictionnary -> RelId -> m Construction
generateBind bind_name dico rid = do
    (FR _ supid subid bindings) <- lookupRelId dico rid
    supFrame <- lookupFrame dico supid
    subFrame <- lookupFrame dico subid
    let supName = unSpace $ frame_name supFrame
    let subName = unSpace $ frame_name subFrame
    let supFRVar = Var $ "fr-" <> supName
    let subFRVar = Var $ "fr-" <> subName
    let unitName = Var $ subName <> "-unit"
    let unitVar  = Var "unit"
    return $ Construction
        (subName <> "-" <> bind_name <> "-" <> supName <> "-cxn")
        [ TUnit unitName
                [ UnitE "meaning" $ SetOfPred  $ [
                    Predicate "frame" [ AnyLisp supFRVar
                                      , AnyLisp $ AnyShow supName
                                      , AnyLisp unitVar
                                      ]
                    ] <> fmap (\(_,lbl,sub,var) ->
                                Predicate "fe" [ AnyLisp $ if sub then subFRVar
                                                                  else supFRVar
                                               , AnyLisp $ AnyShow lbl
                                               , AnyLisp var
                                               ]
                             )
                             (bind bindings)
                ]
        ]
        (let lock = UnitE "meaning" $ SetOfPred [
                     Predicate "frame" [ AnyLisp subFRVar
                                       , AnyLisp $ AnyShow subName
                                       , AnyLisp unitVar
                                       ]
                   ]
        in [ LUnit unitName [ lock ] [ lock ] ])

newtype BindGraph = BG { unBG :: Map Int ((String,Bool),[Int]) }
instance Graph BindGraph Int ((String,Bool),[Int]) where
    glookup gr = flip M.lookup (unBG gr)
    gnexts     = const $ snd
    gnodes     = M.keys . unBG

-- Build a graph representing the links between the FEs
buildGraph :: [FEBinding] -> BindGraph
buildGraph bindings = BG
                    $ foldl (\mp (key,value)
                                 -> M.insertWith (munion) key value mp)
                            M.empty
                          $ mconcat
                            $ fmap (\feb -> [ ( feb_subid feb
                                              , ((feb_subname feb,True)
                                                , [feb_supid feb]))
                                            , ( feb_supid feb
                                              , ((feb_supname feb,False)
                                                , [feb_subid feb]))
                                            ]
                                   ) bindings
 where munion (s,l1) (_,l2) = (s, union l1 l2)

-- Associates a variable with each of the FE to bind
bind :: [FEBinding] -> [(Int,String,Bool,Var)]
bind bindings = mconcat
              $ fmap (\l -> let v = mkVar l
                            in fmap (\((s,b),i) -> (i,s,b,v))
                                    l
                     ) ccs
 where ccs = fmap S.elems
           $ connected (buildGraph bindings)
           $ \ident (value,_) -> (value,ident)
       mkVar :: [((String,Bool),Int)] -> Var
       mkVar (((name,_),_):_) = Var $ "fe-" <> name
       mkVar []               = error "Empty connected component"

--  _              _           _   _   _       _ _       
-- | |    _____  _(_) ___ __ _| | | | | |_ __ (_) |_ ___ 
-- | |   / _ \ \/ / |/ __/ _` | | | | | | '_ \| | __/ __|
-- | |__|  __/>  <| | (_| (_| | | | |_| | | | | | |_\__ \
-- |_____\___/_/\_\_|\___\__,_|_|  \___/|_| |_|_|\__|___/
--                                                       

generateLUs :: Monad m => Dictionnary -> m [Construction]
generateLUs (Dico frames _) = return
                            $ fmap (uncurry generateFrameLexUnit)
                            $ mconcat
                            $ fmap (\fr -> fmap (frame_name fr,) $ frame_lus fr)
                            $ M.elems
                            $ frames

generateFrameLexUnit :: String -> LexUnit -> Construction
generateFrameLexUnit frame (LU name lid lexeme) =
    Construction
        (uname <> "-" <> show lid <> "-cxn")
        [ TUnit unit [] ]
        [ LUnit unit
          [ Hashm $ Hmeaning $ SetOfPred [
              Predicate "frame" [ AnyLisp $ Var $ "fr-" <> frame
                                , AnyLisp $ AnyShow frame
                                , AnyLisp unit
                                ]
            ]
          ]
          [ Hashf $ Hstring unit lexeme ]
        ]
 where unit  = Var $ uname <> "-" <> show lid <> "-unit"
       uname = unSpace name


--   ____                           _ 
--  / ___| ___ _ __   ___ _ __ __ _| |
-- | |  _ / _ \ '_ \ / _ \ '__/ _` | |
-- | |_| |  __/ | | |  __/ | | (_| | |
--  \____|\___|_| |_|\___|_|  \__,_|_|
--                                    

generateGrammar :: Monad m => Dictionnary -> m Grammar
generateGrammar dico = Grammar "framedico"
                   <$> (mappend
                       <$> (generateRelationsScheme dico)
                       <*> (generateLUs             dico))
                   

loadGrammarFromFramenet :: FilePath -> IO Grammar
loadGrammarFromFramenet = framenetDictionnary >=> generateGrammar

writeGrammar :: FilePath -> Grammar -> IO ()
writeGrammar path grammar = TIO.writeFile path
                          $ print_lisp
                          $ toLisp grammar

framenetToFCG :: FilePath -> FilePath -> IO ()
framenetToFCG framenetdir out = loadGrammarFromFramenet framenetdir
                            >>= writeGrammar out

