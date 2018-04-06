{-# LANGUAGE TupleSections #-}

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

generateBind :: Monad m => String -> Dictionnary -> RelId -> m Construction
generateBind bind_name dico rid = do
    (FR _ supid subid bindings) <- lookupRelId dico rid
    supFrame <- lookupFrame dico supid
    subFrame <- lookupFrame dico subid
    let supName = frame_name supFrame
    let subName = frame_name subFrame
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

-- Build a graph representing the links between the FEs
buildGraph :: [FEBinding] -> Map Int ((String,Bool),[Int])
buildGraph bindings = foldl (\mp (key,value)
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
 where ccs  = connected_components bindings
       mkVar :: [((String,Bool),Int)] -> Var
       mkVar (((name,_),_):_) = Var $ "fe-" <> name
       mkVar []               = error "Empty connected component"

connected_components :: [FEBinding] -> [[((String,Bool),Int)]]
connected_components bindings = fmap S.elems
                              $ filter (not . S.null)
                              $ fst
                              $ runState (sequence $ fmap (rundfs graph) nodes)
                                         (S.empty, S.empty)
 where graph = buildGraph bindings
       nodes = M.keys graph
       rundfs :: Ord a => Map Int (a,[Int]) -> Int -> State (S.Set Int, S.Set (a,Int))
                                                            (S.Set (a,Int))
       rundfs gr node = do
           dfs gr node
           (sns, ccs) <- get
           put (sns, S.empty)
           return ccs

dfs :: Ord a => Map Int (a,[Int]) -> Int -> State (S.Set Int, S.Set (a,Int)) ()
dfs graph node = do
    sn  <- seen node
    if sn
      then return ()
      else case M.lookup node graph of
        Nothing            -> fail $ "Node " <> show node <> " not in graph"
        Just (label,nexts) -> do
            insert label node
            sequence_ $ fmap (dfs graph) nexts
 where seen :: Int -> State (S.Set Int, S.Set (a,Int)) Bool
       seen nd = get >>= \s -> return $ S.member nd $ fst s
       insert :: Ord a => a -> Int -> State (S.Set Int, S.Set (a,Int)) ()
       insert label nd = do
           (sns, cc) <- get
           put (S.insert nd sns, S.insert (label,nd) cc)


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
        (name <> "-" <> show lid <> "-cxn")
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
 where unit = Var $ name <> "-" <> show lid <> "-unit"


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

