module DOT where

import Data.Tree
import Data.IORef
import Debug.Trace
import DecisionTree
import Text.Printf
 
toDiGraph :: (Show a) => Tree (Decision a) -> String
toDiGraph graph = "digraph TrainedDecisionTree {\n" ++
                  (dfsShow Nothing graph) ++ -- the raw edge connection data
                  (formatNodes graph)
                  ++ "\n}"


visit :: AttributeGroup a -> Attribute a -> AttributeGroup a -> String
visit parent edge child = "\t" ++(getId parent) ++ "->" ++ (getId child) ++  " [label = \"" ++  (getLabel edge) ++  "\"];\n"
                          where
                            getId (AttributeGroup groupId name _) = name ++ (show groupId)
                            getLabel (AttributeClassification _ label _ ) = label

visitAnswer :: AttributeGroup a -> Attribute a -> StopCondition -> String
visitAnswer parent edge answer = "\t" ++(getId parent) ++ "->" ++ (getAnswer answer) ++  " [label = \"" ++  (getLabel edge) ++  "\"];\n"
                                 ++  formatNode answer
                          where
                            getId (AttributeGroup groupId name _) = name ++ (show groupId)
                            getLabel (AttributeClassification _ label _ ) = label
                            getAnswer Yes = "Yes" ++ (getId parent)
                            getAnswer No  = "No"  ++ (getId parent)
                            getAnswer (CantSay d)  = "CantSay" ++ (getId parent)
                            formatNode No = printf "\t%s [label=\"%s\", style=\"filled\", color=\"red\", shape=\"box\"];\n" (getAnswer answer) "No" 
                            formatNode Yes = printf "\t%s [label=\"%s\", style=\"filled\", color=\"green\", shape=\"box\"];\n" ((getAnswer answer)) "Yes" 
                            formatNode c@(CantSay d)
                              | d >= 0.6  = printf "\t%s [label=\"%s\\n(fic=%.2f%%)\", style=\"filled\", color=\"yellow\", shape=\"box\"];\n" (getAnswer answer) "Yes" (d*100)
                              | otherwise  = printf "\t%s [label=\"%s\\n(fic=%.2f%%)\", style=\"filled\", color=\"yellow\", shape=\"box\"];\n" (getAnswer answer) "No" (d*100)


formatNodes :: (Show a) => Tree (Decision a) -> String
formatNodes tree = concat $ map (\x -> formatNode x) nodes
                   where
                     nodes = flatten tree
                     formatNode (Decision _ _ (Just (AttributeGroup groupId name _)) examples _) = printf "\t%s [label=\"%s\\n(%d samples, %.2f%%)\"];\n" (name ++ (show groupId)) name (length examples) (((fromIntegral $ (length examples))/totalNumberOfSamples)*100)
                     formatNode x@(Decision _ _ _ _ _) = "" -- trace ("fell through: " ++ (show x))  $ ""


                     totalNumberOfSamples = let (Decision _ _ _ examples _) = head nodes in fromIntegral (length examples) :: Double


--
dfsShow :: Maybe (AttributeGroup a) -> Tree (Decision a) -> String
dfsShow (Just parent) (Node (Decision edge _ _ _ (Just Yes)) _) = visitAnswer parent edge Yes
dfsShow (Just parent) (Node (Decision edge _ _ _ (Just No)) _) = visitAnswer parent edge No
dfsShow (Just parent) (Node (Decision edge _ Nothing _ (Just (CantSay d))) _) = visitAnswer parent edge (CantSay d)

dfsShow _ (Node (Decision _ _ _ _ _) []) = ""
dfsShow Nothing (Node (Decision NoAttribute _ node _ _) leaves) = concat $ map (\leaf -> dfsShow node leaf) leaves 
dfsShow (Just parent) n@(Node (Decision edge Nothing (Just node) _ _) leaves) = (visit parent edge node)  ++ (concat $ map (\leaf -> dfsShow (Just node) leaf) leaves)

