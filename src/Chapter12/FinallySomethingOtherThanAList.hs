module Chapter12.FinallySomethingOtherThanAList where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f b = case f b of
  Nothing -> Leaf
  Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild maxDepth =
  unfold
    ( \currentDepth ->
        if currentDepth >= maxDepth
          then Nothing
          else Just (currentDepth + 1, currentDepth, currentDepth + 1)
    )
    0
