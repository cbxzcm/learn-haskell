module Chapter11.WriteFoldrForBinaryTree where

-- Given
data BinaryTree a =
    Leaf |
    Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- My implementation
-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b tree = foldr f b (preorder tree)  where
    preorder Leaf                = []
    preorder (Node left a right) = [a] ++ preorder left ++ preorder right
