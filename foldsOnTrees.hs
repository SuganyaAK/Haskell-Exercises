data Tree a = Empty
            |  Node (Tree a) a (Tree a)
 deriving (Show,Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

numEmpty :: Tree a ->Int
numEmpty Empty = 1
numEmpty (Node l _ r) = numEmpty l + numEmpty r

treeSize' :: Tree a ->Int
treeSize' x = numEmpty x - 1

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = [x] ++ flatten l ++ flatten r

treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r 

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

foldTree :: b -> (b->a->b->b) -> Tree a -> b 
foldTree z f Empty = z
foldTree z f (Node l x r) = let leftFold = foldTree z f l 
                                rightFold = foldTree z f r
                            in f leftFold x rightFold

treeSumFold :: Tree Int -> Int
treeSumFold tree = foldTree 0 (\leftTree x rightTree ->leftTree +x+ rightTree) tree

flattenFold :: Tree a ->[a]
flattenFold tree = foldTree [] (\leftTree x rightTree -> leftTree ++ [x] ++ rightTree )tree

treeDepthFold :: Tree a -> Int 
treeDepthFold tree = foldTree 0 (\leftTree _ rightTree -> 1 + max (leftTree) (rightTree)) tree 