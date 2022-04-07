import Graphics.Win32 (vK_DIVIDE, vK_ACCEPT)
{- When  you  create  the  closure, you  just  mark  what  variables  are  in  scope-}
{- You  don't  have  to  store  a  function  to  tell  what's  in  scope -}
{- Haskell exercise next week -}

--create a type for a binary tree
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show, Eq)

--load an then
    --let t = InnerNode 5 (Inner Node (Leaf 3) Empty 3) (Inner Node (Leaf 10) Empty 2)

--create insert that adds an element to a binary tree, assume the tree in "inorder" sorted

insert v Empty = Leaf v
insert v (Leaf a)
    | v < a = InnerNode a (Leaf v) Empty
    | otherwise = InnerNode a Empty (Leaf v)

insert v (InnerNode a l r)
    | v < a = InnerNode a (insert v l) r
    | otherwise = InnerNode a l (insert v r)

--do an inorder traversal of the tree and place all the values into a list
inorder2list Empty = []
inorder2list (Leaf a) = [a]
inorder2list (InnerNode a l r) = inorder2list l ++ (a : inorder2list r)

--do a post-order traversal of the tree and place all the values into a list
--right child, left child, node
postorder2list Empty = []
postorder2list (Leaf a) = [a]
postorder2list (InnerNode a l r) = postorder2list l ++ postorder2list r ++ [a]

--write a function to add an element to the right most branch of the tree
insertright v Empty = Leaf v
insertright v (Leaf a) = InnerNode a Empty (Leaf v)
insertright v (InnerNode a l r) = InnerNode a l (insertright v r)

--write a function to calculate the height of a tree
height Empty = 0
height (Leaf a) = 1
height (InnerNode a l r) = max (1 + height l) (1 + height r)

--write a function to add an element to the branch with the least height
addbalanced v Empty = Leaf v
addbalanced v (Leaf a) = InnerNode a Empty (Leaf v)
addbalanced v (InnerNode a l r) 
    | height l < height r = InnerNode a (addbalanced v l) r
    | otherwise           = InnerNode a l (addbalanced v r)

{-
    foldinorder: a "higher order function" that takes an input a function, an intial value, and a tree. It calculates the function every node of the tree, in order, we 
    will use the current node as the left operand and the current value as the right operand
-}
foldinorder f v Empty = v
foldinorder f v (Leaf a) = f a v
foldinorder f v (InnerNode a l r) = foldinorder f (f a (foldinorder f v l)) r 

--apply2tree takes a function and a tree and applies the function to all elements of the tree
apply2tree f Empty = Empty
apply2tree f (Leaf a) = Leaf (f a)
apply2tree ...