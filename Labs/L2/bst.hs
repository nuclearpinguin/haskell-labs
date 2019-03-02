module BST (
    Bst, contains, add, search, 
    leaves, remove, nodeType, t) 
    where 

import Data.List

data Bst a = Nil 
            | Node a (Bst a) (Bst a)
            deriving (Show, Eq)


contains :: (Ord a) => Bst a -> a -> Bool
contains Nil _ = False
contains (Node x left right) y
    | x == y = True
    | x < y = contains right y
    | x > y = contains left y


addHelp :: (Ord a) => Bst a -> a -> (Bst a, Bool)
addHelp Nil x = (Node x Nil Nil, True)
addHelp n@(Node x left right) y 
    | x == y = (n, False)
    | y < x = (Node x nl right, sl)
    | y > x = (Node x left nr, sr)
    where 
        (nl, sl) = addHelp left y
        (nr, sr) = addHelp right y


add :: (Ord a) => Bst a -> a -> Bst a
add t = fst . addHelp t


-- this is probably being executed when called
-- so lazy eval magic does not work and
-- (contains test 4) kills the ghci
-- test = foldl add Nil [1,4 ..]


-- Returns a node if it exist in tree
search :: (Ord a) => Bst a -> a -> Bst a
search Nil _ = Nil
search n@(Node x left right) y
    | x == y = n
    | y > x = search right y
    | y < x = search left y


-- Returns list of node leaves
leaves :: (Ord a) => Bst a -> [a]
leaves Nil = []
leaves (Node x Nil Nil) = [x]
leaves (Node _ left right) = (leaves left) ++ (leaves right)


-- Use only to remove node which is a leaf ! !
removeLeaf :: (Ord a) => Bst a -> a -> Bst a
removeLeaf Nil _ = Nil
removeLeaf n@(Node x left right) y 
    | y < x = Node x (removeLeaf left y) right
    | y > x = Node x left (removeLeaf right y)
    | y == x = Nil


-- Use only to remove node with on subtree ! !
removeConnector :: (Ord a) => Bst a -> a -> Bst a
removeConnector Nil _ = Nil
removeConnector n@(Node x left right) y 
    | y < x = Node x (removeConnector left y) right
    | y > x = Node x left (removeConnector right y)
    | y == x && (left /= Nil) = left
    | y == x && (right /= Nil) = right


-- Use only to remove node with both subtrees
removeMagic :: (Ord a) => Bst a -> a -> Bst a
removeMagic Nil _ = Nil
removeMagic n@(Node x left right) y 
    | y < x = Node x (removeMagic left y) right
    | y > x = Node x left (removeMagic right y)
    | y == x = Node succ left nr
    where
        succ = head $ sort $ leaves right
        nr = removeLeaf right succ

-- Resolves node type
nodeType :: (Ord a) => Bst a -> String
nodeType Nil = "Empty"
nodeType (Node _ Nil Nil) = "leaf"
nodeType (Node _ Nil _ ) = "connector"
nodeType (Node _ _ Nil) = "connector"
nodeType (Node _ _ _) = "hardone"


-- Removes a node from bst tree
remove :: (Ord a) => Bst a -> a -> Bst a
remove Nil _ = Nil
remove n y 
    | t == "leaf" = removeLeaf n y
    | t == "connector" = removeConnector n y
    | t == "hardone" = removeMagic n y
    | otherwise = n
    where
        t = nodeType $ search n y


-- sample tree
t = Node 4 (Node 2 (Node 1 Nil Nil) Nil) (Node 10 (Node 7 Nil Nil) (Node 12 (Node 11 Nil Nil) (Node 13 Nil Nil)))
