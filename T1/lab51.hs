data Alfabet = A | B | C deriving (Show, Eq)

data Trie = Nil
            | Node {
                var :: Bool,
                a :: Trie,
                b :: Trie,
                c :: Trie
            } deriving (Eq, Show)


contains :: Trie -> [Alfabet] -> Bool
contains Nil _ = False
contains  t@(Node v a b c) [] = v
contains t@(Node v a b c) (x:xs)
    | x == A && a /= Nil && var a = contains a xs
    | x == B && b /= Nil && var b = contains b xs
    | x == C && c /= Nil && var c = contains c xs
    | otherwise = False

test = Node False (Node True ((Node True Nil Nil Nil)) Nil Nil) (Node True Nil Nil Nil) (Node True Nil Nil Nil)
