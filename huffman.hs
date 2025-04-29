import Heap

data HTree = Leaf Char Int | Node HTree HTree Int

instance Eq HTree where
    (Leaf _ w1) == (Leaf _ w2) = w1 == w2
    (Node _ _ w1) == (Node _ _ w2) = w1 == w2
    _ == _ = False

instance Ord HTree where
    (Leaf _ w1) <= (Leaf _ w2) = w1 <= w2
    (Node _ _ w1) >= (Node _ _ w2) = w1 >= w2
    (Leaf _ w1) < (Leaf _ w2) = w1 < w2
    (Node _ _ w1) > (Node _ _ w2) = w1 > w2

