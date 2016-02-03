
data Zipper a = Zipper [a] a [a]
    deriving (Eq, Show)



zipper (x:xs) = Zipper [] x xs

focus :: Zipper a -> a
focus (Zipper _ x _) = x

right, left :: Zipper a -> Maybe (Zipper a)
right (Zipper av x (b:bs)) = Just $ Zipper (x:av) b bs
right (Zipper _ _ []) = Nothing
left (Zipper (a:as) x bv) = Just $ Zipper as a (x:bv)
left (Zipper [] _ _) = Nothing
