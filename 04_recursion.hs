{-# OPTIONS -Wall -Werror #-}

-- maximum
-- 単一要素のとき -> その要素
-- それ以上のとき -> リストの先頭要素と残りの最大値とで比べる
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

