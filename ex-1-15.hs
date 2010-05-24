-- duple n x = if n == 0 then [] else x : duple (n - 1) x
duple 0 _ = []
duple n x = x : duple (n - 1) x

invert [] = []
invert (x:xs) = reverse x : invert xs

filter_in _ [] = []
filter_in pred (x:xs)
          | pred x = x : filter_in pred xs
          | otherwise = filter_in pred xs

every _ [] = True
every pred (x:xs)
  | pred x = every pred xs
  | otherwise = False

exists _ [] = False
exists pred (x:xs)
  | pred x = True
  | otherwise = exists pred xs
