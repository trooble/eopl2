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

lset lst n e =
    let lsp [] _ = []
        lsp (y:ys) p
             | p == n = e : ys
             | otherwise = y : lsp ys (p + 1)
    in
        lsp lst 0

e_product :: [a] -> [a] -> [[a]]
e_product [] _ = []
e_product (x:xs) y =
    (scalar_product x y) ++ (e_product xs y)

scalar_product s [] = []
scalar_product s (x:xs) =
    [s,x] : scalar_product s xs

my_reverse lst =
    let putpair [] outl = outl
        putpair (x:xs) outl =
            putpair xs (x:outl)
    in
        putpair lst []

my_append l1 l2 =
    let mya [] l2 = l2
        mya l1 [] = l1
        mya (x:xs) l2 =
            x : mya xs l2
    in
        mya l1 l2
