-- 1) Find the last element of a list
last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last xs

-- 2) Find the last element of a list
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs

-- 3) Find the K'th element of a list. The first element in the list is number 1.
elem' :: [a] -> Int -> a
elem' (x:xs) 1 = x
elem' (x:xs) k = elem' xs (k-1)

-- 4) Find the number of elements of a list.
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- 5) Reverse a list.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = snd (reverseAcc xs [])
    where
        reverseAcc :: [a] -> [a] -> ([a], [a])
        reverseAcc [] ys = ([], ys)
        reverseAcc (x:xss) ys = reverseAcc xss (x:ys)

-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (a:[]) = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

-- 7) Flatten a nested list structure.
flatten' :: [[a]] -> [a]
flatten' ys = snd (flattenAcc ys [])
    where 
        flattenAcc :: [[a]] -> [a] -> ([[a]], [a])
        flattenAcc [] acc = ([], acc)
        flattenAcc (xs:xss) acc = flattenAcc xss (acc ++ xs)

-- 8) Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress as = reverse' (snd (compressAcc as []))
    where
        compressAcc :: Eq a => [a] -> [a] -> ([a], [a])
        compressAcc [] acc = ([], acc)
        compressAcc (x:xs) [] = compressAcc xs (x:)
        compressAcc (x:xs) acc@(y:ys) = if x == y then compressAcc xs acc else compressAcc xs (x:acc)

-- 9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

















































