mySucc :: Int -> Int
mySucc a = a + 1

myIsNeg :: Int -> Bool
myIsNeg a =
    if a >= 0 then False else True

myAbs :: Int -> Int
myAbs a =
    if a >= 0 then a else -a

myMin :: Int -> Int -> Int
myMin a b =
    if a < b then a else b

myMax :: Int -> Int -> Int
myMax a b =
    if a < b then b else a

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, _) = a

mySnd :: (a, b) -> b
mySnd (_, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "head: empty list"
myHead (a:_) = a

myTail :: [a] -> [a]
myTail [] = error "tail: empty list"
myTail (_:a) = a

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] _ = error "Index too large" 
myNth _ n | n < 0 = error "Index negative"
myNth (x:_) 0 = x
myNth (_:xs) n = myNth xs (n - 1)

myTake :: Int -> [a] -> [a]
myTake _  [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop _  [] = []
myDrop 1 (_:xs) = xs
myDrop n (_:xs) = myDrop (n - 1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit (x:xs) = myReverse (myDrop 1 (myReverse (x:xs)))

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys