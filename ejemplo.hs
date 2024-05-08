doble :: Int -> Int
doble n = 2 * n

tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar n (x:xs) = x : tomar (n-1) xs