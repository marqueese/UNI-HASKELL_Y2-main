doublePositives :: [Int] -> [Int]
doublePositives xs = [x * 2 | x <- xs, x > 0]

doublePositives [1, -3, -2, 4, 1, 9]