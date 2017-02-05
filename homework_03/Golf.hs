

module Golf where


skips :: [a] -> [[a]]
skips [] = []
skips xs = 
    let nats = [1..(length xs)]
        f = \x -> (map ((xs!!) . \z -> z - 1) (filter (\y -> y `mod` x == 0) nats))
    in map f nats
