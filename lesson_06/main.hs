main = print ""

customRepeat x n = take n (cycle [x])

subseq start end list = drop start (take end list)

inFirstHalf n list = n `elem` take (div (length list) 2) list
