strToMatrix str = strToMatrix1 str [] 0 [] 2

strToMatrix1 :: String ->[[Int]]-> Int-> [Int] -> Int -> [[Int]] 
strToMatrix1 str result tempInt tempArray i =
    if i <  (length(str)-1)
        then
            if str!!i == '['
                then 
                    strToMatrix1 str (result ++ [tempArray]) tempInt [] (i+1)
                else 
                    if str!!i == ',' && str!!(i-1) == ']'
                        then
                            strToMatrix1 str result 0 (tempArray++[tempInt]) (i+1)
                        else 
                            if str!!i /= ',' && str!!i /= ']' && str!!i /= '['
                                then
                                    strToMatrix1 str result 1 tempArray (i+1)
                                else    
                                    strToMatrix1 str result tempInt tempArray (i+1)
        else
            result  