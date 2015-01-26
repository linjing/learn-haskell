
table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = map (\(a, b)-> (a,b, f a b)) [ (a, b) | a <- [True, False], b <- [True, False] ]

ptable :: (Bool -> Bool -> Bool) -> IO ()
ptable f = mapM_ putStrLn $ map (\(a,b,c) -> (show a ++ " " ++ show b ++ " " ++ show c)) $ table f


