main = do
    putStrLn "Hello"
    putStrLn "World"

    let allInputs = [1..9]
        varInput = 1
        calcRes varInput = calcBlock varInput 0 1 12 4
        makeResText varInput res = "Outcome for " ++ show varInput ++ " = " ++ show res ++ "\n"

    putStrLn $ concat $ map (\i -> (makeResText i (calcRes i))) allInputs
    


calcBlock :: Int -> Int -> Int -> Int -> Int -> Int
calcBlock newVar prevZ step5Div step6Add step16Add =
    let w = newVar
        z = prevZ
        x = z -- Step 2 & 3 combined
        x_1 = x `mod` 26
        z_1 = z `div` step5Div
        x_2 = x_1 + step6Add
        x_3 = if x_2 == w then 0 else 1 -- Step 7 & 8 combined
        y = 25 -- Step 9 & 10 combined
        y_1 = y * x_3
        y_2 = y_1 + 1
        z_2 = z_1 * y_2
        y_3 = w -- Step 14 & 15 combined
        y_4 = y_3 + step16Add
        y_5 = y_4 * x_3
        z_3 = z_2 + y_5
        in z_3

        
        
        
