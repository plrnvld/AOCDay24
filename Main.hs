main = do
    let allInputs = [1..9]
        varInput = 1
        calcBlock1 = calcBlock 1 12 4
        calcBlock2 = calcBlock 1 11 11
        calcBlock3 = calcBlock 1 13 5
        calcBlock4 = calcBlock2
        calcBlock5 = calcBlock 1 14 14
        calcBlock6 = calcBlock 26 (-10) 7
        calcBlock7 = calcBlock2
        calcBlock8 = calcBlock 26 (-9) 4
        calcBlock9 = calcBlock 26 (-3) 6
        calcBlock10 = calcBlock3
        calcBlock11 = calcBlock 26 (-5) 9
        calcBlock12 = calcBlock 26 (-10) 12
        calcBlock13 = calcBlock 26 (-4) 14
        calcBlock14 = calcBlock 26 (-5) 14
        calcRes = calcBlock6 1000
        makeResText varInput res = "Outcome for " ++ show varInput ++ " = " ++ show res ++ "\n"

    putStrLn $ concatMap (\i -> makeResText i (calcRes i)) allInputs

calcBlock :: Int -> Int -> Int -> Int -> Int -> Int
calcBlock step5Div step6Add step16Add prevZ newVar =
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

-- Block 1 really does: f(var) = var + 4
-- Block 2 really does: f(var, z) = 26z + var + 11
-- Block 3 really does: f(var, z) = 26z + var + 5
-- Block 4 is the same as block 2
-- Block 5 really does: f(var, z) = 26z + var + 14
-- Block 6 really does: f(var, z) = var + 7? Not true, bigger numbers give bigger results


        
        
        
