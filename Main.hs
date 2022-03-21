import Debug.Trace

calcBlock1 = calcBlock' 1 12 4       --                              var1 + 4
calcBlock2 = calcBlock' 1 11 11      -- (* 26)                       26(var1 + 4) + var2 + 11
calcBlock3 = calcBlock' 1 13 5       -- (* 26)                       26(26(var1 + 4) + var2 + 11) + var3 + 5
calcBlock4 = calcBlock2              -- (* 26)                       26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11
calcBlock5 = calcBlock' 1 14 14      -- (* 26)                       26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var5 + 14
calcBlock6 = calcBlock' 26 (-10) 7   -- [Div when var5 + 4 == var6]  26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11
calcBlock7 = calcBlock2              -- (* 26)                       26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var7 + 11
calcBlock8 = calcBlock' 26 (-9) 4    -- [Div when var7 + 2 == var8]  26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11
calcBlock9 = calcBlock' 26 (-3) 6    -- [Div when var4 + 8 == var9❗] 26(26(var1 + 4) + var2 + 11) + var3 + 5
calcBlock10 = calcBlock3             -- (* 26)                       26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var10 + 5
calcBlock11 = calcBlock' 26 (-5) 9   -- [Div when var10 == var11]    26(26(var1 + 4) + var2 + 11) + var3 + 5
calcBlock12 = calcBlock' 26 (-10) 12 -- [Div when var3 - 5 == var12] 26(var1 + 4) + var2 + 11
calcBlock13 = calcBlock' 26 (-4) 14  -- [Div when var2 + 7 == var13] var1 + 4
calcBlock14 = calcBlock' 26 (-5) 14  -- [Div when var1 - 1 == var14] 0

-- Limitations:
-- var1 - 1 == var14
-- var2 + 7 == var13
-- var3 - 5 == var12
-- var4 + 8 == var9
--   --> var4 == 1
--   --> var9 == 9
-- var5 + 4 == var6
-- var7 + 2 == var8
-- var10 == var11

allBlocks = [calcBlock1, calcBlock2, calcBlock3, calcBlock4, 
    calcBlock5, calcBlock6, calcBlock7, calcBlock8, calcBlock9, 
    calcBlock10, calcBlock11, calcBlock12, calcBlock13, calcBlock14]

allNums = numRange 99999999999999

blocksMini = [calcBlock12, calcBlock13, calcBlock14]
numsMini = numRange 999

blocksSmall = [calcBlock7, calcBlock8, calcBlock9, calcBlock10, calcBlock11, calcBlock12, calcBlock13, calcBlock14]
numsSmall = numRange' 99999 -- allow all numbers

blocksPrefix = [calcBlock1, calcBlock2, calcBlock3, calcBlock4, calcBlock5]

allInputs = [1..9]

calcBlock :: Int -> Int -> Int -> Int -> Int -> Int
calcBlock step5Div step6Add step16Add newVar z =
    let x = z `mod` 26 -- Step 2 & 3 & 4 combined
        z_1 = z `div` step5Div
        x_1 = if x + step6Add == newVar then 0 else 1 -- Step 7 & 8 combined
        y = 25 * x_1 -- Step 9 & 10 & 11 combined
        y_1 = y + 1
        z_2 = z_1 * y_1
        y_new = newVar + step16Add -- Step 14 & 15 & 16 combined
        in z_2 + y_new * x_1 -- Step 17 & 18 combined
--             26     -5         14        1     15
calcBlock' :: Int -> Int -> Int -> Int -> Int -> Int
calcBlock' step5Div step6Add step16Add newVar z =
    if z `mod` 26 + step6Add == newVar 
    then z `div` step5Div 
    else 26 * (z `div` step5Div) + newVar + step16Add

checkMonad :: [Int -> Int -> Int] -> Int -> MonadResult
checkMonad blocks n  = 
    let vars = digits n
        blocksWithVar :: [Int -> Int]
        blocksWithVar = zipWith (\b v -> b v) blocks vars
    in foldl (\(zs, z) f -> let res = f z in(zs ++ [res], res)) ([0], 0) blocksWithVar

main = do
    let printRes :: ([Int], MonadResult) -> [Char]
        printRes (vs, (zs, z)) = "Vars" ++ show vs ++ " => " ++ show z ++ " " ++ show zs ++ "\n"
        
        isValid :: ([Int], MonadResult) -> Bool
        isValid (_, (_, res)) = res == 0
        isWhatEver = const True

        validMonads = filter isValid $ map (\n -> (digits n, checkMonad allBlocks n)) allNums

    -- print $ (take 10 (numRange 99999999999999))
    putStrLn $ concatMap printRes validMonads
    -- print $ "Res => " ++ show (calcBlock14 2 6)
    -- print $ followConstraints (digits 91911513977488)
    print "End"

digits :: Int -> [Int]
digits = map (read.return) . show

followConstraints :: [Int] -> Bool
followConstraints ds = notElem 0 ds && otherLimits ds

-- Limitations:
-- var1 - 1 == var14
-- var2 + 7 == var13
-- var3 - 5 == var12
-- var4 + 8 == var9
--   --> var4 == 1
--   --> var9 == 9
-- var5 + 4 == var6
-- var7 + 2 == var8
-- var10 == var11

otherLimits :: [Int] -> Bool
otherLimits ds = let (v1:r1) = ds
                     (v2:r2) = r1
                     (v3:r3) = r2
                     (v4:r4) = r3
                     (v5:r5) = r4
                     (v6:r6) = r5
                     (v7:r7) = r6
                     (v8:r8) = r7
                     (v9:r9) = r8
                     (v10:r10) = r9
                     (v11:r11) = r10
                     (v12:r12) = r11
                     (v13:r13) = r12
                     (v14:[]) = r13
                in v1 - 1 == v14 && v2 + 7 == v13 && v3 + (-5) == v12 && v4 == 1 && v9 == 9 && v5 + 4 == v6 && v7 + 2 == v8 && v10 == v11

numRange :: Int -> [Int]
numRange maxN = filter (followConstraints.digits) [maxN, maxN - 1 .. minWithoutZeroes maxN]
numRange' maxN = [maxN, maxN - 1 .. minWithoutZeroes maxN] -- allows all numbers

minWithoutZeroes :: Int -> Int
minWithoutZeroes n = fromDigits $ replicate (ceiling $ logBase 10.0 $ fromIntegral n) 1 

fromDigits :: [Int] -> Int
fromDigits = foldl (\n agg -> 10 * n + agg) 0

type MonadResult = ([Int], Int)


-- when step5Div == 1 and step6Add > 9
-- 26 * z + step16Add + newVar

-- For step5Div == 26
-- =====================================
-- when step5Div == 26 and z `mod` 26 + step6Add == newVar
-- z `div` 26                                   (step6Add and newVar are not in the result)

-- when step5Div == 26 and z `mod` 26 + step6Add != newVar
-- 26 * (z `div` 26) + step16Add + newVar       (step6Add is not in the result)


-- getting valid means: calcBlock 26 (-5) 14 z newVar ==> 0 (step6Add == -5)
-- if z `mod` 26 + step6Add == newVar then z `div` 26 (then z needs to be [0, 25] to get 0)
-- then z `mod` 26 - 5 == newVar (dan z = newVar + 5)


-- ALL TOGETHER

-- Block 1 really does: f(var) = var1 + 4
-- Block 2 really does: f(var, z) = 26z + var2 + 11
-- Block 3 really does: f(var, z) = 26z + var3 + 5
-- Block 4 is the same as block 2 ==> 26z + var4 + 11
-- Block 5 really does: f(var, z) = 26z + var5 + 14


-- Block1 to Block5 combined: 
-- 26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var5 + 14


-- Block 6:
-- (26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var5 + 14) `div` 26
-- 26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11

-- Block 7: 26z + var2 + 11
-- 26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var7 + 11

-- Block 8:
-- (26(26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) + var7 + 11) `div` 26
-- (26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11)

-- Block 9:
-- (26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var4 + 11) `div` 26
-- 26(26(var1 + 4) + var2 + 11) + var3 + 5

-- Block 10: 26z + var10 + 5
-- 26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var10 + 5

-- Block11:
-- (26(26(26(var1 + 4) + var2 + 11) + var3 + 5) + var10 + 5) `div` 26
-- 26(26(var1 + 4) + var2 + 11) + var3 + 5

-- Block 12:
-- (26(26(var1 + 4) + var2 + 11) + var3 + 5) `div` 26
-- 26(var1 + 4) + var2 + 11

-- Block 13:
-- (26(var1 + 4) + var2 + 11) `div` 26
-- var1 + 4

-- Block 14:







-- Block 6 (calcBlock' 26 (-10) 7)

-- if z `mod` 26 + step6Add == newVar
-- if (26M + var5 + 14) `mod` 26 + step6Add == newVar 
-- --> 0 + var5 + 14 + -10 == newVar
-- --> var5 + 4 == var6 
-- then z `div` 26 
--   --> divide by 26, remove remainder
-- else 26 * ((26M + var5 + 14) `div` 26) + newVar + step16Add
--   --> 26M + newVar + step16Add (multiply by 26 again)

-- Conclusion: var5 + 4 == var6 has to be true, else this cannot terminate (Right?)



-- Block 7 (calcBlock' 1 11 11)

--  == Block 2 ==> 26z + var7 + 11


-- Block 8 (calcBlock' 26 (-9) 4)

-- --> z `mod` 26 + -9 == newVar
-- --> (26z + var2 + 11) `mod` 26 + -9 == var8
-- --> var2 + 11 - 9 = var8
-- Conclusion: var2 + 2 = var8

-- Block 9 (calcBlock' 26 (-3) 6)

-- if (z `div` step5Div) `mod` 26 + -3 == newVar9
-- --> -3 == var9

-- Block 10 (calcBlock' 1 13 5)

--  == Block 3 ==> 26z + var10 + 5

-- Block 11 (calcBlock' 26 (-5) 9)

-- if (26z + var10 + 5) `mod` 26 + -5 == var11
-- ==> var10 + 5 + -5 == var11
-- ==> var10 == var11


-- Block 12 (calcBlock' 26 (-10) 12)

-- if z `mod` 26 + -10 == newVar




-- Block 13 (calcBlock' 26 (-4) 14)
-- if z `mod` 26 + -4 == var13
-- --> z `mod` 26 + -4 == var14
-- --> z - 5 == var14

-- Block 14 (calcBlock' 26 (-5) 14)
-- if z `mod` 26 + -5 == var14
-- --> z `mod` 26 + -5 == var14
-- --> z - 5 == var14
-- z is tussen 6 (1+5) en 14 (9+5) 
-- then z `div` 26 ==> moet 0 zijn (z moet tussen 0 en 25 zijn)

-- **************************
-- >> z - 5 == var14
-- >> z elem [6,..,14]


-- What about: z == Block 1
-- Block 1: var1 + 4
-- var1 + 4 - 5 == var14
-- var1 - 1 == var14 (var1 cannot be 1, var14 cannot be 9)