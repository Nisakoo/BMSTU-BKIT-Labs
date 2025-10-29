findRoot :: Float -> Float -> Float -> Float
findRoot a b d = (d - b) / (2 * a)

quadrateEquation :: Float -> Float -> Float -> [Float]
quadrateEquation a b c
        | a == 0    = [-c / b]
        | disc > 0  = [findRoot a b (sqrt disc), (findRoot a b (-sqrt disc))]
        | disc == 0 = [findRoot a b disc]
        | otherwise = []
    where disc = b*b - 4*a*c

getSqrts :: [Float] -> [Float]
getSqrts [] = []
getSqrts (x:xs)
    | x >= 0    = [(sqrt x), (-sqrt x)] ++ getSqrts xs
    | otherwise = getSqrts xs

biquadrateEquation :: Float -> Float -> Float -> [Float]
biquadrateEquation a b c = getSqrts (quadrateEquation a b c)


main :: IO ()
main = print(biquadrateEquation 0 (-5) 1)