data State = NoRoots
        | OneRoot Float
        | TwoRoots Float Float
        | FourRoots Float Float Float Float
    deriving (Show, Eq)


findRoot :: Float -> Float -> Float -> Float
findRoot a b d = (d - b) / (2 * a)

findDiscriminant :: Float -> Float -> Float -> Float
findDiscriminant a b c = b*b - 4*a*c

quadrateEquation :: Float -> Float -> Float -> State
quadrateEquation a b c
        | disc > 0  = TwoRoots (findRoot a b (sqrt disc)) (findRoot a b (-sqrt disc))
        | disc == 0 = OneRoot (findRoot a b disc)
        | otherwise = NoRoots
    where disc = findDiscriminant a b c

biquadrateEquation :: Float -> Float -> Float -> State
biquadrateEquation a b c =
    case quadrateEquation a b c of 
        NoRoots -> NoRoots
        OneRoot x
            | x > 0     -> TwoRoots (sqrt x) (-sqrt x)
            | x == 0    -> OneRoot 0
            | otherwise -> NoRoots
        TwoRoots x y
            | x > 0 && y > 0    -> FourRoots (sqrt x) (-sqrt x) (sqrt y) (-sqrt y)
            | x > 0             -> TwoRoots (sqrt x) (-sqrt x)
            | y > 0             -> TwoRoots (sqrt y) (-sqrt y)
            | otherwise         -> NoRoots


main :: IO ()
main = print(biquadrateEquation 1 (-5) 1)