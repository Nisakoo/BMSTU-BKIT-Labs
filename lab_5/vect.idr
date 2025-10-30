module Main

data Vect' : Nat -> Type -> Type where
    Nil : Vect' Z a
    (::)  : a -> Vect' k a -> Vect' (S k) a


vlen : {n : Nat} -> Vect' n a -> Nat
vlen xs = n

sum : Num a => Vect' n a -> Vect' n a -> Vect' n a
sum [] [] = []
sum (x :: xs) (y :: ys) = (x + y) :: sum xs ys

showvec : Show a => Vect' n a -> String
showvec [] = ""
showvec (x :: xs) = show x ++ " " ++ showvec xs

a : Vect' 3 Int
a = 1 :: 2 :: 3 :: Nil

main : IO()
main = putStrLn (showvec (sum a a))