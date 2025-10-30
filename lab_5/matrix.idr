import Data.Vect

vecadd : Num a => Vect n a -> Vect n a -> Vect n a
vecadd [] [] = Nil
vecadd (x :: xs) (y :: ys) = (x + y) :: vecadd xs ys

vecdot : Num a => Vect n a -> Vect n a -> Vect n a
vecdot [] [] = Nil
vecdot (x :: xs) (y :: ys) = (x * y) :: vecdot xs ys

scalar : Num a => Vect n a -> Vect n a -> Vect 1 a
scalar xs ys = sum (vecdot xs ys) :: Nil


Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect m (Vect n a)

add : Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
add [] [] = Nil
add (x :: xs) (y :: ys) = (vecadd x y) :: add xs ys


mat1 : Matrix 2 3 Integer
mat1 = [[1, 2], [1, 2], [1, 2]]

mat2 : Matrix 2 2 Integer
mat2 = [[1, 2], [1,2 ]]

vec1 : Vect 3 Integer
vec1 = [1, 2, 3]