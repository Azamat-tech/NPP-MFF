import Data.List

data SMatrix a = Scalar a | Matrix [[a]]

to_smatrix :: [[a]] -> SMatrix a 
to_smatrix xs = Matrix xs

trimNL :: String -> String
trimNL = reverse . dropWhile (== '\n') . reverse 

addMatrices :: (Num a) => [[a]] -> [[a]] -> [[a]]
addMatrices = zipWith (zipWith(+))

subMatrices :: (Num a) => [[a]] -> [[a]] -> [[a]]
subMatrices = zipWith (zipWith(-))

scalarMatrixMultiplication :: (Num a) => a -> [[a]] -> [[a]]
scalarMatrixMultiplication scalar matrix = [ map (*scalar) x | x <- matrix]

getColumn :: [[a]] -> Int
getColumn m = length (head m)

getRow :: [[a]] -> Int
getRow m = length m

matrixMultiplication :: (Num a) => [[a]] -> [[a]] -> [[a]]
matrixMultiplication m1 m2 = multiply m1 (transpose m2)

multiply :: (Num a) => [[a]] -> [[a]] -> [[a]]
multiply [] _ = []
multiply (x : xs) ys = calculateRow x ys : multiply xs ys

calculateRow :: (Num a) => [a] -> [[a]] -> [a]
calculateRow _ [] = []
calculateRow x (y : ys) = calculateWithColumn x y : calculateRow x ys

calculateWithColumn :: (Num a) => [a] -> [a] -> a
calculateWithColumn x y = foldl1 (+) (zipWith (*) x y)

instance (Show a) => Show (SMatrix a) where 
   show (Scalar a) = ("#" ++ show a)
   show (Matrix a) = trimNL(unlines (map (unwords . map show) a))
instance (Num a) => Num (SMatrix a) where 
    (+)(Scalar a) (Scalar b) = Scalar(a + b)
    (+)(Matrix a) (Matrix b) = 
        if (getRow a, getColumn a) == (getRow b, getColumn b)
            then to_smatrix (addMatrices a b)
            else error "Matrices have to be the same size"
    (+)(Scalar a) (Matrix b) = error "Invalid operation"
    (+)(Matrix b) (Scalar a) = error "Invalid operation"
    (-)(Scalar a) (Scalar b) = Scalar(a - b)
    (-)(Matrix a) (Matrix b) = 
        if (getRow a, getColumn a) == (getRow b, getColumn b)
            then to_smatrix (subMatrices a b)
            else error "Matrices have to be the same size"
    (-)(Scalar a) (Matrix b) = error "Invalid operation"
    (-)(Matrix b) (Scalar a) = error "Invalid operation"
    (*)(Scalar a) (Scalar b) = Scalar(a * b)
    (*)(Scalar a) (Matrix b) = to_smatrix (scalarMatrixMultiplication a b)
    (*)(Matrix a) (Scalar b) = to_smatrix (scalarMatrixMultiplication b a)
    (*)(Matrix a) (Matrix b) = 
        let columnOfA = getColumn a
            rowOfB = getRow b
            in if columnOfA == rowOfB then to_smatrix (matrixMultiplication a b)
                                      else error "Row and Column are not the same"
    fromInteger a = Scalar (fromInteger a)
    (abs) (Scalar a) = Scalar (abs a)
    (signum) (Scalar a) = Scalar(signum a)

