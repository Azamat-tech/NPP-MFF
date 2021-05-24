data Tree = Nil | Node Tree Int Tree
  deriving (Eq, Ord, Show)

allBalanced :: Int -> [Tree] 
allBalanced n = allBalanced_h([1..n])

allBalanced_h :: [Int] -> [Tree] 
allBalanced_h [] = [Nil]
allBalanced_h xs =  if (length xs) `mod` 2 == 1 then
                      let mid = xs !! (length xs `div `2 )
                          start = xs !! 0
                          end = xs !! ((length xs)-1)
                      in [Node left mid right | left <- allBalanced_h ([start..(mid-1)]), 
                                          right <- allBalanced_h ([(mid+1)..end])]
                    else
                      let mid = xs !! ((length xs `div `2)-1)
                          mid2 = xs !! (length xs `div `2)
                          start = xs !! 0
                          end = xs !! ((length xs)-1)
                      in  [Node left mid right | left <- allBalanced_h ([start..(mid-1)]), 
                                          right <- allBalanced_h ([(mid+1)..end])] ++
                          [Node left mid2 right | left <- allBalanced_h ([start..(mid2-1)]), 
                                          right <- allBalanced_h ([(mid2+1)..end])]