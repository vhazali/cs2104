import Data.Array.IArray

fact n = if n==0 then 1 else n * (fact (n-1))

double [] = []
double (x:xs) = 2*x : (double xs)

numsFrom n	= n : numsFrom (n+1)
ones = 1:ones

squares		= map (^ 2) (numsFrom 0)

-- below gives infinite list of primes
primes = sieve [2..]

sieve (x:xs) = x:sieve (filter (\ y -> not((y `mod` x)==0)) xs)



list = [i*i|i<-[1..3]]

list2 = map (\ i -> i*i) [1..3]

list3 = [ (i*i,j) | i<-[1..3],j<-[7..8]]

list3a = concatMap (\ i -> map (\ j -> (i*i,j)) [7..8]) [1..3]

loop = loop

lz = loop:loop:5:6:[]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    quicksort [y | y <-xs, y<x]
    ++ [x]
    ++ quicksort [y | y <-xs, y>=x]

foo x = x

fibs :: Int -> Array Int Integer
fibs n = a where 
    a = array (0,n) ([(0,1),(1,1)] ++ [(i,a!(i-2)+a!(i-1)) | i <- [2..n]])
    
fib1000 = fibs 1000

hist bnds is = accumArray (+) 0 bnds [(i,1) | i <- is,
                                      inRange bnds i]
e17 :: Array Char Int
e17 = hist ('a','z') "This counts the frequencies of each lowercase letter"

main :: IO ()
main = do c<- getChar
          putStrLn ("\nYou have typed.."++[c])

