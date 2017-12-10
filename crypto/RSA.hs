{-# LANGUAGE FlexibleInstances #-}
module RSA where

import           Data.Char(ord, chr)
import           Data.List(find)
import           System.Random
import           Control.Monad(replicateM)
import qualified Data.Set as S

-- types that can be represented as sequence of bits
class Binary a where
  toBitString   :: a -> BitString
  fromBitString :: BitString -> a


data    Bit        = Zero | One deriving (Eq, Ord)
type    BitString  = [Bit]
newtype PrivateKey = PrivateKey (BitString, BitString) deriving Show
newtype PublicKey  = PublicKey  (BitString, BitString) deriving Show


-- NOTE: this implementation doesn't care for sign of number
instance Binary Integer where
  toBitString = reverse . go
    where go 0 = [0]
          go 1 = [1]
          go n = (fromIntegral (n `mod` 2)) : go (n `div` 2)

  fromBitString = go . reverse
    where go [] = 0
          go (x:xs) = (fromBit x) + 2 * go xs


instance Binary Char where
  toBitString   = (toBitString :: Integer -> BitString) . fromIntegral . ord

  fromBitString = chr . fromIntegral . (fromBitString :: BitString -> Integer)


-- this implementation aligns bits to maximum unicode symbol length (32 bits)
instance Binary [Char] where
  toBitString  = concatMap (alignBits 32 . toBitString)

  fromBitString [] = []
  fromBitString s  = fromBitString x : fromBitString xs
    where (x, xs)  = splitAt 32 s


-- used just for ability to conviniently represent Bits as numbers 1 and 0
-- so only fromInteger function is needed
instance Num Bit where
  fromInteger 0 = Zero
  fromInteger 1 = One

  (+) _ _  = error "Not Supported"
  (-) _ _  = error "Not Supported"
  (*) _ _  = error "Not Supported"
  abs _    = error "Not Supported"
  signum _ = error "Not Supported"


instance Show Bit where
  show Zero = "0"
  show One  = "1"


instance Random Bit where
  randomR (One, _)    g = (One,  g)
  randomR (_  , Zero) g = (Zero, g)
  randomR _           g = random g
  random g = let (x, g') = randomR (0, 1) g in (fromInteger x, g')


fromBit :: Num a => Bit -> a
fromBit 0 = 0
fromBit 1 = 1


-- adds insignificant zeros to align BitString to corresponding size
alignBits :: Int -> BitString -> BitString
alignBits n s = replicate (n - length s) 0 ++ s


-- generates random prime number in bitstring representation with specified
-- number of bits (k)
-- for real applications this implementation somewhat inefficient
generatePrime :: Int -> IO Integer
generatePrime k = do
  tl       <- replicateM k randomIO
  let num  =  fromBitString (1 : tl) :: Integer
  let num' =  head $ [x | x <- [num..], testPrime x]
  return num'

  where testPrime  n =  all ((/= 0) . (n `mod`)) [2..(floor . sqrt . fromIntegral) n]


isCoprime :: Int -> Int -> Bool
isCoprime a b = (factorization a `S.intersection` factorization b) == S.empty
  where factorization 1 = S.empty
        factorization n = let factor = head [x | x <- [2..n], n `mod` x == 0] in
                            S.insert factor $ factorization (n `div` factor)


-- generate pair of public and private keys for use in rsa algorithm
generateKeys :: Int -> IO (PublicKey, PrivateKey)
generateKeys bits = do
  p <- generatePrime bits
  q <- generatePrime bits
  let n =  p * q
  let fi = (p - 1) * (q - 1)
  return $ error "WIP"


main :: IO ()
main = do
  print "WIP"
