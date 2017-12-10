module RSA where

import System.Random
import Control.Monad


data Bit = Zero | One
  deriving (Eq, Ord)

newtype BitString  = BitString [Bit]

newtype PrivateKey = PrivateKey (BitString, BitString) deriving Show
newtype PublicKey  = PublicKey  (BitString, BitString) deriving Show


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


main :: IO ()
main = do
  keys <- generateKeys
  print keys
