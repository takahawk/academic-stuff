module RSA where

data Bit = Zero | One
  deriving (Eq, Ord)

type BitString = [Bit]

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


main :: IO ()
main = do
  putStrLn "W.I.P."
