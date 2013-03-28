import Test.QuickCheck
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as B

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_test_00 s = s == s
  where _ = s :: Int

prop_block seq = block seq
  where seq = B.pack '>' B.cons (s :: Bytestring)

tests = [("test_00", quickCheck prop_test_00)
        ,("test_01", quickCheck prop_block  )
        ]
