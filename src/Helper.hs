
---------------------------------------------------------------------------------------------------

module Helper
(
   camelCaseToUnderscore,
   camelCaseToUpperUnderscore
)
where


import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Char(isUpper, toUpper)

---------------------------------------------------------------------------------------------------

camelCaseToUnderscore :: B.ByteString -> B.ByteString
camelCaseToUnderscore = C.concatMap step
   where
      step :: Char -> B.ByteString
      step char = if isUpper char
         then C.pack ['_', char]
         else C.singleton char


camelCaseToUpperUnderscore :: B.ByteString -> B.ByteString
camelCaseToUpperUnderscore = C.map toUpper . camelCaseToUnderscore

---------------------------------------------------------------------------------------------------