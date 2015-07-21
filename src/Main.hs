----------------------------------------------------------------------------------------------------

module Main where

import qualified Data.ByteString as B
import System.IO

import Language
import CddLexer
import CddParser
import Template.ClientInterfaceCpp

----------------------------------------------------------------------------------------------------

main = do
   text <- B.readFile "Test/test0.cdd"
   let modul = parse "Test/test0.cdd" text
   let (i:_) = modInterfaces modul
   B.hPutStr stdout $ renderClientInterface i

----------------------------------------------------------------------------------------------------