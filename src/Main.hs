----------------------------------------------------------------------------------------------------

module Main where

import qualified Data.ByteString as B
import System.IO

import Language
import CddLexer
import CddParser
import Template.ClientInterfaceCpp
import Template.MethodIdsHeader
import Template.ClientProxyCpp

----------------------------------------------------------------------------------------------------

main = do
   text <- B.readFile "Test/test0.cdd"
   let modul = parse "Test/test0.cdd" text
   let (i:_) = modInterfaces modul
   B.hPutStr stdout $ renderClientInterface i
   putStrLn "//--------------------------------------------------------"
   B.hPutStr stdout $ renderMethodIdsHeader "Test" modul

----------------------------------------------------------------------------------------------------
