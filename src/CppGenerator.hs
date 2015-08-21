----------------------------------------------------------------------------------------------------

--TODO module CppGenerator

module CppGenerator(genFiles) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath
import Control.Monad(forM_)

import Language
import Template.HeaderCpp
import Template.StructCpp
import Template.ClientInterfaceCpp
import Template.CppCpp
import Template.ClientProxyCpp
import Template.MethodIdsHeader
import Template.ServerCpp

----------------------------------------------------------------------------------------------------

--TODO genFiles :: FilePath -> Module -> IO ()
genFiles :: FilePath -> Module -> IO ()
genFiles baseDir modul = do
   let
      baseFileName = T.unpack (modName modul) ++ "Client"
      headerFileName = baseDir </> baseFileName ++ ".h"
      structs = T.concat $ map renderStructDecl $ modStructDefs modul
      interfaces = T.concat $ map renderClientInterface $ modInterfaces modul
      content = structs `T.append` interfaces
      header = renderHeader baseFileName modul content

   putStrLn $ "Create file '" ++ headerFileName ++ "' ..."
   TIO.writeFile headerFileName header

   let
      cppFileName = baseDir </> baseFileName ++ ".cpp"
      structs = T.concat $ map renderStructImpl $ modStructDefs modul
      cpp = renderCpp baseFileName modul structs

   putStrLn $ "Create file '" ++ cppFileName ++ "' ..."
   TIO.writeFile cppFileName cpp

   forM_ (modInterfaces modul) $ genProxyFile baseDir modul

   let
      baseFileName = T.unpack (modName modul) ++ "MethodIds"
      headerFileName = baseDir </> baseFileName ++ ".h"
      header = renderMethodIdsHeader baseFileName modul

   putStrLn $ "Create file '" ++ headerFileName ++ "' ..."
   TIO.writeFile headerFileName header

   forM_ (modInterfaces modul) $ genServerCpp baseDir modul

   return ()


genProxyFile :: FilePath -> Module -> Interface -> IO ()
genProxyFile baseDir modul interface = do
   let
      baseFileName = T.unpack (infcName interface) ++ "Proxy"
      cppFileName = baseDir </> baseFileName ++ ".cpp"
      cpp = renderClientProxy modul interface

   putStrLn $ "Create file '" ++ cppFileName ++ "' ..."
   TIO.writeFile cppFileName cpp


genServerCpp :: FilePath -> Module -> Interface -> IO ()
genServerCpp baseDir modul interface = do
   let
         baseFileName = T.unpack (infcName interface) ++ "Server"
         cppFileName = baseDir </> baseFileName ++ ".cpp"
         cpp = renderServerCpp modul interface

   putStrLn $ "Create file '" ++ cppFileName ++ "' ..."
   TIO.writeFile cppFileName cpp


----------------------------------------------------------------------------------------------------