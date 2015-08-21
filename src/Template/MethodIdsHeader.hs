----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Template.MethodIdsHeader(renderMethodIdsHeader) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.StringEngine

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderMethodIdsHeader :: FilePath -> Module -> B.ByteString
renderMethodIdsHeader fileBaseName modul = C.pack $ strEngine vars [str|
#ifndef <headerDef>
#define <headerDef>

<beginNs>
<content>
<endNsNs>

#endif // <headerDef>
|]
   where
      ns = modNs modul
      vars =
         [
            Var "headerDef" (headerDefine fileBaseName ns),
            Var "beginNs" (renderBeginNs ns),
            Var "endNs" (renderEndNs ns),
            Var "content" (C.unlines . map renderMethodIdsClass (modInterfaces modul))
         ]


renderMethodIdsClass :: Interface -> B.ByteString
renderMethodIdsClass interface = C.pack $ strEngine vars [str|
struct <infcName interface>MethodIds
{
   enum MethodId
   {
<for assign in assigns>
      <assign>
<end>
   };
};
|]
   where
      vars =
         [
            Var "methodAssigns" (map renderMethodIdAssign . zip [100..] (infcMethods interface))
         ]


renderMethodIdAssign :: (Int, Method) -> B.ByteString
renderMethodIdAssign (methodId, name) = C.pack $ strEngine vars "<idName> = <idValue>,"
   where
      vars = [Var "idName" (renderMethodId name), Var "idValue" methodId]

----------------------------------------------------------------------------------------------------
