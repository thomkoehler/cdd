----------------------------------------------------------------------------------------------------

module Template.MethodIdsHeader(renderMethodIdsHeader) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.StringEngine

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderMethodIdsHeader :: FilePath -> Module -> T.Text
renderMethodIdsHeader fileBaseName modul = [st|
#ifndef #{headerDef}
#define #{headerDef}

#{renderBeginNs ns}
#{content}
#{renderEndNs ns}

#endif // #{headerDef}
|]
   where
      ns = modNs modul
      headerDef = headerDefine fileBaseName ns
      content = T.unlines . map renderMethodIdsClass $ modInterfaces modul


renderMethodIdsClass :: Interface -> T.Text
renderMethodIdsClass interface = [st|
struct #{infcName interface}MethodIds
{
   enum MethodId
   {
#{methodAssigns}
   };
};
|]
   where
      methodAssigns = unlinesIndent 6 . map renderMethodIdAssign . zip [100..] $ infcMethods interface


renderMethodIdAssign :: (Int, Method) -> B.ByteString
renderMethodIdAssign (methodId, name) = C.pack $ strEngine vars "<idName> = <idValue>,"
   where
      vars = [Var "idName" (renderMethodId name), Var "idValue" methodId]

----------------------------------------------------------------------------------------------------
