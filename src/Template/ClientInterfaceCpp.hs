----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Template.ClientInterfaceCpp where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.StringEngine

import Language
import CppHelper

----------------------------------------------------------------------------------------------------

renderClientInterface :: Interface -> B.ByteString
renderClientInterface interface = C.pack $ strEngine vars [str|

class I<name>;
typedef boost::shared_ptr\<<name>\> I<name>_ap;

class I<name>
{
public:
   virtual I<name>::~I<name>(){}

<for method in methods>
   <method>
<end>

   static <name>_ap connect(const char* host, unsigned short port, int timeout);

}; // class I<name>

|]
   where
      vars =
         [
            Var "name" (infcName interface),
            Var "methods" (map renderInterfaceMethod (infcMethods interface))
         ]


renderInterfaceMethod :: Method -> String
renderInterfaceMethod method = strEngine vars [str|virtual <retType> <name>(<params>) = 0;|]
   where
      vars =
         [
            Var "retType" (renderType (metRetType method)),
            Var "name" (metName method),
            Var "params" (commaSep (map renderParam (metParams method)))
         ]

----------------------------------------------------------------------------------------------------
