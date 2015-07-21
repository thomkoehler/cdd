----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Template.ClientInterfaceCpp where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.StringEngine

import Language

----------------------------------------------------------------------------------------------------

renderClientInterface :: Interface -> B.ByteString
renderClientInterface interface = C.pack $ strEngine vars [str|

class I<name>;
typedef boost::shared_ptr<I<name>> <name>_ap;

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
            Var "methods" ([] :: [String])
         ]


renderInterfaceMethod :: Method -> B.ByteString
renderInterfaceMethod method = C.pack $ strEngine vars [str|virtual <retType> <name>() = 0;|]
   where
      vars =
         [
            Var "retType" "void",
            Var "name" (metName method)
         ]

----------------------------------------------------------------------------------------------------
