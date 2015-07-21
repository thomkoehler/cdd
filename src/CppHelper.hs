
---------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module CppHelper
(
   renderType,
   renderAttrDecl,
   renderParam,
   renderParams,
   renderBeginNs,
   renderEndNs,
   renderNs,
   attrToTypeFunction,
   renderMethodId,
   headerDefine
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Char(toUpper)
import Text.Printf(printf)
import Text.StringEngine

import Language
import Helper

---------------------------------------------------------------------------------------------------

renderNs :: Ns -> B.ByteString
renderNs (Ns p) = B.intercalate "::" p


renderBeginNs :: Ns -> B.ByteString
renderBeginNs ns@(Ns ps) =
   let
      step p = B.concat ["namespace ", p, "{ "]
   in
      if isGlobalNs ns
         then B.empty
         else C.unlines $ map step ps


renderEndNs :: Ns -> B.ByteString
renderEndNs (Ns ps) = C.replicate (length ps) '}'


renderType :: Type -> B.ByteString
renderType TBool = "bool"
renderType TInt = "int"
renderType TInt64 = "__int64"
renderType TDouble = "double"
renderType TVoid = "void"
renderType TString = "std::string"
renderType TObject = "CINEMA::AttrObject"
renderType TGetFilter = "CINEMA::AttrGetFilter"
renderType TCndFilter = "CINEMA::AttrCndFilter"
renderType (Type ns name)
   | isGlobalNs ns = name
   | otherwise = B.concat [renderNs ns, "::", name]


--TODO attrToTypeFunction :: Type -> B.ByteString
attrToTypeFunction :: Type -> B.ByteString
attrToTypeFunction TBool = "Bool()"
attrToTypeFunction TInt = "Int()"
attrToTypeFunction TInt64 = "Int64()"
attrToTypeFunction TDouble = "Double()"
attrToTypeFunction TString = "String()"
attrToTypeFunction TObject = "Object()"
attrToTypeFunction TGetFilter = "GetFilter()"
attrToTypeFunction TCndFilter = "CndFilter()"
attrToTypeFunction TVoid = "Cannot get a value from void."
attrToTypeFunction _ = error "AttrToTypeFunction not implemented yet."


renderAttrDecl :: Attr -> B.ByteString
renderAttrDecl (Attr t n) = C.pack $ strEngine vars "<t> <n>"
   where
      vars = [Var "t" (renderType t), Var "n" n]


renderParam :: (Type, B.ByteString) -> B.ByteString
renderParam (t, n) =
   if isSimpleType t
      then C.pack $ strEngine vars "<t> <n>"
      else C.pack $ strEngine vars "const <t> &<n>"
   where
      vars = [Var "t" (renderType t), Var "n" n]


renderParams :: [(Type, B.ByteString)] -> B.ByteString
renderParams attrs = B.intercalate ", " $ map renderParam attrs


renderMethodId :: Method -> B.ByteString
renderMethodId method = "MID_" `B.append` camelCaseToUpperUnderscore (metName method)


headerDefine :: FilePath -> Ns -> B.ByteString
headerDefine fileBaseName ns =
   C.map toUpper $ B.concat [B.intercalate "_" (nsPath ns), "_", C.pack fileBaseName]

---------------------------------------------------------------------------------------------------




