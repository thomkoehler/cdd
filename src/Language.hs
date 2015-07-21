----------------------------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Language where


import Data.ByteString

----------------------------------------------------------------------------------------------------

data Type
   = Type { tNs :: Ns, tName :: ByteString }
   | TVoid
   | TString
   | TInt
   | TInt64
   | TDouble
   | TBool
   | TObject
   | TGetFilter
   | TCndFilter
   deriving Show


data Ns = Ns
   {
      nsPath :: [ByteString]
   }
   deriving Show


data Attr = Attr
   {
      attrType :: Type,
      attrName :: ByteString
   }
   deriving Show


data Struct = Struct
   {
      stName :: ByteString,
      stAttrs :: [Attr]
   }
   deriving Show


data Method = Method
   {
      metName :: ByteString,
      metRetType :: Type,
      metParams :: [(Type, ByteString)]
   }
   deriving Show

data Interface = Interface
   {
      infcName :: ByteString,
      infcMethods :: [Method]
   }
   deriving Show

data Module = Module
   {
      modName :: ByteString,
      modNs :: Ns,
      modStructDefs :: [Struct],
      modInterfaces :: [Interface]
   }
   deriving Show


isVoid :: Type -> Bool
isVoid TVoid = True
isVoid _ = False


isCustomType :: Type -> Bool
isCustomType Type {..} = True
isCustomType _ = False


isSimpleType :: Type -> Bool
isSimpleType Type {..} = False
isSimpleType TString = False
isSimpleType TObject = False
isSimpleType TGetFilter = False
isSimpleType TCndFilter = False
isSimpleType _ = True


isGlobalNs :: Ns -> Bool
isGlobalNs (Ns []) = True
isGlobalNs _ = False


globalNs :: Ns
globalNs = Ns []

----------------------------------------------------------------------------------------------------

