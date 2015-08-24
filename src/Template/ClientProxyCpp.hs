----------------------------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

--TODO module Template.ClientProxyCpp
module Template.ClientProxyCpp where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Text.StringEngine

import Language
import CppHelper
import Helper

----------------------------------------------------------------------------------------------------

renderClientProxy :: Module -> Interface -> B.ByteString
renderClientProxy modul interface = C.pack $ strEngine vars [str|
#include "<modName>.h"

#include "dbserver/CinemaDispatch.h"

using namespace <ns>;

namespace
{

class <name>Proxy : public I<name>
{
public:
<name>Proxy(CINEMA::ObjectDispatcher_ap disp, int dispId)
   : _disp(disp), _dispId(dispId)
{
}

<for method in methods>
   <method>
<end>

private:
   CINEMA::ObjectDispatcher_ap _disp;
   int _dispId;

}; // class <name>Proxy

} // namespace

<connectMethod>

|]
   where
      vars =
         [
            Var "methods" (map (renderProxyMethod interface) (infcMethods interface)),
            Var "name" (infcName interface),
            Var "ns" (renderNs (modNs modul)),
            Var "modName" (modName modul),
            Var "connectMethod" (renderConnectMethod interface)
         ]


renderProxyMethod :: Interface -> Method -> B.ByteString
renderProxyMethod interface method = C.pack $ strEngine vars [str|
virtual #{retType} #{name}(#{params})
{
   CINEMA::AttrObject params(_dispId, CINEMA::to_attr(#{infcName interface}MethodIds::#{renderMethodId method}));
#{renderAssignParams method}

   _disp->call(params);
#{renderProxyMethodReturn method}
}
|]
   where
      name = metName method
      retType = renderType $ metRetType method
      params = T.intercalate ", " $ map renderParam $ metParams method


renderProxyMethodReturn :: Method -> B.ByteString
renderProxyMethodReturn method
   | isVoid retType = T.empty
   | isCustomType retType = [st|   return #{renderType retType}(params[P_RETURN].Object());|]
   | otherwise = [st|   return params[P_RETURN].#{attrToTypeFunction retType};|]
   where
      retType = metRetType method


renderAssignParams :: Method -> B.ByteString
renderAssignParams method = unlinesIndent 3 $ map renderAssignParam $ zip [1..] $ metParams method


renderAssignParam :: (Int, (Type, T.Text)) -> B.ByteString
renderAssignParam (pos, (typ, name)) = if isCustomType typ
   then [st|
   params[P_#{pos}] = new CINEMA::AttrObject;
   #{name}.marshal(params[P_#{pos}].Object());
|]

   else [st|params[P_#{pos}] = #{name};|]


renderConnectMethod :: Interface -> B.ByteString
renderConnectMethod interface = C.pack $ strEngine vars [str|
#{name}_ap I#{name}::connect(const char* host, unsigned short port, int timeout)
{
   CINEMA::ObjectDispatcher_ap disp(ObjectDispatcher::create());
   disp->connect(host, port);
   CINEMA::OBJECT_TYPE id = disp->resolve(DISPATCHER_NAME);

   if(timeout >= 0)
   {
      disp->set_timeout(id, timeout);
   }

   return #{name}_ap(new #{name}Proxy(disp, id));
}
|]
   where
      name = infcName interface


----------------------------------------------------------------------------------------------------