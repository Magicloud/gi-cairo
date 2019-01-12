{-# LANGUAGE QuasiQuotes #-}
module Helper (genForEnum, genForStruct, mkName, castForeignPtr, newIORef, Connector, fromGI, toGI) where

import Data.Char
import Data.GI.Base.BasicTypes
import Data.IORef
import Foreign.ForeignPtr
import GI.Cairo.Connector
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

genForEnum :: Name -> Q [Dec]
genForEnum enumName =
  let n = capital2underscore $ unOccName $ nameOccName enumName
      c_fun = "cairo_gobject_" ++ n ++ "_get_type"
      fun = mkName $ n ++ "GType"
  in sequence
  [ return $ ForeignD $ ImportF CCall Safe c_fun fun $ AppT (ConT $ mkName "IO") (ConT $ mkName "GType")
  , head <$> [d|instance BoxedEnum $(conT enumName) where
                  boxedEnumType _ = $(varE fun)|] ]

genForStruct :: Name -> Q [Dec]
genForStruct structName =
  let n = capital2underscore $ unOccName $ nameOccName structName
      c_fun = "cairo_gobject_" ++ n ++ "_get_type"
      fun = mkName $ n ++ "GType"
      qn = Name (nameOccName structName) (NameQ $ ModName "T")
  in sequence
  [ newtypeD (cxt []) structName [] Nothing (normalC structName [bangType (bang noSourceUnpackedness noSourceStrictness) $ appT (conT ''ManagedPtr) (conT structName)]) []
  , head <$> [d|instance BoxedObject $(conT structName) where
                  boxedType _ = $(varE fun)|]
  , return $ ForeignD $ ImportF CCall Safe c_fun fun $ AppT (ConT $ mkName "IO") (ConT $ mkName "GType")
  , head <$> [d|instance Connector $(conT structName) $(conT qn) where
                  fromGI $(conP structName [conP (mkName "ManagedPtr") [varP $ mkName "fp", wildP, wildP]]) = return $ $(conE qn) $ castForeignPtr $(varE $ mkName "fp")
                  toGI $(conP qn [varP (mkName "fp")]) = newIORef Nothing >>= return . $(conE structName) . ManagedPtr (castForeignPtr $(varE $ mkName "fp")) Nothing|] ]

capital2underscore :: String -> String
capital2underscore i = case concatMap (\c ->
  if isUpper c
    then ['_', toLower c]
    else [c]) i of
  '_' : x -> x
  x -> x

nameOccName :: Name -> OccName
nameOccName (Name occName _) = occName

unOccName :: OccName -> String
unOccName (OccName s) = s
