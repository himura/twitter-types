{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Types.TH
       where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeLenses :: Name -> Q [Dec]
makeLenses typename = do
    typeinfo <- reify typename
    case typeinfo of
        TyConI (DataD cxt _name tyVarBndr [RecC _dataConName fields] _names) ->
            makeFieldLenses cxt typename tyVarBndr fields
        _ -> error $ "unknown type info: reify " ++ show typename

makeFieldLenses :: Cxt -> Name -> [TyVarBndr] -> [VarStrictType] -> Q [Dec]
makeFieldLenses cxt tyConName tyVarBndr fields = do
    fieldsDec <- mapM (eachField cxt tyConName tyVarBndr) fields
    return $ concat fieldsDec

eachField :: Cxt -> Name -> [TyVarBndr] -> (Name, Strict, Type) -> Q [Dec]
eachField _cxt tyConName _tyVarBndr (fieldName, _, fieldType) = do
    let funN = mkName (nameBase fieldName)
        simpleLensName = mkName "SimpleLens"
    sigdef <- sigD funN (conT simpleLensName `appT` conT tyConName `appT` return fieldType)
    f <- newName "f"
    record <- newName "record"
    newVal <- newName "newVal"
    recUpdVal <- varE newVal
    let expr = [|fmap|]
               `appE` (lamE [varP newVal] (recUpdE (varE record) [return (fieldName, recUpdVal)]))
               `appE` (varE f `appE` (varE fieldName `appE` varE record))
    bind <- funD funN [clause [varP f, varP record] (normalB expr) []]
    return [sigdef, bind]
