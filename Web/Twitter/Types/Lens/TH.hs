{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Twitter.Types.Lens.TH
       where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Web.Twitter.Types.Lens.Types

makeLenses :: Name -> Q [Dec]
makeLenses typename = do
    typeinfo <- reify typename
    case typeinfo of
        TyConI (DataD _cxt _name tyVarBndr [RecC _dataConName fields] _names) ->
            makeFieldLenses typename tyVarBndr fields
        _ -> error $ "unknown type info: reify " ++ show typename

makeFieldLenses :: Name -> [TyVarBndr] -> [VarStrictType] -> Q [Dec]
makeFieldLenses tyConName tyVarBndr fields = do
    fieldsDec <- mapM (eachField tyConName tyVarBndr) fields
    return $ concat fieldsDec

eachField :: Name -> [TyVarBndr] -> (Name, Strict, Type) -> Q [Dec]
eachField tyConName tyVarBndr (fieldName, _, fieldType) = do
    let funN = mkName (nameBase fieldName)
    sigdef <- eachFieldSigD funN tyConName tyVarBndr fieldType
    f <- newName "f"
    record <- newName "record"
    newVal <- newName "newVal"
    recUpdVal <- varE newVal
    let expr = [|fmap|]
               `appE` (lamE [varP newVal] (recUpdE (varE record) [return (fieldName, recUpdVal)]))
               `appE` (varE f `appE` (varE fieldName `appE` varE record))
    bind <- funD funN [clause [varP f, varP record] (normalB expr) []]
    pragD <- pragInlD funN Inline FunLike AllPhases
    return [sigdef, bind, pragD]

eachFieldSigD :: Name -> Name -> [TyVarBndr] -> Type -> DecQ
eachFieldSigD funN tyConName [_] (VarT _fieldTypeVal) = do
    a <- newName "a"
    b <- newName "b"
    let typ = forallT [PlainTV a, PlainTV b] (return []) (conT ''Lens `appT` (conT tyConName `appT` varT a) `appT` (conT tyConName `appT` varT b) `appT` varT a `appT` varT b)
    sigD funN typ
eachFieldSigD funN tyConName [PlainTV a] fieldType = do
    let typ = forallT [PlainTV a] (return []) (conT ''SimpleLens `appT` (conT tyConName `appT` varT a) `appT` return fieldType)
    sigD funN typ
eachFieldSigD funN tyConName [] fieldType = do
    sigD funN (conT ''SimpleLens `appT` conT tyConName `appT` return fieldType)
eachFieldSigD funN tyConName tyVarBndr fieldType =
    error $ "Unknown TH : " ++ show funN ++ " " ++ show tyConName ++ " " ++ show tyVarBndr ++ " " ++ show fieldType
