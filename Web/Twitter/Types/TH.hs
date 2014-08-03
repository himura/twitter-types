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

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type SimpleLens s a = Lens s s a a

-- data Hoge = Hoge { hoge :: String }

-- do
--     test <- [d| name :: SimpleLens Hoge String; name f record = fmap (\newVal -> record { hoge = newVal }) (f (hoge record)) |]
--     runIO $ print test
--     return []

-- [SigD name_1627469072 (AppT (AppT (ConT Web.Twitter.Types.TH.SimpleLens) (ConT Web.Twitter.Types.TH.Hoge)) (ConT GHC.Base.String)),FunD name_1627469072 [Clause [VarP f_1627469073,VarP record_1627469074] (NormalB (AppE (AppE (VarE GHC.Base.fmap) (LamE [VarP newVal_1627469075] (RecUpdE (VarE record_1627469074) [(Web.Twitter.Types.TH.hoge,VarE newVal_1627469075)]))) (AppE (VarE f_1627469073) (AppE (VarE Web.Twitter.Types.TH.hoge) (VarE record_1627469074))))) []]]


-- [SigD name_1627466792 (AppT (AppT (ConT Web.Twitter.Types.TH.SimpleLens) (ConT Web.Twitter.Types.TH.Hoge)) (ConT GHC.Base.String)),FunD name_1627466792 [Clause [VarP f_1627466793,VarP record_1627466794] (NormalB (InfixE (Just (LamE [VarP newVal_1627466795] (RecUpdE (VarE record_1627466794) [(Web.Twitter.Types.TH.hoge,VarE newVal_1627466795)]))) (VarE Data.Functor.<$>) (Just (AppE (VarE f_1627466793) (AppE (VarE Web.Twitter.Types.TH.hoge) (VarE record_1627466794)))))) []]]

-- TyConI (DataD [] Web.Twitter.Types.Status [] [RecC Web.Twitter.Types.Status [(Web.Twitter.Types.statusCreatedAt,NotStrict,ConT Web.Twitter.Types.DateString),(Web.Twitter.Types.statusId,NotStrict,ConT Web.Twitter.Types.StatusId),(Web.Twitter.Types.statusText,NotStrict,ConT Data.Text.Internal.Text),(Web.Twitter.Types.statusSource,NotStrict,ConT Data.Text.Internal.Text),(Web.Twitter.Types.statusTruncated,NotStrict,ConT GHC.Types.Bool),(Web.Twitter.Types.statusEntities,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.Entities)),(Web.Twitter.Types.statusExtendedEntities,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.Entities)),(Web.Twitter.Types.statusInReplyTo,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.StatusId)),(Web.Twitter.Types.statusInReplyToUser,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.UserId)),(Web.Twitter.Types.statusFavorite,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Bool)),(Web.Twitter.Types.statusRetweetCount,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Integer.Type.Integer)),(Web.Twitter.Types.statusUser,NotStrict,ConT Web.Twitter.Types.User),(Web.Twitter.Types.statusRetweet,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.Status)),(Web.Twitter.Types.statusPlace,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.Place)),(Web.Twitter.Types.statusFavoriteCount,NotStrict,ConT GHC.Integer.Type.Integer),(Web.Twitter.Types.statusLang,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Data.Text.Internal.Text)),(Web.Twitter.Types.statusPossiblySensitive,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Bool)),(Web.Twitter.Types.statusCoordinates,NotStrict,AppT (ConT Data.Maybe.Maybe) (ConT Web.Twitter.Types.Coordinates))]] [])
