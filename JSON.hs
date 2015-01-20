{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module JSON where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson
import Data.Aeson.Generic as AG
import qualified Data.Aeson.Types as AT
import Data.Data
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import SmartCopy as SC
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific (coefficient, base10Exponent, fromFloatDigits)
import Data.Typeable (Typeable)

-------------------------------------------------------------------------------
-- JSON Values
-------------------------------------------------------------------------------

emptyJS :: JSVal a
emptyJS = JS (Version 0) Base AT.emptyObject

array :: [Value] -> Value
array vs = Array $ V.fromList vs

objFromArray (Array vs) = let objs = V.toList vs in
                       object $ concat $ map M.toList (getMap objs)
                       where getMap [] = [] 
                             getMap (Object o:vs) = o:(getMap vs)
                                

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

instance Monoid (JSON Value) where
    mempty = return AT.emptyObject
    mappend = mult

instance Monad JSON where
    return a = JSON a
    m >>= g = g (unJSON m)

data JSVal a
    = JS
    { js_version :: Version a
    , js_kind :: Kind a
    , js_value :: Value
    }

newtype JSONVersioned a = JSONVersioned { unJSONVs :: JSVal a }
newtype JSON a = JSON { unJSON :: a } deriving Show
 
instance Functor JSON where
    fmap = undefined

instance Applicative JSON where
    pure = return
    jsa <*> jsb
        = do f <- jsa
             a <- jsb
             return (f a)

------- Comments are just "notes", only relevant for version control later on.
instance Format JSON where
    type EncodedType = Value
    unPack ma = unJSON ma
    returnEmpty = return AT.emptyObject
    enterCons (Cons clevel _ name sumType _) v =
        let val = unPack v
            --val = js_value $ unPack v
            --version = js_version $ unPack v in
            --kind = js_kind $ unPack v in
        in
        case val of
          Object _ ->
            case sumType of
              --False -> JSON $ JS version val
              False -> return val
              True ->
                  formatSumType name val
                --  in JSON $ JS version (object [("tag", t), ("contents", cont)])
          Array a ->
            case sumType of
              False ->
                  case clevel of
                    0 -> return $ objFromArray val
                    _ -> return val
              True ->
                  formatSumType name val
          _ -> fail "Failed to serialize at constructor level. Didn't get\
                     \ object or array. This shouldn't have happenend."
        where formatSumType name val
                  = let t = String $ T.pack name
                        cont = val
                    in return (object [("tag", t), ("contents", cont)])
                                        
    enterField (Field i s) False v =
            case s of
              "" ->
                return $ object [(T.pack $ show i, unPack v)]
              _  ->
                return $ object [(T.pack s, unPack v)]
    enterField (Field i s) True v =
            case s of
              "" ->
                return $ array [object [(T.pack $ show i, unPack v)]]
              _  ->
                return $ array [object [(T.pack s, unPack v)]]
    openRepetition length vs =
        return $ array (map unPack vs)
    writeValue v =
        let val =
              case v of
                PrimInt i    -> Number $ fromInteger $ toInteger i -- is there a better way to do this?
                PrimString s -> String $ T.pack s
                PrimChar c   -> String $ T.pack [c]
        in JSON val
    closeRepetition = return AT.emptyArray
    mult (JSON (Object v1)) (JSON (Object v2)) = 
                 let vals = object $ M.toList $ M.union v1 v2
                 in return vals
    mult (JSON (Array a1)) (JSON (Array a2)) =
                 let vals = array $ V.toList $ a1 V.++ a2
                 in return vals
    getFromCons ct obj@(JSON (Object o)) =
         do case fromObject o of
              [("tag", String tag), ("contents", cont)] ->
                  consLookup (T.unpack tag) (ct_index ct) (ct_name ct) obj
              o' -> fail ("Was expecting sumtype. Found: " ++ show o')
         where consLookup :: Format m
                          => String
                          -> Int
                          -> String
                          -> m Value
                          -> m (Parser Prim)
               consLookup t index x cont
                   = case t == x of
                       True ->
                           let fields = ct_fields ct in
                           case fields of
                             [] -> parseValue $ return Null
{- FIX THIS: SHOULDN'T RETURN EMPTY.-}
                             _ -> parseField (Field index t) (ct_fields ct) cont
                       _    -> return $ pure PrimUnit --- Error handling at top level
                                   
    parseField ft fields (JSON (Object o)) =
        let name = case ft_name ft of
                     "" -> show $ ft_index ft
                     _ -> ft_name ft
            msg = ("Couldn't parse at constructor level.\
                  \ No constructor with name " ++ name)
        in
        case fromObject o of
          []         -> fail msg
          [(key, val)] ->
              case (key == name) of
                True -> parseValue $ return val
                _    -> fail msg
          [("tag", String tag),("contents", cont)] ->
              case (T.unpack tag == name) of         
                True ->
                  case fields of
                    []     -> do a <- parseValue (return cont)
                                 return (a <|> empty)
                    (x:xs) -> parseField (head fields) (tail fields) (return cont)
                _ -> fail msg
          _ ->
              case M.lookup (T.pack $ name) o of
                Just val -> parseValue $ return val
                Nothing  -> fail msg            

--    parseField _ _ (JSON (Array a))
--        = parseRepetition a

    parseField _ _ val =
        parseValue val

    parseRepetition fields (JSON (Array a)) ---- (Not working)
        = let rep = V.toList a in
          case fields of
            []      -> map (parseValue . JSON) rep
            _       ->
                case V.toList a of
                  []      -> return $ return $ pure PrimUnit
                  xs  ->
                     map (parseField (head fields) (tail fields) . return) xs

    parseValue (JSON val)
       = case val of
           Number _ ->
               return $ parseNumber val
           String _ ->
               return $ parseString val
           Null ->
               return $ pure PrimUnit
           _ ->
               fail ("Failed to parse at value level. Expected primitive.\
                     \ Got: " ++ show val)
--    parseRepetition (JSON val)
--        = undefined

{-
resToMaybe :: Result a -> Maybe a
resToMaybe (Success a) = Just a
resToMaybe _ = Nothing
-}

parseNumber :: Value -> Parser Prim
parseNumber (Number i) = pure (PrimInt $ floor i)
parseNumber _ = fail "Couldn't parse number."

parseString :: Value -> Parser Prim
parseString (String s) = pure (PrimString $ T.unpack s)
parseString _ = fail "Couldn't parse string."

fromObject = map (first T.unpack) . M.toList

runJSONEncode :: SmartCopy a JSON => a -> IO ()
runJSONEncode a = print $ unPack (serialize a :: JSON Value)

runJSONParse :: (Data a, SmartCopy a JSON) => JSON EncodedType -> JSON a
runJSONParse v = runParser (unPack ((parse False False v))) fail return
