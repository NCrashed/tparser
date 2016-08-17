{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where 

import Data.Proxy 
import Data.Typeable 
import Data.Vinyl
import GHC.TypeLits 
import Servant.API 
import Servant.Client
import Text.Read 

type family APIToFields layout :: [(Symbol, *)] where 
  APIToFields (QueryParam s a :> b) = '(s, a) ': APIToFields b 
  APIToFields (a :> b) = APIToFields b 
  APIToFields a = '[]

class ComParser api where 
  type ComParserRes api :: * 

  parser :: forall proxy . proxy api -> [String] -> Maybe (ComParserRes api)

instance (
    FieldRec (APIToFields b) ~ ComParserRes b
  , ComParser b
  , KnownSymbol s
  , Read a
  ) => ComParser (QueryParam s a :> b) where 

  type ComParserRes (QueryParam s a :> b) = FieldRec ( '(s, a) ': APIToFields b)

  parser _ ss = case ss of 
    [] -> Nothing 
    (s : ss') -> (:&)
      <$> (fmap Field (readMaybe s) :: Maybe (ElField '(s, a)) )
      <*> (parser (Proxy :: Proxy b) ss')

instance ComParser (Verb (a :: StdMethod) b c d) where 
  type ComParserRes (Verb a b c d) = FieldRec '[]
  parser _ _ = Just RNil 

class HasClient api => ConvertAPI api input where
  type FinalType api :: *

  convertClient :: Proxy api -> input -> Client api -> FinalType api

instance (
    ConvertAPI b (FieldRec as)
  , KnownSymbol s
  , ToHttpApiData a
  ) => ConvertAPI (QueryParam s a :> b) (FieldRec ( '(s, a) ': as)) where
  type FinalType (QueryParam s a :> b) = FinalType b

  convertClient _ (Field a :& as) cl = convertClient (Proxy :: Proxy b) as (cl $ Just a) 

instance (
    ReflectMethod a
  , MimeUnrender ct d
  ) => ConvertAPI (Verb (a :: StdMethod) b (ct ': cts) d) (FieldRec fields) where 
  type FinalType (Verb a b (ct ': cts) d) = Client (Verb a b (ct ': cts) d)

  convertClient _ _ cl = cl 

type API = QueryParam "a" Int :> QueryParam "b" Double :> QueryParam "c" Bool :> Get '[JSON] ()

convert :: ConvertAPI api input => Proxy api -> input -> FinalType api
convert prx input = convertClient prx input $ client prx

main :: IO ()
main = do 
  let 
    api = Proxy :: Proxy API
    apiParser = parser api
  print $ apiParser ["1", "42.0", "True"]
  print $ apiParser ["1.0", "42.0", "True"]
  print $ apiParser ["1.0", "42.0"]
  print $ apiParser ["1", "42.0", "True", ""]

  let Just input = apiParser ["1", "42.0", "True"]
  print $ typeOf (convert api input)