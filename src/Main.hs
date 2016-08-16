{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where 

import Data.Proxy 
import Data.Typeable 
import Data.Vinyl
import GHC.TypeLits 
import Servant.API 
import Text.Read 

type family APIToFields' (acc :: [(Symbol, *)]) layout where 
  APIToFields' acc (QueryParam s a :> b) = APIToFields' ( '(s, a) ': acc) b
  APIToFields' acc (a :> b) = APIToFields' acc b
  APIToFields' acc a = acc

type family Reverse (acc :: [(Symbol, *)]) ls where 
  Reverse acc '[] = acc
  Reverse acc (a ': ls) = Reverse (a ': acc) ls 

-- wrapper for simplicity
type family APIToFields layout where 
  APIToFields layout = Reverse '[] (APIToFields' '[] layout)

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

type API = QueryParam "a" Int :> QueryParam "b" Double :> QueryParam "c" Bool :> Get '[JSON] ()

main :: IO ()
main = do 
  let apiParser = parser (Proxy :: Proxy API) 
  print $ apiParser ["1", "42.0", "True"]
  print $ apiParser ["1.0", "42.0", "True"]
  print $ apiParser ["1.0", "42.0"]
  print $ apiParser ["1", "42.0", "True", ""]