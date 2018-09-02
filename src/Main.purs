module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- ADT
data Fruit
  = Apple
  | Banana Int

mkApple :: Fruit
mkApple = Apple

mkBanana :: Int -> Fruit
mkBanana = Banana

-- Pattern matching
fruitString :: Fruit -> String
fruitString fruit =
  case fruit of
    Apple -> "Apple"
    Banana i -> "Banana " <> show i

-- Proxy
data Proxy ty = Proxy

-- type class
class MyLabel ty where
  myLabel :: Proxy ty -> String

-- instances
instance myLabelString :: MyLabel String where
  myLabel _ = "String"

instance myLabelFruit :: MyLabel Fruit where
  myLabel _ = "Fruit"

-- kinds
data Proxy2 (ty :: Type) = Proxy2
data ProxyF (f :: Type -> Type) = ProxyF

arrayProxyF :: ProxyF Array
arrayProxyF = ProxyF

foreign import kind MyList
foreign import data MyCons :: Type -> MyList -> MyList
foreign import data MyNil :: MyList

data ProxyMyList (xs :: MyList) = ProxyMyList

infixr 6 type MyCons as :::

class MyListLength (xs :: MyList) where
  myListLength :: ProxyMyList xs -> Int

instance myListLengthNil :: MyListLength MyNil where
  myListLength _ = 0

instance myListLengthCons ::
  ( MyListLength tail
  ) => MyListLength (MyCons ty tail) where
  myListLength _ =
    1 + myListLength (ProxyMyList :: ProxyMyList tail)

-- multiple parameters, with functional dependencies
class MyPair a b | a -> b

instance myPairString :: MyPair String Unit
instance myPairBoolean :: MyPair Boolean Int

myPair :: forall a b. MyPair a b => Proxy a -> Proxy b
myPair _ = Proxy

-- example of using this, maybe just take something from simple-json

main :: Effect Unit
main = do
  log "Hello world"
