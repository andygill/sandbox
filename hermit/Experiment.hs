{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, RankNTypes, 
             StandaloneDeriving, ConstraintKinds, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, LiberalTypeSynonyms
            ,  UndecidableInstances
 #-}
import Data.Aeson
import Data.Text
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Monad
import Data.Aeson.Types
import Data.Monoid
import Data.Proxy
import Control.Applicative

import GHC.Exts

{- We're doing this all wrong! We do not need the src and dest to be the same structure -}

class (ToJSON e) => Effector e where
   toEffectH :: (Monad m, m ~ IO) => e -> m ()

class Rewriter r where
   toRewriter :: r a -> a -> IO a
   
data TypedEffectH :: (* -> Constraint) -> * where
  EffectH                :: (c e, Effector  e)              => e -> TypedEffectH c
  RewriteInt             :: (c e, Rewriter rr, e ~ rr Int)  => e -> TypedEffectH c
  RewriteBool            :: (c e, Rewriter rr, e ~ rr Bool) => e -> TypedEffectH c

------------------------------------------------------------------      

--newtype RewriteH :: * -> * where
--  RewriteH


------------------------------------------------------------------      

class (Show a, ToJSON a, FromJSON a) => C a
instance (Show a, ToJSON a, FromJSON a) => C a

------------------------------------------------------------------      

instance Show (TypedEffectH C) where
  show (EffectH e) = show e
  show (RewriteInt e) = show e
  
instance ToJSON (TypedEffectH C) where
  toJSON (EffectH e)    = tagged "TypedEffectH" "EffectH"    [toJSON e]
  toJSON (RewriteInt e) = tagged "TypedEffectH" "RewriteInt" [toJSON e]

instance FromJSON (TypedEffectH C) where
  parseJSON (Array a) = case V.toList a of
    ["TypedEffectH","EffectH",e]     -> parseEffector e EffectH
    ["TypedEffectH","RewriteInt",e]  -> parseRewriter e RewriteInt
    ["TypedEffectH","RewriteBool",e] -> parseRewriter e RewriteBool
    _ -> mzero

parseEffector :: Value -> (forall a . (C a, Effector a) => a -> k) -> Parser k
parseEffector e k = 
      k <$> (parseJSON e :: Parser Effects    ) <|>
      k <$> (parseJSON e :: Parser MoreEffects)


parseRewriter :: forall a k . (FromJSON (RewriteMe a))
                 => Value -> (forall rr . (C (rr a), Rewriter rr) => rr a -> k) -> Parser k
parseRewriter e k = 
      k <$> (parseJSON e :: Parser (RewriteMe a))

------------------------------------------------------------------      

data Effects :: * where
  Effect1 :: Effects
  deriving Show
  
instance Effector Effects where
   toEffectH Effect1 = print ("effect(1)" :: String)
   
instance ToJSON Effects where
  toJSON Effect1 = enum "Effects" "Effect1"

instance FromJSON Effects where
  parseJSON (Array a) = case V.toList a of
    ["Effects","Effect1"] -> return Effect1                
    _ -> mzero
    

------------------------------------

data MoreEffects :: * where
  Effect2 :: MoreEffects
  deriving Show
  
instance Effector MoreEffects where
   toEffectH Effect2 = print ("effect(2)" :: String)
   
instance ToJSON MoreEffects where
  toJSON Effect2 = enum "MoreEffects" "Effect2"

instance FromJSON MoreEffects where
  parseJSON (Array a) = case V.toList a of
    ["MoreEffects","Effect2"] -> return Effect2               
    _ -> mzero

------------------------------------------------------------------      
    
data RewriteMe :: * -> * where
  RewriteTheFirst  :: RewriteMe Int
  RewriteTheSecond :: String -> RewriteMe Bool

deriving instance Show (RewriteMe a)

instance Rewriter RewriteMe where
   toRewriter RewriteTheFirst        n = return $ n + 1
   toRewriter (RewriteTheSecond msg) b = do
           print msg
           return $ not b

instance FromJSON (RewriteMe Int) where
  parseJSON (Array e) = case V.toList e of
    ["RewriteMe","RewriteTheFirst"] -> return $ RewriteTheFirst
    _ -> mzero

instance FromJSON (RewriteMe Bool) where
  parseJSON (Array e) = case V.toList e of
    ["RewriteMe","RewriteTheFirst",e] -> liftM RewriteTheSecond $ parseJSON e
    _ -> mzero

instance ToJSON (RewriteMe a) where
  toJSON  RewriteTheFirst        = enum "RewriteMe" "RewriteTheFirst"
  toJSON (RewriteTheSecond str) = tagged "RewriteMe" "RewriteTheSecond" [toJSON str]

------------------------------------------------------------------      

effect1 :: Effects
effect1 = Effect1

effect2 :: MoreEffects
effect2 = Effect2

theFirst :: RewriteMe Int
theFirst = RewriteTheFirst

theSecond :: String -> RewriteMe Bool
theSecond = RewriteTheSecond

------------------------------------
{-
class Perform p where
  perform :: p -> IO ()
  
instance Effector e => Perform e where
   perform e = print ("Effector",toJSON e)

instance (Rewriter rr, e ~ rr Int) => Perform e where
   perform e = print ("Effector",toJSON e)
-}
--  EffectH                :: (c e, Effector  e)              => e -> TypedEffectH c
--  RewriteInt             :: (c e, Rewriter rr, e ~ rr Int)  => e -> TypedEffectH c
--  RewriteBool            :: (c e, Rewriter rr, e ~ rr Bool) => e -> TypedEffectH c

------------------------------------

enum :: String -> String -> Value
enum ty tag = toJSON [ty,tag]

tagged :: String -> String -> [Value] -> Value
tagged ty tag rest = Array $ V.fromList $ [toJSON ty, toJSON tag] ++ rest

transmit :: TypedEffectH C -> Result (TypedEffectH C)
transmit e = fromJSON (toJSON e)

main = do
        print $ transmit $ EffectH $ effect1
        print $ transmit $ EffectH $ effect2
        print $ transmit $ RewriteInt $ theFirst
