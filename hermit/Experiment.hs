{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, RankNTypes,
             StandaloneDeriving, ConstraintKinds, FlexibleContexts, OverloadedStrings, ScopedTypeVariables
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

class (ToJSON e, Show e) => Effector e where
   toEffectH :: (Monad m, m ~ IO) => e -> m ()

class Rewriter r where
   toRewriter :: r a -> a -> IO a
   
--instance Rewriter rr => ToJSON (

data TypedEffectH :: * where
  EffectH                :: Effector  e                          => e -> TypedEffectH
  RewriteInt             :: (e ~ rr Int, Show e, ToJSON e, Rewriter rr)  => e -> TypedEffectH

------------------------------------------------------------------      

instance Show TypedEffectH where
  show (EffectH e) = show e
  show (RewriteInt e) = show e
  
instance ToJSON TypedEffectH where
  toJSON (EffectH e)    = tagged "TypedEffectH" "EffectH" [toJSON e]
  toJSON (RewriteInt e) = tagged "TypedEffectH" "RewriteInt" [toJSON e]

instance FromJSON TypedEffectH where
  parseJSON (Array a) = case V.toList a of
    ["TypedEffectH","EffectH",e]    -> parseEffector    e EffectH
    ["TypedEffectH","RewriteInt",e] -> parseRewriterInt e (Proxy :: Proxy Int) RewriteInt
    _ -> mzero

parseEffector :: Value -> (forall a . Effector a => a -> k) -> Parser k
parseEffector e k = 
      k <$> (parseJSON e :: Parser Effects    ) <|>
      k <$> (parseJSON e :: Parser MoreEffects)

parseRewriterInt :: forall a k . (FromJSON (RewriteMe a))
                 => Value -> Proxy a -> (forall rr . (Show (rr a), ToJSON (rr a), Rewriter rr) => rr a -> k) -> Parser k
parseRewriterInt e Proxy k = 
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

enum :: String -> String -> Value
enum ty tag = toJSON [ty,tag]

tagged :: String -> String -> [Value] -> Value
tagged ty tag rest = Array $ V.fromList $ [toJSON ty, toJSON tag] ++ rest

transmit :: TypedEffectH -> Result TypedEffectH
transmit e = fromJSON (toJSON e)

main = do
        print $ transmit $ EffectH $ effect1
        print $ transmit $ EffectH $ effect2
        print $ transmit $ RewriteInt $ theFirst

