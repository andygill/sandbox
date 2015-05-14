{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, RankNTypes,
             StandaloneDeriving, ConstraintKinds, FlexibleContexts, OverloadedStrings
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

data TypedEffectH :: * where
  EffectH                :: Effector  e   => e                   -> TypedEffectH
  RewriteInt             :: Rewriter  rr  => rr Int              -> TypedEffectH
{-
    RewriteLCore           :: Rewriter  rr  => rr LCore            -> TypedEffectH
  TransformLCorePathBoxH :: Transformer t => t LCore LocalPathH -> TypedEffectH
-}

------------------------------------------------------------------      

instance Show TypedEffectH where
  show (EffectH e) = show e
  
instance ToJSON TypedEffectH where
  toJSON (EffectH e) = tagged "TypedEffectH" "EffectH" [toJSON e]

instance FromJSON TypedEffectH where
  parseJSON (Array a) = case V.toList a of
    ["TypedEffectH","EffectH",e] -> 
      EffectH <$> (parseJSON e :: Parser Effects    ) <|>
      EffectH <$> (parseJSON e :: Parser MoreEffects)
    _ -> mzero

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
    

effect1 :: Effects
effect1 = Effect1

------------------------------------

enum :: String -> String -> Value
enum ty tag = toJSON [ty,tag]

tagged :: String -> String -> [Value] -> Value
tagged ty tag rest = Array $ V.fromList $ [toJSON ty, toJSON tag] ++ rest

transmit :: TypedEffectH -> Result TypedEffectH
transmit e = fromJSON (toJSON e)

main = do
        print $ transmit $ EffectH $ effect1

