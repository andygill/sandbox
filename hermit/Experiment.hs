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

class (ToJSON e, Show e) => Effector e where
   toEffectH :: (Monad m, m ~ IO) => e -> m ()

data TypedEffectH :: * where
  EffectH                :: Effector  e   => e                   -> TypedEffectH
{-
  RewriteLCoreTC         :: Rewriter  rr  => rr LCoreTC          -> TypedEffectH
  RewriteLCore           :: Rewriter  rr  => rr LCore            -> TypedEffectH
  TransformLCorePathBoxH :: Transformer t => t LCore LocalPathH -> TypedEffectH
-}

instance Show TypedEffectH where
  show (EffectH e) = show e
  
data Effects :: * where
  Effect1 :: Effects
  deriving Show
  
instance Effector Effects where
   toEffectH Effect1 = print ("effect(1)" :: String)
   
instance ToJSON Effects where
  toJSON Effect1 = enum "Effect" "Effect1"

instance ToJSON TypedEffectH where
  toJSON (EffectH e) = tagged "TypedEffectH" "EffectH" [toJSON e]

instance FromJSON Effects where
  parseJSON (Array a) = case V.toList a of
    ["Effect","Effect1"] -> return Effect1                
    _ -> mzero

instance FromJSON TypedEffectH where
  parseJSON (Array a) = case V.toList a of
    ["TypedEffectH","EffectH",e] -> parseJSON_Effector e EffectH
    _ -> mzero
    
parseJSON_Effector :: Value -> (forall e . Effector e => e -> k) -> Parser k
parseJSON_Effector (Array a) k = case V.toList a of
        ["Effect","Effect1"] -> return $ k Effect1
        _ -> mzero

------------------------------------

effect1 :: Effects
effect1 = Effect1

------------------------------------

enum :: String -> String -> Value
enum ty tag = toJSON [ty,tag]

tagged :: String -> String -> [Value] -> Value
tagged ty tag rest = Array $ V.fromList $ [toJSON ty, toJSON tag] ++ rest

        