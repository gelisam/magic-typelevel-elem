{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# OPTIONS -Wno-name-shadowing #-}
module TypeLevel.Elem.Plugin (plugin) where

import GhcPlugins
import Plugin.MagicTyFam (magicTyFamPlugin, withStuckSemantics)

-- trace-debugging:
--import Debug.Trace
----traceShow ("foo", showSDocUnsafe $ ppr foo)


asList :: Type -> Maybe [Type]
asList t = do
  (tyCon, args) <- splitTyConApp_maybe t
  if | tyCon == promotedNilDataCon -> do
         pure []
     | tyCon == promotedConsDataCon -> do
         [_k, hd, tl] <- pure args
         (hd :) <$> asList tl
     | otherwise -> do
         Nothing

plugin :: Plugin
plugin = magicTyFamPlugin "typelevel-elem"
                          "TypeLevel.Elem"
                          "Elem" $
  withStuckSemantics $ \[x, xs] -> promoteBool <$> do
    case asList xs of
      Nothing -> pure False
      Just xs -> pure $ any (eqType x) xs


promoteBool :: Bool -> Type
promoteBool = \case
   False -> mkTyConApp promotedFalseDataCon []
   True  -> mkTyConApp promotedTrueDataCon []
