{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.Scoped.Internal
  ( Scoped (..),
    scoped,
    runScoped,
  )
where

import Polysemy (InterpreterFor, Member, Sem, raise, transform)
import Polysemy.Internal (Sem (Sem, runSem), liftSem, send)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, injWeaving)
import Polysemy.Resource (Resource, bracket)

data Scoped tok eff m a where
  Run :: tok -> eff m a -> Scoped tok eff m a
  InScope :: forall tok eff m a. (tok -> m a) -> Scoped tok eff m a

interpretH' ::
  (forall x. Weaving e (Sem (e ': r)) x -> Sem r x) ->
  Sem (e ': r) a ->
  Sem r a
interpretH' h sem = Sem $ \k -> runSem sem $ \u -> case decomp u of
  Right wav -> runSem (h wav) k
  Left g -> k $ hoist (interpretH' h) g

scoped :: forall tok eff r a. Member (Scoped tok eff) r => Sem (eff ': r) a -> Sem r a
scoped main = send $ InScope @tok @eff $ \token -> transform @eff (Run token) main

runScoped ::
  forall tok eff r a.
  Member Resource r =>
  (tok -> InterpreterFor eff r) ->
  Sem r tok ->
  (tok -> Sem r ()) ->
  Sem (Scoped tok eff ': r) a ->
  Sem r a
runScoped runner acquire release =
  let go :: forall x. Sem (Scoped tok eff ': r) x -> Sem r x
      go = interpretH' $ \(Weaving eff s wv ex ins) -> case eff of
        Run token act -> runner token (liftSem $ injWeaving $ Weaving act s (raise . go . wv) ex ins)
        InScope main -> do
          let main' token = go $ wv $ main token <$ s
          ex <$> bracket acquire release main'
   in go
