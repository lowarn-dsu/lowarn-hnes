{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_hnes () where

import Control.Monad
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Lowarn
import Lowarn.TH
import Lowarn.Transformer
import qualified NextVersion.EntryPoint as NextVersion
import qualified PreviousVersion.EntryPoint as PreviousVersion

instance (Transformable a b) => Transformable (IORef a) (IORef b) where
  transform :: IORef a -> IO (Maybe (IORef b))
  transform =
    readIORef >=> transform >=> \case
      Nothing -> return Nothing
      Just x -> Just <$> newIORef x

instance (Transformable [a] [b], Ord b) => Transformable (Set a) (Set b) where
  transform :: Set a -> IO (Maybe (Set b))
  transform = fmap (fmap Set.fromList) . transform . Set.toList

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update
