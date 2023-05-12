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
import Data.Dynamic
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Lowarn
import Lowarn.TH
import Lowarn.Transformer
import qualified NextVersion.Emulator.Mapper as NextVersion
import qualified NextVersion.Emulator.Mapper.Mapper2 as NextVersion.Mapper2
import qualified NextVersion.EntryPoint as NextVersion
import qualified PreviousVersion.Emulator.Mapper as PreviousVersion
import qualified PreviousVersion.Emulator.Mapper.Mapper2 as PreviousVersion.Mapper2
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

instance Transformable PreviousVersion.Mapper NextVersion.Mapper where
  transform :: PreviousVersion.Mapper -> IO (Maybe NextVersion.Mapper)
  transform (PreviousVersion.Mapper _ _ mirror d) =
    case (fromDynamic d :: Maybe PreviousVersion.Mapper2.Mapper2) of
      Nothing -> return Nothing
      Just m ->
        transform m >>= \case
          Nothing -> return Nothing
          Just (m' :: NextVersion.Mapper2.Mapper2) ->
            return $
              Just $
                NextVersion.Mapper
                  (NextVersion.Mapper2.read m')
                  (NextVersion.Mapper2.write m')
                  mirror
                  (toDyn m')

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update
