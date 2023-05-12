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
{-# LANGUAGE TupleSections #-}
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
import qualified NextVersion.Emulator.Cartridge as NextVersion
import qualified NextVersion.Emulator.Cartridge as PreviousVersion
import qualified NextVersion.Emulator.Mapper as NextVersion
import qualified NextVersion.Emulator.Mapper.Mapper2 as NextVersion.Mapper2
import qualified NextVersion.EntryPoint as NextVersion
import qualified NextVersion.Emulator.Nes as NextVersion
import qualified PreviousVersion.Emulator.Mapper as PreviousVersion
import qualified PreviousVersion.Emulator.Mapper.Mapper2 as PreviousVersion.Mapper2
import qualified PreviousVersion.EntryPoint as PreviousVersion
import qualified PreviousVersion.Emulator.Nes as PreviousVersion

instance (Transformable a b) => Transformable (IORef a) (IORef b) where
  transform :: IORef a -> IO (Maybe (IORef b))
  transform =
    readIORef >=> transform >=> \case
      Nothing -> return Nothing
      Just x -> Just <$> newIORef x

instance (Transformable [a] [b], Ord b) => Transformable (Set a) (Set b) where
  transform :: Set a -> IO (Maybe (Set b))
  transform = fmap (fmap Set.fromList) . transform . Set.toList

instance
  Transformable
    PreviousVersion.Mapper2.Mapper2
    NextVersion.Mapper2.Mapper2
  where
  transform ::
    PreviousVersion.Mapper2.Mapper2 -> IO (Maybe NextVersion.Mapper2.Mapper2)
  transform (PreviousVersion.Mapper2.Mapper2 {..}) = do
    cart' <- transform cart
    prgBank1' <- transform prgBank1
    prgBank2' <- transform prgBank2
    return $
      NextVersion.Mapper2.Mapper2
        <$> cart'
        <*> Just prgBanks
        <*> prgBank1'
        <*> prgBank2'

transformMapper ::
  PreviousVersion.Mapper ->
  IO (Maybe (NextVersion.Mapper, NextVersion.Cartridge))
transformMapper (PreviousVersion.Mapper _ _ _ d) =
  case (fromDynamic d :: Maybe PreviousVersion.Mapper2.Mapper2) of
    Nothing -> return Nothing
    Just m ->
      transform m >>= \case
        Nothing -> return Nothing
        Just (m' :: NextVersion.Mapper2.Mapper2) -> do
          cart' <- transform $ PreviousVersion.Mapper2.cart m
          return $
            (NextVersion.Mapper
               (NextVersion.Mapper2.read m')
               (NextVersion.Mapper2.write m')
               (toDyn m'),)
              <$> cart'

instance Transformable Int (IORef NextVersion.Mirror) where
  transform :: Int -> IO (Maybe (IORef NextVersion.Mirror))
  transform = fmap Just . newIORef . toEnum

instance Transformable PreviousVersion.Cartridge NextVersion.Cartridge where
  transformer :: Transformer PreviousVersion.Cartridge NextVersion.Cartridge
  transformer = genericReorderingTransformer

instance Transformable PreviousVersion.Nes NextVersion.Nes where
  transform :: PreviousVersion.Nes -> IO (Maybe NextVersion.Nes)
  transform (PreviousVersion.Nes {..}) = do
    cpu' <- transform cpu
    ppu' <- transform ppu
    mapperAndCart <- transformMapper mapper
    controller' <- transform controller
    return $
      NextVersion.Nes
        <$> cpu'
        <*> ppu'
        <*> (snd <$> mapperAndCart)
        <*> (fst <$> mapperAndCart)
        <*> controller'

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update
