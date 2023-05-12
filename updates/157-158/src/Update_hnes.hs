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
import qualified NextVersion.Emulator.Cartridge as NextVersion
import qualified NextVersion.Emulator.Mapper as NextVersion hiding (mirror)
import qualified NextVersion.Emulator.Mapper.Mapper2 as NextVersion.Mapper2
import qualified NextVersion.EntryPoint as NextVersion
import qualified NextVersion.Emulator.Nes as NextVersion
import qualified PreviousVersion.Emulator.Cartridge as PreviousVersion
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

instance Transformable PreviousVersion.Cartridge NextVersion.Cartridge where
  transform :: PreviousVersion.Cartridge -> IO (Maybe NextVersion.Cartridge)
  transform (PreviousVersion.Cartridge {..}) = do
    chrRom' <- transform chrRom
    prgRom' <- transform prgRom
    sram <- transform sRam
    prgBanks' <- transform prgBanks
    chrBanks' <- transform chrBanks
    prgBank1' <- transform prgBank1
    prgBank2' <- transform prgBank2
    chrBank1' <- transform chrBank1
    return $
      NextVersion.Cartridge
        <$> chrRom'
        <*> prgRom'
        <*> sram
        <*> prgBanks'
        <*> chrBanks'
        <*> prgBank1'
        <*> prgBank2'
        <*> chrBank1'
        <*> Just mirror
        <*> Just 2

makeMapper :: PreviousVersion.Cartridge -> IO (Maybe NextVersion.Mapper)
makeMapper cart = do
  cart' <- transform cart
  return $
    NextVersion.Mapper
      <$> (NextVersion.Mapper2.read <$> cart')
      <*> (NextVersion.Mapper2.write <$> cart')
      <*> (NextVersion.mirror <$> cart')
      <*> cart'
      <*> Just 2

instance Transformable PreviousVersion.Nes NextVersion.Nes where
  transform :: PreviousVersion.Nes -> IO (Maybe NextVersion.Nes)
  transform (PreviousVersion.Nes {..}) = do
    cpu' <- transform cpu
    ppu' <- transform ppu
    mapper <- makeMapper cart
    controller' <- transform controller
    return $
      NextVersion.Nes
        <$> cpu'
        <*> ppu'
        <*> mapper
        <*> controller'

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update
