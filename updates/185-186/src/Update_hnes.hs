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

import Control.Applicative
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
import qualified NextVersion.Emulator.Mapper.Mapper3 as NextVersion.Mapper3
import qualified NextVersion.Emulator.Mapper.Mapper7 as NextVersion.Mapper7
import qualified NextVersion.EntryPoint as NextVersion
import qualified NextVersion.Emulator.Nes as NextVersion
import qualified PreviousVersion.Emulator.Mapper as PreviousVersion
import qualified PreviousVersion.Emulator.Mapper.Mapper2 as PreviousVersion.Mapper2
import qualified PreviousVersion.Emulator.Mapper.Mapper3 as PreviousVersion.Mapper3
import qualified PreviousVersion.Emulator.Mapper.Mapper7 as PreviousVersion.Mapper7
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

instance Transformable PreviousVersion.Mapper NextVersion.Mapper where
  transform :: PreviousVersion.Mapper -> IO (Maybe NextVersion.Mapper)
  transform (PreviousVersion.Mapper _ _ d) =
    asum
      <$> sequence
        [ case (fromDynamic d :: Maybe PreviousVersion.Mapper2.Mapper2) of
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
                        (toDyn m'),
          case (fromDynamic d :: Maybe PreviousVersion.Mapper3.Mapper3) of
            Nothing -> return Nothing
            Just m ->
              transform m >>= \case
                Nothing -> return Nothing
                Just (m' :: NextVersion.Mapper3.Mapper3) ->
                  return $
                    Just $
                      NextVersion.Mapper
                        (NextVersion.Mapper3.read m')
                        (NextVersion.Mapper3.write m')
                        (toDyn m'),
          case (fromDynamic d :: Maybe PreviousVersion.Mapper7.Mapper7) of
            Nothing -> return Nothing
            Just m ->
              transform m >>= \case
                Nothing -> return Nothing
                Just (m' :: NextVersion.Mapper7.Mapper7) ->
                  return $
                    Just $
                      NextVersion.Mapper
                        (NextVersion.Mapper7.read m')
                        (NextVersion.Mapper7.write m')
                        (toDyn m')
        ]

instance Transformable PreviousVersion.PPU NextVersion.PPU where
  transform :: PreviousVersion.PPU -> IO (Maybe NextVersion.PPU)
  transform (PreviousVersion.PPU {..}) = do
    ppuCycles' <- transform ppuCycles
    scanline' <- transform scanline
    frameCount' <- transform frameCount
    writeToggle' <- transform writeToggle
    ppuRegister' <- transform ppuRegister
    oddFrame <- newIORef False
    oamData' <- transform oamData
    nameTableData' <- transform nameTableData
    paletteData' <- transform paletteData
    screen' <- transform screen
    currentVramAddress' <- transform currentVramAddress
    tempVramAddress' <- transform tempVramAddress
    oamAddress' <- transform oamAddress
    nmiOutput' <- transform nmiOutput
    nmiOccurred' <- transform nmiOccurred
    nmiDelay' <- transform nmiDelay
    nmiPrevious' <- transform nmiPrevious
    nameTable' <- transform nameTable
    incrementMode' <- transform incrementMode
    spriteTable' <- transform spriteTable
    bgTable' <- transform bgTable
    spriteSize' <- transform spriteSize
    colorMode' <- transform colorMode
    leftBgVisibility' <- transform leftBgVisibility
    leftSpritesVisibility' <- transform leftSpritesVisibility
    bgVisibility' <- transform bgVisibility
    spriteVisibility' <- transform spriteVisibility
    intensifyReds' <- transform intensifyReds
    intensifyGreens' <- transform intensifyGreens
    intensifyBlues' <- transform intensifyBlues
    spriteOverflow' <- transform spriteOverflow
    spriteZeroHit' <- transform spriteZeroHit
    fineX' <- transform fineX
    dataV' <- transform dataV
    nameTableByte' <- transform nameTableByte
    attrTableByte' <- transform attrTableByte
    loTileByte' <- transform loTileByte
    hiTileByte' <- transform hiTileByte
    tileData' <- transform tileData
    sprites' <- transform sprites

    return $
      NextVersion.PPU
        <$> ppuCycles'
        <*> scanline'
        <*> frameCount'
        <*> writeToggle'
        <*> ppuRegister'
        <*> Just oddFrame
        <*> oamData'
        <*> nameTableData'
        <*> paletteData'
        <*> screen'
        <*> currentVramAddress'
        <*> tempVramAddress'
        <*> oamAddress'
        <*> nmiOutput'
        <*> nmiOccurred'
        <*> nmiDelay'
        <*> nmiPrevious'
        <*> nameTable'
        <*> incrementMode'
        <*> spriteTable'
        <*> bgTable'
        <*> spriteSize'
        <*> colorMode'
        <*> leftBgVisibility'
        <*> leftSpritesVisibility'
        <*> bgVisibility'
        <*> spriteVisibility'
        <*> intensifyReds'
        <*> intensifyGreens'
        <*> intensifyBlues'
        <*> spriteOverflow'
        <*> spriteZeroHit'
        <*> fineX'
        <*> dataV'
        <*> nameTableByte'
        <*> attrTableByte'
        <*> loTileByte'
        <*> hiTileByte'
        <*> tileData'
        <*> sprites'

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update