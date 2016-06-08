{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emulator.IO where

import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans (lift, MonadIO)
import Data.Word (Word16)

import Emulator.Monad
import Memory

newtype IOEmulator a = IOEmulator (ReaderT (Memory RealWorld, Address -> Word16 -> (Address -> IO Word16) -> IO ()) IO a)
    deriving (Monad, MonadIO)

instance Emulator IOEmulator where
    load address = IOEmulator $ do
        (memory, _) <- ask
        lift $ stToIO $ Memory.read memory address

    store address word = IOEmulator $ do
        (memory, display) <- ask
        lift $ stToIO $ Memory.write memory address word
        lift $ display address word (\x -> stToIO $ Memory.read memory x)

    swap address address' = IOEmulator $ do
        (memory, _) <- ask
        lift $ stToIO $ Memory.swap memory address address'

runIOEmulator :: (Address -> Word16 -> (Address -> IO Word16) -> IO ()) -> IOEmulator a -> IO a
runIOEmulator display (IOEmulator reader) = do
    memory <- stToIO Memory.new
    runReaderT reader (memory, display)
