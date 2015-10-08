{-# LANGUAGE TypeFamilies #-}

module Microprogram (
	module Control.Monad,
    Microprogram (..), Address (..), Value (..),
    increment, fetchArgument, fetchNextOpcode, load, store, jump
    ) where

import Control.Monad

-- Type synonyms allow to avoid pain when converting values to addresses
type Address = Int
type Value   = Int

class Monad m => Microprogram m where
    data Register m
    pc, opcode    :: Register m
    readMemory    :: Address -> m Value
    writeMemory   :: Address -> Value -> m ()
    readRegister  :: Register m -> m Value
    writeRegister :: Register m -> Value -> m ()

-- Increment the value stored in a register
increment :: Microprogram m => Register m -> m ()
increment register = do
    value <- readRegister register
    writeRegister register (value + 1) -- TODO: change flags

-- Increment the program counter and fetch the value it points to; used for
-- fetching instruction opcodes and immediate arguments
fetchArgument :: Microprogram m => m Value
fetchArgument = do
    increment pc
    address <- readRegister pc
    readMemory address

-- Fetch the next instruction opcode and store it in the opcode register
fetchNextOpcode :: Microprogram m => m ()
fetchNextOpcode = do
    value <- fetchArgument
    writeRegister opcode value

-- Load a register from memory from an address given by an immediate argument
load :: Microprogram m => Register m -> m ()
load register = do
    address <- fetchArgument
    value <- readMemory address
    writeRegister register value
    fetchNextOpcode

-- Store a register in memory at an address given by an immediate argument
store :: Microprogram m => Register m -> m ()
store register = do
    address <- fetchArgument
    value <- readRegister register
    writeMemory address value
    fetchNextOpcode

-- Unconditional jump to an address given by an immediate argument
jump :: Microprogram m => m ()
jump = do
    address <- fetchArgument
    writeRegister pc address
    nextOpcode <- readMemory address
    writeRegister opcode nextOpcode
