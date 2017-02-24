{-# LANGUAGE TypeFamilies #-}

module Simulation (Processor (..), Simulation (..), Register (..), simulate) where

import Microprogram
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

data Processor = Processor { memory :: IntMap Value, registerBank :: IntMap Value }

instance Show Processor where
    show (Processor mem regs) = "Memory = " ++ show mem ++ "\nRegister bank = " ++ show regs ++ "\n"

data Simulation a = Simulation (Processor -> (a, Processor))

simulate :: Simulation a -> Processor -> (a, Processor)
simulate (Simulation f) processor = f processor

instance Monad Simulation where
    return a = Simulation $ \p -> (a, p)
    Simulation f >>= g = Simulation $ \p ->
                          let
                            (a, p') = f p
                            Simulation h = g a
                          in h p'

instance Applicative Simulation where
    pure  = return
    (<*>) =  ap

instance Functor Simulation where
    fmap = (<$>)

instance Microprogram Simulation where
    data Register Simulation = R Int | RegisterPC | RegisterOpcode
    pc     = RegisterPC
    opcode = RegisterOpcode

    readMemory address = Simulation $
        \(Processor mem regs) -> (Map.findWithDefault 0 address mem, Processor mem regs)

    writeMemory address value = Simulation $
        \(Processor mem regs) -> ((), Processor (Map.insert address value mem) regs)

    readRegister register = Simulation $
        \(Processor mem regs) -> (Map.findWithDefault 0 (registerID register) regs, Processor mem regs)

    writeRegister register value = Simulation $
        \(Processor mem regs) -> ((), Processor mem (Map.insert (registerID register) value regs))

registerID :: Register Simulation -> Int
registerID (R n)          = n
registerID RegisterPC     = -1
registerID RegisterOpcode = -2
