import Microprogram
import qualified Simulation as S
import qualified Listing as L

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

program :: Microprogram m => Register m -> Register m -> m ()
program reg0 reg1 = do
    writeMemory 100 200
    writeMemory 100 200
    writeRegister reg0 1234

    let programAddress = 10000
    writeRegister pc programAddress
    writeMemory (programAddress + 1) 100
    writeMemory (programAddress + 2) 222

    load reg1

programSimulation :: S.Simulation ()
programSimulation =
    program (S.R 0) (S.R 1)

programListing :: L.Listing ()
programListing =
    program (L.Register "0") (L.Register "1")

processor = S.Processor (Map.empty) (Map.empty)

main = do
    putStrLn "==============="
    putStrLn "=== Listing ==="
    putStrLn "==============="
    putStrLn $ L.listing programListing

    putStrLn "=================="
    putStrLn "=== Simulation ==="
    putStrLn "=================="
    putStrLn $ "\nInitial processor:\n" ++ show processor ++ "\n"
    let (value, newProcessor) = S.simulate programSimulation processor
    putStrLn $ "Value = " ++ show value ++ "\n"
    putStrLn $ "New processor:\n" ++ show newProcessor ++ "\n"
