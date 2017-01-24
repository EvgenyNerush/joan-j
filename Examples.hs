module Examples where

import Control.Concurrent -- for _threadDelay_
import System.Hardware.WiringPi

-- applies a voltage to wiringPi pin 0 for 5 seconds
pin0HIGH :: IO ()
pin0HIGH = wiringPiSetup >>
           pinMode 0 OUTPUT >>
           digitalWrite 0 HIGH >>
           threadDelay 5000000 >>
           digitalWrite 0 LOW

repeatIO :: Int -> IO () -> IO ()
repeatIO n f
    | n <= 0    = return ()
    | otherwise = (f >> (repeatIO (n - 1) f))

-- blinks by a diode in pin 0 ten times in 5 seconds
pin0BLINK :: IO ()
pin0BLINK = wiringPiSetup >>
            pinMode 0 OUTPUT >>
            repeatIO 10 (digitalWrite 0 HIGH >>
                        threadDelay 250000 >>
                        digitalWrite 0 LOW >>
                        threadDelay 250000)
