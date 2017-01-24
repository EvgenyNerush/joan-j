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
