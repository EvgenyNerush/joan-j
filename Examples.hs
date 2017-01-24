module Examples where

import Control.Concurrent -- for _threadDelay_
import Data.Word
import System.Hardware.WiringPi

-- applies a voltage to wiringPi pin 0 for 5 seconds
pin0HIGH :: IO ()
pin0HIGH = wiringPiSetup >>
           pinMode 0 OUTPUT >>
           digitalWrite 0 HIGH >>
           threadDelay 5000000 >>
           digitalWrite 0 LOW

-- repeats _f_ _n_ times
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

maxLevel = 5000 :: Int

-- applies voltage to _pin_ for _level_ microseconds, then turns the pin off for maxLevel - level
-- microseconds
pwm :: Pin -> Int -> IO ()
pwm pin level = digitalWrite pin HIGH >>
                threadDelay level >>
                digitalWrite pin LOW >>
                threadDelay (maxLevel - level)

-- ioWith xs f applies _f_ to elements of _xs_, starting from the head
ioWith :: [a] -> (a -> IO ()) -> IO ()
ioWith [] _ = return ()
ioWith (x:xs) f = f x >>
                  ioWith xs f

-- blinks 15 s a diod connected to pin 0, "smoothly" changing its brightness; note that this function
-- doesn't use wiringPi PWM suitable only for pin 1
pin0PWM :: IO ()
pin0PWM = wiringPiSetup >>
          pinMode 0 OUTPUT >>
          ioWith xs (pwm 0)
              where xs = [floor $ maxLevel' * (sin $ pi * maxLevel' / 1000000 * fromIntegral i)**4
                          | i <- [0..n]]
                    maxLevel' = fromIntegral maxLevel
                    n = 15 * 1000000 `div` maxLevel
