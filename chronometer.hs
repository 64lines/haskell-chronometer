import Control.Concurrent

clear :: IO ()
clear = putStr "\ESC[2J"

trailZero :: Int -> String
trailZero num = if num < 10 then "0" ++ show num else show num

sleep :: Int -> IO ()
sleep x = threadDelay (x * 1000000)

formatTime :: Int -> Int -> Int -> String
formatTime hours minutes seconds = trailZero hours ++ ":" ++ trailZero minutes ++ ":" ++ trailZero seconds

base64time :: Int -> Int
base64time x = mod x 60

startChronometer :: Int -> Int -> Int -> IO ()
startChronometer hours minutes seconds = do
    putStrLn (formatTime hours minutes seconds)
    clear
    sleep 1
    let currentSeconds = base64time (seconds + 1)
    let currentMinutes = base64time (minutes + 1)
    let currentHours = hours + 1
    if minutes == 59 && seconds == 59 
        then startChronometer currentHours currentMinutes currentSeconds
    else if seconds == 59 
        then startChronometer hours currentMinutes currentSeconds
    else startChronometer hours minutes currentSeconds

main :: IO ()
main = do
  startChronometer 0 0 0