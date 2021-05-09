module Main where

import Bill
import Data.List (foldl')
import Data.Time
-- TODO: fetch ACTUAL banking Data
--   - SPIKE to figure out an API to get the data
--   - implement the API request service
--   - create an API -> to PaycheckToPaycheck Data wrapper
--         - JSON decoder?
--         - Probably some custom logic to figure out the info I need based on what's given
-- TODO: integrate with Postgres
--    - set up tables
--    - make a PaycheckToPaycheck Data -> Database Wrapper
--    - set up "Repository" service to fetch and write data to the db
-- TODO: design and build an actual interface
--     - could be a TUI for now? Maybe use Elm to create a simple front end?
-- TODO: incorporate real datetime stuff

bills :: [Bill]
bills = 
    [
    Bill 100 1 "Credit Card" Unpaid,
    Bill 630 1 "Car" Unpaid,
    Bill 500 1 "House" Unpaid,
    Bill 200 15 "Insurance" Unpaid
    ]

currentBalance :: Amount
currentBalance = 2035.67

currentDate :: Date
currentDate = 5

nextPayDay paydays currentDate 
    | currentDate < fst paydays = fst paydays
    | otherwise                 = snd paydays

getPayDays year month 
    | month `elem` [1,3,5,7,8,10,12] = (15, 31)
    | month == 2 && isLeapYear year  = (15, 29)
    | month == 2                     = (15, 28)
    | otherwise                      = (15, 30)

main :: IO ()
main = do 
    (year, month, currentDate) <- toGregorian . utctDay <$> getCurrentTime
    let paydays = getPayDays year month
    print paydays
    let unpaidBeforeNextPayDay = filter (\bill -> getPaymentDate bill < nextPayDay paydays currentDate) (getUnpaidBills bills)
    let totalOwed = sum $ getPaymentAmount <$> unpaidBeforeNextPayDay
    putStrLn "-----------Your current balance-----------" 
    print currentBalance
    putStrLn "-----------Your current unpaid bills------------"
    mapM_ print unpaidBeforeNextPayDay
    putStrLn "-----------Your total owed before your next pay check-----------"
    print totalOwed
    putStrLn "-----------Flexible Spending-----------"
    print $ currentBalance - totalOwed


