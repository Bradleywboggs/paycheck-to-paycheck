module Main where

import Bill
import Data.List (foldl')

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

nextPayDay :: Date -> Date
nextPayDay currentDate 
    | currentDate < 15 = 15
    | otherwise        = 30


main :: IO ()
main = do 
    let unpaidBeforeNextPayDay = filter (\bill -> getPaymentDate bill < nextPayDay currentDate) (getUnpaidBills bills)
    let totalOwed = sum $ getPaymentAmount <$> unpaidBeforeNextPayDay
    putStrLn "-----------Your current balance-----------" 
    print currentBalance
    putStrLn "-----------Your current unpaid bills------------"
    mapM_ print unpaidBeforeNextPayDay
    putStrLn "-----------Your total owed before your next pay check-----------"
    print totalOwed
    putStrLn "-----------Flexible Spending-----------"
    print $ currentBalance - totalOwed


