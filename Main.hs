module Main where

import Bill
import BillInputHelpers

main :: IO (Bill)
main = do
  bill <- getBillInput
  return bill

getBillInput :: IO (Bill)
getBillInput = do
  putStrLn "Enter the payment amount"
  amount <- getLine
  floatAmt <- return $ toFloat amount

  putStrLn "Enter the Payee's name"
  name <- getLine

  putStrLn "Enter the numeric payment Date (0 - 31)"
  date <- getLine
  intDate <- return $ toInt date
  
  putStrLn "Has the bill been paid this month? (y/n)"
  yesOrNo <- getLine
  paymentStatus <- return $ toPaymentStatus yesOrNo

  return (Bill floatAmt intDate name paymentStatus)



