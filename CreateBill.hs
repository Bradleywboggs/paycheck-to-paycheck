module CreateBill where

import Bill
import BillInputHelpers

createBill :: IO Bill
createBill = do
  putStrLn "Enter the Payee's name: "
  name <- getLine
  
  putStrLn "Enter the payment amount: "
  amount <- getLine
  let floatAmt = toFloat amount

  putStrLn "Enter the numeric payment Date (1 - 31): "
  date <- getLine
  let intDate = toInt date
  
  putStrLn "Has the bill been paid this month? (y/n): "
  yesOrNo <- getLine
  let paymentStatus = toPaymentStatus yesOrNo

  return (Bill floatAmt intDate name paymentStatus)



