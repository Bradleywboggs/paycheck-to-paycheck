module CreateBill where

import Bill
import BillInputHelpers

createBill :: IO (Bill)
createBill = do
  putStrLn "Enter the Payee's name: "
  name <- getLine
  
  putStrLn "Enter the payment amount: "
  amount <- getLine
  floatAmt <- return $ toFloat amount

  putStrLn "Enter the numeric payment Date (1 - 31): "
  date <- getLine
  intDate <- return $ toInt date
  
  putStrLn "Has the bill been paid this month? (y/n): "
  yesOrNo <- getLine
  paymentStatus <- return $ toPaymentStatus yesOrNo

  return (Bill floatAmt intDate name paymentStatus)



