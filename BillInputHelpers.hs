module BillInputHelpers where

import Bill
import Text.Read
import Data.Maybe

toFloat :: String -> Float
toFloat stringVal = fromMaybe 0.0 (readMaybe stringVal::Maybe Float)

toInt :: String -> Int
toInt stringVal = fromMaybe 1 (readMaybe stringVal::Maybe Int)

toPaymentStatus :: String -> PaymentStatus
toPaymentStatus stringVal = if stringVal == "n" then Unpaid else Paid