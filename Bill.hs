module Bill where

type Amount = Float
type Date = Int
type Name = String
data PaymentStatus = Paid | Unpaid deriving (Eq, Show)

data Bill = Bill 
  { paymentAmount :: Amount
  , paymentDate   :: Date
  , payeeName     :: Name
  , status        :: PaymentStatus
  } deriving Show

getPaymentAmount :: Bill -> Amount
getPaymentAmount (Bill amount date name status) = amount

getPaymentDate   :: Bill -> Date
getPaymentDate (Bill amount date name status ) = date

getPayeeName    :: Bill -> Name
getPayeeName (Bill amount date name status) = name
 
checkPaymentStatus :: Bill -> PaymentStatus
checkPaymentStatus (Bill amount date name status) = status

getUnpaidBills :: [Bill] -> [Bill]
getUnpaidBills bills = filter (\x -> Unpaid == (checkPaymentStatus x)) bills

getUnpaidTotal :: [Bill] -> Amount
getUnpaidTotal bills = sum $ fmap getPaymentAmount $ getUnpaidBills bills

payBill :: Bill -> Bill
payBill (Bill amount date name status) = (Bill amount date name Paid)

updateBills :: Bill -> [Bill] -> [Bill]
updateBills updatedBill bills =  updatedBill  : (filter (\x -> (getPayeeName updatedBill) /= (getPayeeName x)) bills)