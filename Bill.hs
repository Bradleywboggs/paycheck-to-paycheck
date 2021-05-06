module Bill where

type Amount = Float
type Date = Int
type Name = String
data PaymentStatus = Paid | Unpaid deriving Eq

instance Show PaymentStatus where
  show paymentStatus = 
    case paymentStatus of 
      Paid -> "Paid"
      Unpaid -> "Unpaid"

data Bill = Bill 
  { paymentAmount :: Amount
  , paymentDate   :: Date
  , payeeName     :: Name
  , status        :: PaymentStatus
  } deriving (Eq)

instance Show Bill where
  show (Bill paymentAmount paymentDate payeeName status) = "Payee: " ++ payeeName ++ ", Amount: " ++ show paymentAmount ++ ", Status: " ++ show status

getPaymentAmount :: Bill -> Amount
getPaymentAmount (Bill amount date name status) = amount

getPaymentDate   :: Bill -> Date
getPaymentDate (Bill amount date name status ) = date

getPayeeName    :: Bill -> Name
getPayeeName (Bill amount date name status) = name
 
checkPaymentStatus :: Bill -> PaymentStatus
checkPaymentStatus (Bill amount date name status) = status

getUnpaidBills :: [Bill] -> [Bill]
getUnpaidBills = filter (\x -> Unpaid == checkPaymentStatus x)

getUnpaidTotal :: [Bill] -> Amount
getUnpaidTotal bills = sum $ getPaymentAmount <$> getUnpaidBills bills

payBill :: Bill -> Bill
payBill (Bill amount date name status) = Bill amount date name Paid

updateBills :: Bill -> [Bill] -> [Bill]
updateBills updatedBill bills =  updatedBill  : filter (\x -> getPayeeName updatedBill /= getPayeeName x) bills