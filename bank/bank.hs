module Bank where

newtype BankOp a = BankOp { runBankOpWithState :: Float -> (a, Float) }

runBankOp :: BankOp a -> a
runBankOp (BankOp op) = fst (op 0)

instance Functor BankOp where
    fmap f (BankOp op) = BankOp (\balance -> 
        let (result, newBalance) = op balance
        in (f result, newBalance))


instance Applicative BankOp where
    pure x = BankOp (\balance -> (x, balance))
    (BankOp f) <*> (BankOp x) = BankOp (\balance ->
        let (g, balance1) = f balance
            (y, balance2) = x balance1
        in (g y, balance2))


instance Monad BankOp where
    return = pure
    (BankOp op) >>= f = BankOp (\balance -> 
        let (result, newBalance) = op balance
            BankOp nextOp = f result
        in nextOp newBalance)

deposit :: Float -> BankOp ()
deposit amount = BankOp (\balance -> ((), balance + amount))

withdraw :: Float -> BankOp Float
withdraw amount = BankOp (\balance ->
    let actualWithdrawal = if balance - amount >= -100 then amount else balance + 100
    in (actualWithdrawal, balance - actualWithdrawal))

getBalance :: BankOp Float
getBalance = BankOp (\balance -> (balance, balance))
