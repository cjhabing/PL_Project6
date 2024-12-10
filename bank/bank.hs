module Bank where

-- Define the BankOp type to encapsulate stateful computations
newtype BankOp a = BankOp { runBankOpWithState :: Float -> (a, Float) }

-- Define how to run a BankOp, starting from an initial balance of 0
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = fst (op 0)

-- Functor instance for BankOp
instance Functor BankOp where
    fmap f (BankOp op) = BankOp (\balance -> 
        let (result, newBalance) = op balance
        in (f result, newBalance))

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure x = BankOp (\balance -> (x, balance))
    (BankOp f) <*> (BankOp x) = BankOp (\balance ->
        let (g, balance1) = f balance
            (y, balance2) = x balance1
        in (g y, balance2))

-- Monad instance for BankOp
instance Monad BankOp where
    return = pure
    (BankOp op) >>= f = BankOp (\balance -> 
        let (result, newBalance) = op balance
            BankOp nextOp = f result
        in nextOp newBalance)

-- Deposit a specified amount
deposit :: Float -> BankOp ()
deposit amount = BankOp (\balance -> ((), balance + amount))

-- Withdraw a specified amount, allowing overdrafts up to $100
withdraw :: Float -> BankOp Float
withdraw amount = BankOp (\balance ->
    let actualWithdrawal = if balance - amount >= -100 then amount else balance + 100
    in (actualWithdrawal, balance - actualWithdrawal))

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp (\balance -> (balance, balance))
