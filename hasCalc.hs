import Data.Fixed
--Haskell Calculator 2020 William Sease

--DATA TYPES
data Token a = Const Double | VAR
                     | PLU | MIN
                     | MUL | DIV
                     | MOD | EXP
                     | LPA | RPA
               deriving(Eq)
--MAIN ROUTINE
main = do
    print ("**************************************")
    print ("* Simple Univariate Func. Calculator *")
    print ("*         William Sease 2020         *")
    print ("**************************************")
    print ("input a starting value...")
    par1 <- getLine
    print ("input an ending value...")
    par2 <- getLine
    print ("input the increment...")
    par3 <- getLine
    print ("*********************************************************")
    print ("*  input some function 'f(x)' (supports the following)  *")
    print ("*                ( ) + - * / % ^ x                      *")
    print ("* You can also use double/integer constants of any size *")
    print ("*                Spaces don't matter                    *")
    print ("*         There is no implicit multiplication           *")
    print ("*********************************************************")
    par4 <- getLine
    let par5 = (itp(tokenize ((words (splitterWrap (scrubSpaces par4)))++[")"])) [LPA])
    putStr(iter (read par1 :: Double) (read par2 :: Double) (read par3 :: Double) par4 par5)


--ITERATION TOOLS
--show :: Token a -> String
--show a = "Some token"

iter :: Double -> Double -> Double -> String -> [Token a] -> String
iter x y z p t 
    | x <= y = wrapEval x p t ++ (iter (x+z) y z p t)
    | x > y = "\ndone..."

wrapEval :: Double -> String -> [Token a] -> String
wrapEval x p t = ("\nf = " ++ p ++ " , x = " ++ (show x) ++ " f(x) = " ++ (show(evalu x [] t))) 

tokdub :: Token a -> Double
tokdub (Const a) = a

--First param is x, second is the stack, third is the token list
evalu :: Double -> [Double] -> [Token a] -> Double
evalu x [s] [] = s
evalu x s (t:ts)
    | t == PLU || t == MIN || t == MUL || t == DIV || t == MOD || t == EXP 
        = evalu x ((init(init s))++ [(realEval (last(init s)) (last s) t)]) ts
    | t == VAR = evalu x (s++[x]) ts
    | otherwise = evalu x (s++[(tokdub t)]) ts
           
realEval :: Double -> Double -> Token a -> Double
realEval op1 op2 func
    | (func == PLU) = op1 + op2
    | (func == MIN) = op1 - op2
    | (func == MUL) = op1 * op2
    | (func == DIV) = op1 / op2
    | (func == MOD) = mod' op1 op2
    | (func == EXP) = op1 ** op2
    | otherwise = 0.0
--HARD COMPONENTS
tokenize :: [String] -> [Token a]
tokenize [] = [] 
tokenize (s:ss)
    | s == "x" = VAR : tokenize ss
    | s == "(" = LPA : tokenize ss
    | s == ")" = RPA : tokenize ss
    | s == "*" = MUL : tokenize ss
    | s == "/" = DIV : tokenize ss
    | s == "+" = PLU : tokenize ss
    | s == "-" = MIN : tokenize ss
    | s == "%" = MOD : tokenize ss
    | s == "^" = EXP : tokenize ss
    | otherwise = Const (read s :: Double) : tokenize ss

--IN-TO-POST TOOLS
--First param is in, second param is stack
itp :: [Token a] -> [Token a] -> [Token a]
itp [] y = dumpStack y
itp (x:xs) y
    | (x == MUL || x == DIV || x == PLU || 
       x == MIN || x == MOD || x == EXP)
             = (getOperands x y) ++ (itp xs ((scrapeOperands x y)++[x])) --Is an operator.
    | (x == RPA) = (getOperands x y) ++ (itp xs (scrapeOperands x y))
    | (x == LPA) = itp xs (y ++ [x]) --Is a left Paranthesis
    | otherwise = x:(itp xs y) --Is an operand

--Takes an encountered operand and the current stack and returns what must be appended.
getOperands :: Token a -> [Token a] -> [Token a]
getOperands x [] = []
getOperands x y
    | (x == EXP) = if (last y == EXP)
                        then (last y):(getOperands x (init y))
                        else []
    | (x == MUL || x == DIV || x == MOD) = if (last y == EXP || last y == MUL || last y == DIV || last y == MOD)
                                    then (last y):(getOperands x (init y))
                                    else []
    | (x == PLU || x == MIN) = if (last y == EXP || last y == MUL || last y == DIV || last y == MOD || last y == MIN || last y == PLU)
                                    then (last y):(getOperands x (init y))
                                    else []
    | (x == RPA) = if (last y == LPA) 
            then []
            else (last y):(getOperands x (init y))                             

--Takes an encountered operand and the current stack and returns the new stack.
scrapeOperands :: Token a -> [Token a] -> [Token a]
scrapeOperands x [] = []
scrapeOperands x y
    | (x == EXP) = if (last y == EXP)
                        then scrapeOperands x (init y)
                        else y
    | (x == MUL || x == DIV || x == MOD) = if (last y == EXP || last y == MUL || last y == DIV || last y == MOD)
                                    then scrapeOperands x (init y)
                                    else y
    | (x == PLU || x == MIN) = if (last y == EXP || last y == MUL || last y == DIV || last y == MOD || last y == MIN || last y == PLU)
                                    then scrapeOperands x (init y)
                                    else y
    | (x == RPA) = if (last y == LPA) 
            then (init y)
            else scrapeOperands x (init y)

dumpStack :: [Token a] -> [Token a]
dumpStack [] = []
dumpStack x = (last [x]) ++ (dumpStack(init x))

--HELPER FUNCTIONS (NOT RELIED ON)
testTok :: String -> String
testTok x = tokToString(itp((tokenize (words x))++[RPA]) [LPA])

tokToString :: [Token a] -> String
tokToString [] = ""
tokToString (x:xs)
    | (x == PLU) = "PLU " ++ (tokToString xs)
    | (x == MIN) = "MIN " ++ (tokToString xs)
    | (x == MUL) = "MUL " ++ (tokToString xs)
    | (x == DIV) = "DIV " ++ (tokToString xs)
    | (x == MOD) = "MOD " ++ (tokToString xs)
    | (x == EXP) = "EXP " ++ (tokToString xs)
    | (x == LPA) = "LPA " ++ (tokToString xs)
    | (x == RPA) = "RPA " ++ (tokToString xs)
    | (x == VAR) = "VAR " ++ (tokToString xs)
    | otherwise = show(tokdub x) ++ " " ++ (tokToString xs)


--STRING MANIP FUNCTIONS
scrubSpaces :: String -> String
scrubSpaces x = filter (/=' ') x

splitterWrap :: String -> String
splitterWrap (x:xs)
    | x == '(' || x == ')' || x == '+' || x == '-' || x == '*' || x == '/' || x == '%' || x == '^' = splitterOperator (x:xs) 
    | otherwise = splitterNumeric (x:xs)

splitterNumeric :: String -> String
splitterNumeric [x] = [x]
splitterNumeric (x:xs:xss) 
    | xs == '(' || xs == ')' || xs == '+' || xs == '-' || xs == '*' || xs == '/' || xs == '%' || xs == '^' = (x : ' ' : splitterOperator(xs:xss))
    | otherwise = x:(splitterNumeric (xs:xss))

splitterOperator :: String -> String
splitterOperator [x] = [x]
splitterOperator (x:xs:xss)
    | xs == '(' || xs == ')' || xs == '+' || xs == '-' || xs == '*' || xs == '/' || xs == '%' || xs == '^' = x : ' ' : splitterOperator(xs:xss)
    | otherwise = x : ' ' : splitterNumeric(xs:xss)
