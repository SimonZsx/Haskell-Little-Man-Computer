module Assignment3 where

import Parsing
import Control.Monad.State


-- You should not modify this definition. But you can add a deriving clause if you want.
type Label = String
type Program = [(Maybe Label, Instruction)]

data Instruction
    = ADD Label
    | SUB Label
    | STA Label
    | LDA Label
    | BRA Label
    | BRZ Label
    | BRP Label
    | DAT Int
    | INP
    | OUT
    | HLT deriving (Show,Read)



--Use Read to parse Instruction. May cause an exception if the instruction is not in the definition. Can be solved by catch exception and print an error
instruction :: Parser Instruction
instruction = P ( \inp -> --firstly ignore the comment 
                        let inter = parse (sepBy (many (alphanum +++ char ' ')) (symbol "//")) inp in
                          let inp' = head$fst$head inter
                              out' = snd$head inter
                          in 
                          case parse (sepBy (many alphanum) (char ' ')) inp' of 
                            [([instr],_)]       -> 
                                                    case instr of 
                                                      "DAT" -> [(DAT 0,out')] 
                                                      x   -> [(read x :: Instruction,out')] 
                                                      _ -> []
                            [([instr,label],_)] ->
                                                    case instr of 
                                                      "DAT" -> [(DAT (read label::Int),out')]
                                                      _ -> [(read (instr ++ " \"" ++ label ++ "\"") :: Instruction ,out')]
                            _ -> error "Parse error" )


line :: Parser (Maybe Label, Instruction)
line =P ( \inp -> case parse (optionMaybe (many upper)) inp of
                     [(Just "",out)] -> case parse (token instruction) out of
                                            [(instr,out1)]->[((Nothing,instr),out1)]
                                            _ -> []
                     [(label,out)] -> case parse (token instruction) out of
                                            [(instr,out2)]->[((label,instr),out2)] 
                                            _ -> [] )
                    
parseLMC :: String -> Program
parseLMC s = case parse (sepBy line (char '\n')) s of
               [(p, "")] -> p
               _ -> error "Parse error"

-- 
showProgram :: Program -> String
showProgram [] = ""  
showProgram (x:xs) = (case x of
                       (Nothing,instr) -> case instr of 
                                              DAT _ ->  " " ++ show instr
                                              _ -> " " ++ fst (head (parse (many letter) (show instr))) ++ " " ++ read (snd ( head (parse (many letter) (show instr))))::String 
                       (Just a,instr) -> case instr of
                                              DAT _ ->a ++ " " ++ show instr
                                              _ -> a ++ " " ++ fst ( head (parse (many letter) (show instr))) ++" "++ read ( snd (head (parse (many letter) (show instr))))::String )
                     ++ "\n" ++ showProgram xs

type Addr = Int
type Accumulator = Maybe Int
type PC = Int
type Mailbox = (String, Int)

data Env
    = Env
    { mailboxes :: [(String, Int)]
    , accumulator :: Accumulator
    , pc :: Addr -- program counter
    , instructions :: [Instruction]
    , labelAddr :: [(String, Int)]
    } deriving (Show) -- the derving show is just for testing purpose

-- 
initMailboxes :: Program -> [Mailbox]
initMailboxes [] = []
initMailboxes (x:xs) = case x of
                        (Just label, DAT num) -> (label,num): initMailboxes xs
                        _ -> initMailboxes xs


initLabelAddr :: [Maybe Label] -> [(Label, Addr)]
initLabelAddr [] = []
initLabelAddr xs = labeladdrs ++ [(label,num)]
                       where label = case last xs of
                                        Just la -> la
                                        Nothing -> ""  -- May use fromMaybe "" (last xs) but new import is not allowed
                             num = length (init xs)
                             labeladdrs = initLabelAddr (init xs)


mkInitEnv :: Program -> Env
mkInitEnv prog = Env mailB Nothing 0 instrs labelA
                    where mailB = initMailboxes prog
                          instrs = foldr (\x xs -> snd x:xs) [] prog
                          labelA = initLabelAddr $ foldr (\x xs -> fst x:xs) [] prog

type IOEnv = StateT Env IO

-- 
decode :: Instruction  -> IOEnv ()
decode INP =
    do val <- liftIO (readLn :: IO Int)
       setAccumulator val
       i <- nextInstruction
       decode i
decode OUT =
    do acc <- getAccumulator
       liftIO $ print acc
       i <- nextInstruction
       decode i
decode HLT  = liftIO $ print "Program Terminated"
decode (STA label) = 
    do acc <- getAccumulator
       store acc label
       i <- nextInstruction
       decode i
decode (LDA label) =
    do env <- get
       acc <- getValue label
       setAccumulator acc
       i <- nextInstruction
       decode i
decode (ADD label) =
    do acc <- getAccumulator
       ope <- getValue label
       setAccumulator (acc+ope)
       i <- nextInstruction
       decode i
decode (SUB label) =
    do acc <- getAccumulator
       ope <- getValue label
       setAccumulator (acc-ope)
       i <- nextInstruction
       decode i
decode (BRA label) =
    do newpc <- getLabelAddr label
       env <- get
       put $ env { pc = newpc }
       i <- nextInstruction
       decode i
decode (BRZ label) =
    do acc <- getAccumulator
       case acc of 
         0 -> decode (BRA label)
         _ -> do i <- nextInstruction
                 decode i
decode (BRP label) =
    do acc <- getAccumulator
       if acc<0 then 
         decode (BRA label)
       else
         do i <- nextInstruction
            decode i
decode (DAT label) = 
    do i <- nextInstruction
       decode i

store :: Int -> Label -> IOEnv ()
store acc label =
    do env <- get
       let mailB = mailboxes env
       let newmail = update mailB acc label  
       put $ env { mailboxes = newmail }

update :: [(String,Int)] -> Int -> String -> [(String,Int)]
update ((str,num):xs) acc label 
     | str == label = (str,acc):update xs acc label
     | otherwise = (str,num):update xs acc label
update [] _ _ = []

getValue :: Label ->IOEnv Int
getValue label =
    do env <- get
       let mailB = mailboxes env
       case lookup label mailB of
         Just num -> return num
         Nothing -> error "No value of this label is available in mailboxes"

getLabelAddr :: Label ->IOEnv Int
getLabelAddr label =
    do env <- get
       let labelA = labelAddr env 
       case lookup label labelA of
         Just num -> return num
         Nothing -> error "No label-address is available in labelAddr"


setAccumulator :: Int -> IOEnv ()
setAccumulator acc =
    do env <- get
       put $ env { accumulator = Just acc }

getAccumulator :: IOEnv Int
getAccumulator =
    do env <- get
       case accumulator env of
         Just i -> return i
         Nothing -> error "Nothing in the accumulator"

nextInstruction :: IOEnv Instruction
nextInstruction =
    do env <- get
       let i = pc env
       when (i == length (instructions env)) $ error "No more instructions"
       put $ env { pc = i + 1 }
       return $ instructions env !! i

evalProgram :: Program -> IO ()
evalProgram [] = return ()
evalProgram p@((_,i):_) = liftM fst $ runStateT (decode i) (mkInitEnv p)
