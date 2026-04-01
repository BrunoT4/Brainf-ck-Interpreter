{-# LANGUAGE BangPatterns #-}
module Main (main, runBytes) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8)
import System.IO (stdin, stdout, hFlush)
import Data.Bits ((.&.), shiftL)
import Control.Monad.ST (runST)
import Data.STRef


data OpType
  = LEFT_OP
  | RIGHT_OP
  | ADD_OP
  | SUB_OP
  | LOOP_START_OP
  | LOOP_END_OP
  | OUTPUT_OP
  | INPUT_OP
  | ZERO_OP
  | MULTIPLY_OP
  deriving (Eq, Show)

data Op = Op
  { opType   :: !OpType
  , opArg    :: !Int
  , opOffset :: !Int
  } deriving (Eq, Show)

newOp :: OpType -> Int -> Int -> Op
newOp = Op

consecOp :: BS.ByteString -> Int -> Word8 -> (Int, Int)
consecOp bytes i target = go i
  where
    len = BS.length bytes
    go !j
      | j < len && BS.index bytes j == target = go (j + 1)
      | otherwise = (j - 1, j - i)

createProgram :: BS.ByteString -> [Op]
createProgram bytes = go 0 []
  where
    len = BS.length bytes
    go !i acc
      | i >= len = reverse acc
      | otherwise =
          case BS.index bytes i of
            60 -> let (i', count) = consecOp bytes i 60 in go (i' + 1) (newOp LEFT_OP count 0 : acc)
            62 -> let (i', count) = consecOp bytes i 62 in go (i' + 1) (newOp RIGHT_OP count 0 : acc)
            43 -> let (i', count) = consecOp bytes i 43 in go (i' + 1) (newOp ADD_OP count 0 : acc)
            45 -> let (i', count) = consecOp bytes i 45 in go (i' + 1) (newOp SUB_OP count 0 : acc)
            91 -> go (i + 1) (newOp LOOP_START_OP 0 0 : acc)
            93 -> go (i + 1) (newOp LOOP_END_OP 0 0 : acc)
            46 -> go (i + 1) (newOp OUTPUT_OP 0 0 : acc)
            44 -> go (i + 1) (newOp INPUT_OP 0 0 : acc)
            _  -> go (i + 1) acc

optimizeContraction :: [Op] -> [Op]
optimizeContraction = reverse . foldl step []
  where
    signedData op = case opType op of
      ADD_OP -> opArg op
      SUB_OP -> negate (opArg op)
      _      -> 0
    signedPtr op = case opType op of
      RIGHT_OP -> opArg op
      LEFT_OP  -> negate (opArg op)
      _        -> 0

    emitData n acc
      | n > 0     = newOp ADD_OP n 0 : acc
      | n < 0     = newOp SUB_OP (-n) 0 : acc
      | otherwise = acc

    emitPtr n acc
      | n > 0     = newOp RIGHT_OP n 0 : acc
      | n < 0     = newOp LEFT_OP (-n) 0 : acc
      | otherwise = acc

    step [] op
      | opType op == ADD_OP || opType op == SUB_OP = emitData (signedData op) []
      | opType op == LEFT_OP || opType op == RIGHT_OP = emitPtr (signedPtr op) []
      | otherwise = [op]
    step acc@(lastOp:rest) op
      | opType op == ADD_OP || opType op == SUB_OP =
          if opType lastOp == ADD_OP || opType lastOp == SUB_OP
            then emitData (signedData op + signedData lastOp) rest
            else emitData (signedData op) acc
      | opType op == LEFT_OP || opType op == RIGHT_OP =
          if opType lastOp == LEFT_OP || opType lastOp == RIGHT_OP
            then emitPtr (signedPtr op + signedPtr lastOp) rest
            else emitPtr (signedPtr op) acc
      | otherwise = op : acc

optimizeClearLoop :: [Op] -> [Op]
optimizeClearLoop [] = []
optimizeClearLoop [a] = [a]
optimizeClearLoop [a,b] = [a,b]
optimizeClearLoop (a:b:c:rest)
  | opType a == LOOP_START_OP
    && ((opType b == SUB_OP && opArg b == 1) || (opType b == ADD_OP && opArg b == 1))
    && opType c == LOOP_END_OP = newOp ZERO_OP 0 0 : optimizeClearLoop rest
  | otherwise = a : optimizeClearLoop (b:c:rest)

optimizeCopyLoop :: [Op] -> [Op]
optimizeCopyLoop = go
  where
    go [] = []
    go (startOp:rest)
      | opType startOp /= LOOP_START_OP = startOp : go rest
      | otherwise =
          case rest of
            (seed:xs) | opType seed == SUB_OP && opArg seed == 1 ->
              case scanLoop 0 [] xs of
                Just (copies, remaining) ->
                  let newOps = [newOp MULTIPLY_OP mult off | (off, mult) <- copies] ++ [newOp ZERO_OP 0 0]
                  in newOps ++ go remaining
                Nothing -> startOp : go rest
            _ -> startOp : go rest

    scanLoop !_ acc [] = Nothing
    scanLoop !pos acc (op:xs) =
      case opType op of
        RIGHT_OP -> scanLoop (pos + opArg op) acc xs
        LEFT_OP  -> scanLoop (pos - opArg op) acc xs
        ADD_OP   -> scanLoop pos (acc ++ [(pos, opArg op)]) xs
        LOOP_END_OP | pos == 0 -> Just (acc, xs)
        _ -> Nothing

optimizeMultiplicationLoops :: [Op] -> [Op]
optimizeMultiplicationLoops = go
  where
    go [] = []
    go (startOp:rest)
      | opType startOp /= LOOP_START_OP = startOp : go rest
      | otherwise =
          case rest of
            (seed:xs) | opType seed == SUB_OP && opArg seed == 1 ->
              case scanLoop 0 IM.empty xs of
                Just (multMap, remaining) ->
                  let newOps = [newOp MULTIPLY_OP m off | (off, m) <- IM.toList multMap, off /= 0, m /= 0]
                           ++ [newOp ZERO_OP 0 0]
                  in newOps ++ go remaining
                Nothing -> startOp : go rest
            _ -> startOp : go rest

    scanLoop !_ !_ [] = Nothing
    scanLoop !ptrDelta !multMap (op:xs) =
      case opType op of
        LEFT_OP  -> scanLoop (ptrDelta - opArg op) multMap xs
        RIGHT_OP -> scanLoop (ptrDelta + opArg op) multMap xs
        ADD_OP   -> scanLoop ptrDelta (IM.insertWith (+) ptrDelta (opArg op) multMap) xs
        SUB_OP   -> scanLoop ptrDelta (IM.insertWith (+) ptrDelta (negate (opArg op)) multMap) xs
        LOOP_END_OP | ptrDelta == 0 -> Just (multMap, xs)
        _ -> Nothing

optimizeOffsets :: [Op] -> [Op]
optimizeOffsets ops = finalize $ foldl step ([], 0) ops
  where
    canOffset t = t `elem` [ADD_OP, SUB_OP, ZERO_OP, OUTPUT_OP, INPUT_OP]

    emitMove (out, acc)
      | acc == 0 = (out, 0)
      | acc > 0  = (out ++ [newOp RIGHT_OP acc 0], 0)
      | otherwise = (out ++ [newOp LEFT_OP (-acc) 0], 0)

    step (!out, !acc) op =
      case opType op of
        LEFT_OP  -> (out, acc - opArg op)
        RIGHT_OP -> (out, acc + opArg op)
        LOOP_START_OP ->
          let (out', acc') = emitMove (out, acc)
          in (out' ++ [op], acc')
        _ | canOffset (opType op) -> (out ++ [op { opOffset = opOffset op + acc }], acc)
        _ -> let (out', acc') = emitMove (out, acc)
             in (out' ++ [op], acc')

    finalize st = fst (emitMove st)

alignBrackets :: [Op] -> [Op]
alignBrackets ops = map attach [0 .. n - 1]
  where
    n = length ops
    indexed = zip [0..] ops
    pairMap = build indexed [] IM.empty

    build [] [] m = m
    build [] _ _ = error "Unmatched '['"
    build ((i,op):xs) stack m =
      case opType op of
        LOOP_START_OP -> build xs (i:stack) m
        LOOP_END_OP ->
          case stack of
            [] -> error "Unmatched ']'"
            (open:rest) -> build xs rest (IM.insert i open (IM.insert open i m))
        _ -> build xs stack m

    attach i =
      let op = ops !! i
      in case opType op of
           LOOP_START_OP -> op { opArg = IM.findWithDefault 0 i pairMap }
           LOOP_END_OP   -> op { opArg = IM.findWithDefault 0 i pairMap }
           _             -> op

bfEval :: [Op] -> BS.ByteString -> BS.ByteString
bfEval prog inputBytes = runST $ do
  cells <- MV.replicate (1 `shiftL` 16) (0 :: Int)
  inRef <- newSTRef 0
  outRef <- newSTRef ([] :: [Word8])

  let plen = length prog

      readCell idx = MV.read cells idx
      writeCell idx val = MV.write cells idx val
      modCell idx f = do
        cur <- MV.read cells idx
        MV.write cells idx (f cur)

      exec !pc !cc
        | pc >= plen = pure ()
        | otherwise = do
            let op = prog !! pc
                idx = cc + opOffset op
            case opType op of
              LEFT_OP  -> exec (pc + 1) (cc - opArg op)
              RIGHT_OP -> exec (pc + 1) (cc + opArg op)
              ADD_OP   -> modCell idx (+ opArg op) >> exec (pc + 1) cc
              SUB_OP   -> modCell idx (subtract (opArg op)) >> exec (pc + 1) cc
              ZERO_OP  -> writeCell idx 0 >> exec (pc + 1) cc
              LOOP_START_OP -> do
                cur <- readCell cc
                if cur == 0 then exec (opArg op + 1) cc else exec (pc + 1) cc
              LOOP_END_OP -> do
                cur <- readCell cc
                if cur /= 0 then exec (opArg op) cc else exec (pc + 1) cc
              MULTIPLY_OP -> do
                base <- readCell cc
                modCell idx (+ (base * opArg op))
                exec (pc + 1) cc
              OUTPUT_OP -> do
                cur <- readCell idx
                modifySTRef' outRef (fromIntegral (cur .&. 255) :)
                exec (pc + 1) cc
              INPUT_OP -> do
                ip <- readSTRef inRef
                let b = if ip < BS.length inputBytes then BS.index inputBytes ip else 0
                writeSTRef inRef (ip + 1)
                writeCell idx (fromIntegral b)
                exec (pc + 1) cc

  exec 0 0
  out <- readSTRef outRef
  pure (BS.pack (reverse out))

runBytes :: BS.ByteString -> BS.ByteString
runBytes bytes =
  let prog0 = createProgram bytes
      prog1 = optimizeContraction prog0
      prog2 = optimizeMultiplicationLoops prog1
      prog3 = optimizeContraction prog2
      prog4 = optimizeClearLoop prog3
      prog5 = optimizeContraction prog4
      prog6 = optimizeOffsets prog5
      prog7 = alignBrackets prog6
  in bfEval prog7 BS.empty

main :: IO ()
main = do
  src <- BS.hGetContents stdin
  BS.hPut stdout (runBytes src)
  hFlush stdout
