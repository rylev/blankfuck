import Text.Parsec
import Text.Parsec.String as P
import Control.Monad.State
import qualified Data.Map as M
import Data.Word8

import Control.Applicative ((<$>))
import Data.Maybe

data Instruction = IncrementPointer
                 | DecrementPointer
                 | IncrementValue
                 | DecrementValue
                 | Input
                 | Output
                 | Loop [Instruction] deriving Show

main :: IO ()
main = do program <- getContents
          execute program

execute :: String -> IO ()
execute str = case parse parseProgram "" str of
                Left e -> print $ "There was an error: " ++ show e
                Right ins -> evalStateT (runInstructions ins) (0, M.empty)

parseProgram :: Parser [Instruction]
parseProgram = many parseInputChar

parseIncrementPointer :: Parser Instruction
parseIncrementPointer = char ' ' >> return IncrementPointer

parseDecrementPointer :: Parser Instruction
parseDecrementPointer = char '\b' >> return DecrementPointer

parseIncrementValue :: Parser Instruction
parseIncrementValue = tab >> return IncrementValue

parseDecrementValue :: Parser Instruction
parseDecrementValue = char '\v' >> return DecrementValue

parseOutput :: Parser Instruction
parseOutput = char '\f' >> return Output

parseInput :: Parser Instruction
parseInput = char '\a' >> return Input

parseLoop :: Parser Instruction
parseLoop = do _ <- newline
               ins <- many parseInputChar
               _ <- char '\r'
               return $ Loop ins

parseComment :: Parser ()
parseComment = do _ <- many $ noneOf "\t \r\n\f\a\v\b"
                  return ()

parseInstruction :: Parser Instruction
parseInstruction = parseIncrementPointer <|>
                   parseDecrementPointer <|>
                   parseIncrementValue <|>
                   parseDecrementValue <|>
                   parseOutput <|>
                   parseInput <|>
                   parseLoop

parseInputChar :: Parser Instruction
parseInputChar = do parseComment
                    i <- parseInstruction
                    parseComment
                    return i

type BFState = StateT (Int, M.Map Int Word8) IO ()

stepState :: Instruction -> BFState
stepState IncrementPointer = moveTapeHead (+ 1)
stepState DecrementPointer = moveTapeHead $ subtract 1
stepState IncrementValue = updateVal (+ 1)
stepState DecrementValue = updateVal $ subtract 1
stepState Output = do (h, tape) <- get
                      let c = fromMaybe 0 $ M.lookup h tape
                      liftIO . putChar $ toEnum . fromIntegral $ c
stepState Input = do (h, tape) <- get
                     liftIO $ print "hello"
                     c <- fromIntegral . fromEnum <$> liftIO getChar
                     put (h, M.insert h c tape)
stepState l@(Loop ins) = do (h, tape) <- get
                            let val = fromMaybe 0 $ M.lookup h tape
                            case val of
                              0 -> return ()
                              _ -> do runInstructions ins
                                      stepState l
updateVal :: (Word8 -> Word8) -> BFState
updateVal fn = do (h, tape) <- get
                  let newVal = fn $ oldVal h tape
                  put (h, M.insert h newVal tape)
  where oldVal h t = fromMaybe 0 $ M.lookup h t

moveTapeHead :: (Int -> Int) -> BFState
moveTapeHead fn = modify doMove
  where doMove (tapeHead, m) = (fn tapeHead, m)

runInstructions :: [Instruction] -> BFState
runInstructions = mapM_ stepState