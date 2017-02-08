module BrainHask where

import Data.Char (isSpace)
import System.Exit (ExitCode, exitSuccess, exitFailure)
import System.Environment (getArgs)

data CMD = ADV | RET | INC | DEC | GET | PUT | JFD | JBK deriving Eq

newtype Tape = Tape ([Char], Char, [Char])

moveLeft :: Tape -> Tape
moveLeft (Tape (cPrev : pre, '\0', []  )) = Tape (pre, cPrev, []         )
moveLeft (Tape (cPrev : pre, cCur, post)) = Tape (pre, cPrev, cCur : post)
moveLeft (Tape ([]         , cCur, post)) = Tape ([] , '\0' , cCur : post)

moveRight :: Tape -> Tape
moveRight (Tape ([] , '\0', cNext : post)) = Tape ([]        , cNext, post)
moveRight (Tape (pre, cCur, cNext : post)) = Tape (cCur : pre, cNext, post)
moveRight (Tape (pre, cCur, []          )) = Tape (cCur : pre, '\0' , []  )

inc :: Char -> Char
inc '\DEL' = '\0'
inc c | c < '\DEL' = succ c
inc _ = error "Invalid character"

dec :: Char -> Char
dec '\0' = '\DEL'
dec c | c > '\0' = pred c
dec _ = error "Invalid character"

runAhead :: [CMD] -> Int -> Int -> Int
runAhead cmds depth idx = case cmds !! idx of
    JFD -> runAhead cmds (depth + 1) (idx + 1)
    JBK | depth == 0 -> idx + 1
    JBK | depth /= 0 -> runAhead cmds (depth - 1) (idx + 1)
    _   -> runAhead cmds depth (idx + 1)

runBack :: [CMD] -> Int -> Int -> Int
runBack cmds depth idx = case cmds !! idx of
    JBK -> runBack cmds (depth + 1) (idx - 1)
    JFD | depth == 0 -> idx + 1
    JFD | depth /= 0 -> runBack cmds (depth - 1) (idx - 1)
    _   -> runBack cmds depth (idx - 1)

step :: [CMD] -> Int -> Tape -> IO (Int, Tape)
step cmds idx tape@(Tape (pre, c, post)) = case cmds !! idx of
    ADV -> return (idx + 1, moveRight tape)
    RET -> return (idx + 1, moveLeft  tape)
    INC -> return (idx + 1, Tape (pre, inc c, post))
    DEC -> return (idx + 1, Tape (pre, dec c, post))
    GET -> do { c' <- getChar ; return (idx + 1, Tape (pre, c', post)) }
    PUT -> do { putChar c     ; return (idx + 1, tape                ) }
    JFD | c == '\0' -> return (runAhead cmds (-1) idx, tape)
    JFD | c /= '\0' -> return (idx + 1, tape)
    JBK | c == '\0' -> return (idx + 1, tape)
    JBK | c /= '\0' -> return (runBack  cmds (-1) idx, tape)

run :: [CMD] -> Int -> Tape -> IO ()
run cmds idx tape
    | idx >= length cmds = return ()
    | idx <  length cmds = do
        (newIdx, newTape) <- step cmds idx tape
        run cmds newIdx newTape

scan :: String -> [CMD]
scan str = case str of
    '>' : cs -> ADV : scan cs
    '<' : cs -> RET : scan cs
    '+' : cs -> INC : scan cs
    '-' : cs -> DEC : scan cs
    ',' : cs -> GET : scan cs
    '.' : cs -> PUT : scan cs
    '[' : cs -> JFD : scan cs
    ']' : cs -> JBK : scan cs
    _ : cs -> scan cs
    [] -> []

parse :: Int -> [CMD] -> Bool
parse 0     []                       = True
parse _     []                       = False
parse depth (JFD : cmds)             = parse (depth + 1) cmds
parse depth (JBK : cmds) | depth > 0 = parse (depth - 1) cmds
parse depth (JBK : _   ) | otherwise = False
parse depth (_   : cmds)             = parse depth cmds

main :: IO ExitCode
main = do
    args <- getArgs
    text <- readFile $ args !! 0
    let prog = scan text
    if not (parse 0 prog) then do
        putStrLn "ERROR: program does not parse."
        exitFailure
    else do
        run prog 0 (Tape ([], '\0', []))
        exitSuccess
