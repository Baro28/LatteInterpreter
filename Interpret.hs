module Main where

import System.Environment(getArgs)
import System.IO.Error (userError, ioError)
import System.IO (openFile, IOMode (ReadMode), hGetContents, hPutStrLn, stderr)

import AbsGramatyka
import ParGramatyka
import LexGramatyka
import ErrM
import Interpreter

main :: IO()
-- main = pBlock $ evalBlock (Block (Just (1,13)) [SExp (Just (2,5)) (EApp (Just (2,5)) (Ident "print") [EVal (Just (2,11)) (ELitInt (Just (2,11)) 23)]),Decl (Just (3,5)) (Int (Just (3,5))) [Init (Just (3,9)) (Ident "x") (ELitInt (Just (3,13)) 10)],SExp (Just (4,5)) (EApp (Just (4,5)) (Ident "print") [EVal (Just (4,11)) (ELitInt (Just (4,11)) 23)]),SExp (Just (5,5)) (EApp (Just (5,5)) (Ident "print") [EVal (Just (5,11)) (EVar (Just (5,11)) (Ident "x"))]),Decl (Just (6,5)) (Int (Just (6,5))) [Init (Just (6,9)) (Ident "y") (EAdd (Just (6,13)) (ELitInt (Just (6,13)) 2127) (Plus (Just (6,18))) (EVar (Just (6,20)) (Ident "x")))],SExp (Just (7,5)) (EApp (Just (7,5)) (Ident "print") [EVal (Just (7,11)) (EAdd (Just (7,11)) (EAdd (Just (7,11)) (EVar (Just (7,11)) (Ident "y")) (Minus (Just (7,13))) (EVar (Just (7,15)) (Ident "x"))) (Plus (Just (7,17))) (ELitInt (Just (7,19)) 10))])])
main = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    text <- hGetContents file
    case pProgram $ myLexer text of
        (Ok p) -> do 
            res <- interpret p
            case res of
                (Left err) -> hPutStrLn stderr err
                _ -> return ()
        (Bad err) -> print err
