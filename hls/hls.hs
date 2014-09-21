import System.Directory
import System.Environment
import Data.Map as Map
import Data.List as List
import Control.Monad

-- "ls" command 
-- Usage
--      $ hls OPTIONS DIRECTORY_NAME
--      Example) hls -a ~
-- Options
--      a: Show hidden files and directories.
--      r: Show files and directories recursively.

main = do 
    args <- getArgs
    let opts = checkOptions args $ Map.fromList [('a', False), ('r', False)]

    -- If target directory is not specified, current directory is set as the target directory.
    let feedDir = (\(x:xs) -> x == '-') $ last args
    let tgtDir = if feedDir == True
                     then "."
                     else last args
    setCurrentDirectory tgtDir
    lsList <- getLsList tgtDir opts
    mapM_ putStrLn lsList

getLsList :: String -> Map Char Bool -> IO [String]
getLsList rdir opts
    | Map.lookup 'r' opts == Just True = do
        contsInit <- getDirectoryContents rdir
        let showHidden = if Map.lookup 'a' opts == Just True
                             then True
                             else False
        makeLsListRec rdir contsInit showHidden 0 []
    | otherwise = do
        conts <- getDirectoryContents rdir
        if Map.lookup 'a' opts == Just False
            then do
                mapM addHead $
                     List.filter (\(x:xs) -> x /= '.') conts
            else do
                mapM addHead conts

addHead :: String -> IO String
addHead cont = do
    isDir <- doesDirectoryExist cont
    if isDir 
        then return ("+ " ++ cont)
        else return ("  " ++ cont)

makeLsListRec :: String -> [String] -> Bool -> Int -> [String] -> IO [String]
makeLsListRec rdir conts showHidden n acm = do
    let _conts = if showHidden == True
                     then [x| x <- conts, x /= ".", x /= ".."]
                     else List.filter (\(x:xs) -> x /= '.') conts
    if _conts == []
        then do
            return acm
        else do
            let (x:xs) = _conts
            isDir <- doesDirectoryExist x
            if isDir
                then do
                    _sub <- getDirectoryContents x
                    let _x = (replicate n ' ') ++ "+  " ++ x
                    setCurrentDirectory x
                    _acm <- makeLsListRec x _sub showHidden (n+3) (acm ++ [_x])
                    setCurrentDirectory ".."
                    makeLsListRec rdir xs showHidden n _acm
                else do
                    let _x = (replicate (n) ' ') ++ "|- " ++ x
                    makeLsListRec rdir xs showHidden n (acm ++ [_x])

checkOptions :: [String] -> Map Char Bool -> Map Char Bool
checkOptions (x:xs) opt_init = 
    checkOptions xs $ isOption x opt_init
checkOptions [] opt_init = opt_init
    
isOption :: [Char] -> Map Char Bool -> Map Char Bool
isOption [] opts_init = opts_init
isOption (x:xs) opts_init
    | x == '-' = analyzeOption xs opts_init
    | otherwise = opts_init

analyzeOption :: [Char] -> Map Char Bool -> Map Char Bool
analyzeOption [] opt_init = opt_init
analyzeOption (x:xs) opt_init
    | x == 'a' = analyzeOption xs $ Map.insert 'a' True opt_init
    | x == 'r' = analyzeOption xs $ Map.insert 'r' True opt_init
    | otherwise = analyzeOption xs opt_init
