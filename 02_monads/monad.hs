-- Pure
import Control.Monad
import Data.List

replace x y z 
    | z == x = y
    | otherwise = z

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                   [(x,"")] -> Just x
                   _        -> Nothing

listFromString :: String -> Maybe [Int]
listFromString line = maybeRead $ "[" ++ intervirg line ++ "]"
    where intervirg = map $ replace ' ' ','

-- Impure
showSum :: [Int] -> IO ()
showSum list = do
    putStr $ join $ intersperse " + " $ map show list
    putStr " = "

lookupList :: String -> IO [Int]
lookupList line =
    let readList = listFromString line in
        case readList of
            Nothing   -> error "I said integers!"
            Just list -> return list

main :: IO ()
main = do
    putStrLn "Enter a list of integers"
    line <- getLine 
    list <- lookupList line
    let slist = map (^2) list in do
        showSum $ slist
        print $ sum slist
