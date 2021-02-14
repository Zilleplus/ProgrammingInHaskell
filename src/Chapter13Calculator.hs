module Chapter13Calculator where
    import Chapter13ParserExample
    import Control.Applicative
    import Data.Char
    import System.IO

    box :: [String]
    box = ["+---------------+",
           "|               |",
           "+---+---+---+---+",
           "| q | c | d | = |",
           "+---+---+---+---+",
           "| 1 | 2 | 3 | + |",
           "+---+---+---+---+",
           "| 3 | 4 | 5 | - |",
           "+---+---+---+---+",
           "| 6 | 7 | 8 | * |",
           "+---+---+---+---+",
           "| 0 | ( | ) | / |",
           "+---+---+---+---+"]

    buttons :: String
    buttons = standard ++ extra
        where
            standard = "qcs=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

    type Pos = (Int, Int)

    newline :: IO ()
    newline = putChar '\n'

    -- START helper functions from chapter 10
    goto :: Pos -> IO ()
    goto (x, y) = putStr ("\ESC[" ++ show y ++ "" ++ show x ++ "H")

    writeat :: Pos -> String -> IO()
    writeat p xs = do goto p
                      putStr xs
    
    getCh :: IO Char 
    getCh = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x

    cls :: IO ()
    cls = putStr "\ESC[2J"
    -- END helper functions from chapter 10

    showbox :: IO ()
    showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]
    
    dispay xs = do writeat (3,2) (replicate 12 ' ')
                   writeat (3,2) (reverse (take 13 (reverse xs)))

    calc :: String -> IO ()
    calc xs = do dispay xs 
                 c <- getCh
                 if elem c buttons then 
                     process c xs
                 else
                     do calc xs

    process :: Char -> String -> IO ()
    process c xs | elem c "qQ\ESC" = quit
                 | elem c "dD\BS\DEL" = delete xs
                 | elem c "=\n" = eval_calc xs 
                 | elem c "cC" = clear
                 | otherwise = press c xs

    quit :: IO ()
    quit = goto (1,14)

    delete :: String -> IO ()
    delete [] = calc []
    delete xs = calc (init xs)

    eval_calc :: String -> IO ()
    eval_calc xs = case parse expr xs of 
                [(n, [])] -> calc (show n)
                _         -> do calc xs

    clear :: IO ()
    clear = calc []

    press :: Char -> String -> IO ()
    press c xs = calc (xs ++ [c])

    run :: IO ()
    run = do cls
             showbox 
             clear
