import qualified Text.CaboCha as C

main :: IO ()
main = do
    c <- C.new ["cabocha", "-f3", "-n1"]
    let s = "太郎は花子が読んでいる本を次郎に渡した"
    putStrLn =<< C.parse c s
    chunks <- C.parseToChunks c s
    mapM_ printChunk chunks

printToken :: C.Token String ->  IO ()
printToken t = do
    putStrLn "## token"
    putStrLn $ "surface:            " ++ C.tokenSurface t
    putStrLn $ "normalized surface: " ++ C.tokenNormalizedSurface t
    case C.tokenNE t of
        Just x
            | '0' `elem` x -> putStrLn "named entity:       no"
            | otherwise -> return ()
        Nothing -> return ()
    case C.tokenAdditionalInfo t of
        Just x -> putStrLn $ "additional info:    " ++ x
        _ -> return ()
    putStrLn $ joint ", " (C.tokenFeatureList t)

printChunk :: C.Chunk String -> IO ()
printChunk c = do
    putStrLn "# chunk"
    putStrLn $ "link:            " ++ show (C.chunkLink c)
    putStrLn $ "head position:   " ++ show (C.chunkHeadPos c)
    putStrLn $ "func. position:  " ++ show (C.chunkFuncPos c)
    putStrLn $ "token position:  " ++ show (C.chunkTokenPos c)
    putStrLn $ "token size:      " ++ show (C.chunkTokenSize c)
    putStrLn $ "score:           " ++ show (C.chunkScore c)
    case C.chunkAdditionalInfo c of
        Just x -> putStrLn $ "additional info: " ++ x
        _ -> return ()
    mapM_ printToken (C.chunkTokens c)

joint :: String -> [String] -> String
joint _ [] = []
joint _ [xs] = xs
joint glue (x:xs) = x ++ glue ++ joint glue xs
