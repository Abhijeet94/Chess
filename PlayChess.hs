{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module PlayChess where
import GameLogic





































----------------------------------------------------
----------------------------------------------------
----------------------OLD---------------------------
----------------------------------------------------
----------------------------------------------------







--getNextMove :: String
--getNextMove = undefined

---- White to move:
---- E4 E5
---- print board
---- Black to move:

---- takes in a string of format LetterNumber and returns the corresponding Location
---- this needs to be changed to get both of them! [TODO]
--inputToLocation :: String -> Maybe (Location,Location)
--inputToLocation (x:y:[]) = undefined --if the string is 2 characters then convert it appropriately
--inputToLocation _ = Nothing

---- takes in a string of format "E4 E5" and if this is a valid action:
---- updates board with movePiece
---- checks for win
---- prints board
---- sets turn to next player
---- else: 
---- ask for correct input
--playGame :: IO ()
--playGame game = do
--    printBoard (board game)
--    case (checkForWin (board game)) of 
--        (Just (Win White)) -> S.liftIO $ putStrLn "White wins"
--        (Just (Win Black)) -> S.liftIO $ putStrLn "Black wins"
--        (Just Tie) -> S.liftIO $ putStrLn "Tie. Game Over."
--        Nothing -> do
--            S.liftIO $ putStr $ "Player " ++ (show (current game)) ++ "'s turn"
--            case (inputToLocation (getNextMove)) of 
--                Nothing -> S.liftIO $ putStrLn "Incorrect input"
--                (Just (from@(Loc x1 y1), to@(Loc x2 y2))) -> do
--                        initialBoard <-  S.get
--                        case (getPiece initialBoard from) of 
--                            Nothing -> S.liftIO $ putStrLn "Incorrect input"
--                            (Just pc@(P pl _)) -> if ((validMove pc from to) && (pl == current game))
--                                then do
--                                    case (handleTurn game from to) of
--                                        (Just game') -> playGame game'
--                                        Nothing -> playGame game
--                                else S.liftIO $ putStrLn "Incorrect input"


---- PrettyPrint our board
--printBoard :: Board -> ChessBoard ()
--printBoard = undefined                                