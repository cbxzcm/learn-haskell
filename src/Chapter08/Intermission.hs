module Chapter08.Intermission where

--applyTimes 5 (+1) 5
--applyTimes 5 (+1) 5 $ applyTimes 4 (+1) 5
--applyTimes 5 (+1) 5 $ applyTimes 4 (+1) 5 $ applyTimes 3 (+1) 5
--applyTimes 5 (+1) 5 $ applyTimes 4 (+1) 5 $ applyTimes 3 (+1) 5 $ applyTimes 2 (+1) 5
--applyTimes 5 (+1) 5 $ applyTimes 4 (+1) 5 $ applyTimes 3 (+1) 5 $ applyTimes 2 (+1) 5 $ applyTimes 1 (+1) 5
--applyTimes 5 (+1) 5 $ applyTimes 4 (+1) 5 $ applyTimes 3 (+1) 5 $ applyTimes 2 (+1) 5 $ applyTimes 1 (+1) 5 $ 5

--applyTimes 5 (+1)
--(+1) . applyTimes 4 (+1) $ 5
--(+1) . (+1) . applyTimes 3 (+1) $ 5
--(+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
--(+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5
--(+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
--((+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1)) 5
