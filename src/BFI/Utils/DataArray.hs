module BFI.Utils.DataArray
  ( DataArrayActionT,
    DataArrayAction,
    runDataArrayT,
    runDataArray,
    readDataArray,
    writeDataArray,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State
  ( MonadState (get, put),
    StateT,
    execStateT,
  )

type DataArrayActionT e = StateT [e]

type DataArrayAction e = DataArrayActionT e Identity

runDataArrayT :: Monad m => DataArrayActionT e m a -> e -> m [e]
runDataArrayT action initial = execStateT action $ genArray initial

runDataArray :: DataArrayAction e a -> e -> [e]
runDataArray action initial = runIdentity $ runDataArrayT action initial

readDataArray :: Monad m => Int -> DataArrayActionT e m e
readDataArray i = do
  array <- get
  return $ getElemAt array i

writeDataArray :: Monad m => Int -> e -> DataArrayActionT e m ()
writeDataArray i v = do
  array <- get
  put $ setElemAt array i v

--

genArray :: e -> [e]
genArray = repeat

getElemAt :: [e] -> Int -> e
getElemAt array i = array !! i

setElemAt :: [e] -> Int -> e -> [e]
setElemAt array i v = take i array ++ [v] ++ drop (i + 1) array
