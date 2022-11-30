import Control.Monad.Writer
import Data.Semigroup 

stat::[Int] -> Writer (Sum Int, Sum Int, Min Int, Max Int,Sum Int) [Int]

stat list | list == [] = do return []
          | otherwise  = do tell(Sum (head list), Sum (head list)^2,Min (head list), Max (head list), 1);stat (tail list); return list
            

