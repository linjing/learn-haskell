import Control.Monad (replicateM)

gray :: Int -> [String]
gray n = [ str | str <- replicateM n "01" ]

