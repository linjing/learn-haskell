import Data.List
import Control.Applicative

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x->(length x, head x)) $ Data.List.group xs
encode' xs = map ( (,) <$> length <*> head ) $ group xs
