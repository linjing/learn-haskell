-- import TestModule (add)
-- import TestModule
import TestModule hiding (month)
-- import qualified TestModule as TM

import Text.Printf
main = printf "%d\n" (TestModule.add 5 4)

