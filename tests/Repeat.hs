module Repeat where

import Prelude hiding (repeat)

repeat :: a -> [a]
repeat x = x:repeat x
