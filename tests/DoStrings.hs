module DoStrings where

import           Prelude hiding ((>>), (>>=))

(>>) :: String -> String -> String
(>>) str1 str2 = str1 ++ "\n" ++ str2

(>>=) :: String -> (String -> String) -> String
(>>=) str f = f str
