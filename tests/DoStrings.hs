module DoStrings where

(>>) str1 str2 = str1 ++ "\n" ++ str2
(>>=) str1 f = f str1
