module PutJson where

import Data.List (intercalate)
import Json

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber s) = show s
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ",\n" (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)


putJValue :: JValue -> IO()
putJValue v = putStrLn (renderJValue v)
