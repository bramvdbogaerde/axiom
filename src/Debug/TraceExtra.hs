module Debug.TraceExtra where

import qualified Debug.Trace as Debug

traceShowPrefix :: Show a => String -> a -> a
traceShowPrefix prefix = Debug.traceWith ((prefix ++) . show)
