-- |
module Python.Internal.Util where

import Data.Char
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types

withWCtring :: String -> (Ptr CWchar -> IO a) -> IO a
withWCtring = withArray0 (CWchar 0) . map (fromIntegral . ord)
