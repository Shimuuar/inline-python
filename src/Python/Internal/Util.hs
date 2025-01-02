-- |
module Python.Internal.Util where

import Data.Char
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types


withWCString :: String -> (Ptr CWchar -> IO a) -> IO a
withWCString = withArray0 (CWchar 0) . map (fromIntegral . ord)
