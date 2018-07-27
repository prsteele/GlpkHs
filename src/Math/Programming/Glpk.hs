module Math.Programming.Glpk
  ( module Math.Programming.Glpk.Header
  , mkGlpkArray
  ) where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Math.Programming.Glpk.Header

mkGlpkArray :: Storable a => [a] -> IO (GlpkArray a)
mkGlpkArray xs = do
  let aSize :: Int
      aSize = (sizeOf (head xs))
  array <- mallocArray (1 + length xs)
  pokeArray (plusPtr array aSize) xs
  return $ GlpkArray array
