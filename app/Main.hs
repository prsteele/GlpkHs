module Main where

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Text.Printf

import Math.Programming.Glpk.Header

main :: IO ()
main = do
  problem <- glp_create_prob

  -- Add a constraint
  row <- glp_add_rows problem 1

  -- Add two variables
  x_index <- glp_add_cols problem 2
  let y_index = succ x_index

  -- Make the variables integer
  glp_set_col_kind problem x_index glpkInteger
  glp_set_col_kind problem y_index glpkInteger

  -- Set the constraint
  glp_set_row_bnds problem row glpkLT 0.1 4

  indices <- mkGlpkArray [x_index, y_index]

  coefs <- mkGlpkArray [1.0, 1.0]
  glp_set_mat_row problem row 2 indices coefs
  free (fromGplkArray indices)
  free (fromGplkArray coefs)

  glp_set_col_bnds problem x_index glpkEQ 1 8
  glp_set_col_bnds problem y_index glpkEQ 0 10

  glp_set_obj_coef problem x_index 2.0
  glp_set_obj_coef problem y_index 0.5

  withCString "example.lp" (glp_write_lp problem nullPtr)

  bfcp <- alloca $ \ptr -> do
    glp_get_bfcp problem ptr
    peek ptr

  print bfcp

  alloca $ \simplexControl -> do
    glp_init_smcp simplexControl
    glp_simplex problem simplexControl >>= print

  GlpkMIPStatus mipStatus <- alloca $ \mipControlPtr -> do
    glp_init_iocp mipControlPtr

    mipControl <- peek mipControlPtr
    poke mipControlPtr $ mipControl { iocpPresolve = glpkPresolveOn }

    glp_intopt problem mipControlPtr

  putStrLn $ printf "Finished with status %i" (fromIntegral mipStatus :: Int)

  return ()
