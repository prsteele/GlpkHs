module Main where

import Lib

main :: IO ()
main = do
  problem <- glp_create_prob

  -- Add a constraint
  row <- glp_add_rows problem 1

  -- Add two variables
  x_index <- glp_add_cols problem 2
  let y_index = x_index + 1

  -- Set the constraint  
  glp_set_row_bnds problem row glp_UP 0 4
  return ()
  
