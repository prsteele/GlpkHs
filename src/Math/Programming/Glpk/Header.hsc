{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.Glpk.Header where

import GHC.Generics (Generic)
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic

#include <glpk.h>

data Problem

-- | An array whose data begins at index 1
newtype GlpkArray a
  = GlpkArray { fromGplkArray :: Ptr a }
  deriving
    ( Eq
    , Ord
    , Show
    , Storable
    )

mkGlpkArray :: Storable a => [a] -> IO (GlpkArray a)
mkGlpkArray xs = do
  let aSize :: Int
      aSize = (sizeOf (head xs))
  array <- mallocArray (1 + length xs)
  pokeArray (plusPtr array aSize) xs
  return $ GlpkArray array

newtype VariableIndex
  = VariableIndex { fromVariableIndex :: CInt}
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    )

newtype ConstraintIndex
  = ConstraintIndex { fromConstraintIndex :: CInt}
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    )

foreign import ccall "glp_create_prob" glp_create_prob
  :: IO (Ptr Problem)
  -- ^ The allocated problem instance

foreign import ccall "glp_delete_prob" glp_delete_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_set_prob_name" glp_set_prob_name
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The problem name
  -> IO ()

foreign import ccall "glp_set_obj_name" glp_set_obj_name
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The objective name
  -> IO ()

foreign import ccall "glp_set_obj_dir" glp_set_obj_dir
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkDirection
  -- ^ Whether the problem is a minimization or maximization problem
  -> IO ()

foreign import ccall "glp_add_rows" glp_add_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of constraints to add
  -> IO ConstraintIndex
  -- ^ The index of the first new constraint added

foreign import ccall "glp_add_cols" glp_add_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of variables to add
  -> IO VariableIndex
  -- ^ The index of the first new variable added

foreign import ccall "glp_set_row_name" glp_set_row_name
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint being named
  -> CString
  -- ^ The name of the constraint
  -> IO ()

foreign import ccall "glp_set_col_name" glp_set_col_name
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable being named
  -> CString
  -- ^ The name of the variable
  -> IO ()

foreign import ccall "glp_set_row_bnds" glp_set_row_bnds
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint being bounded
  -> GlpkConstraintType
  -- ^ The type of constraint
  -> CDouble
  -- ^ The lower bound
  -> CDouble
  -- ^ The upper bound
  -> IO ()

foreign import ccall "glp_set_col_bnds" glp_set_col_bnds
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable being bounded
  -> GlpkConstraintType
  -- ^ The type of constraint
  -> CDouble
  -- ^ The lower bound
  -> CDouble
  -- ^ The upper bound
  -> IO ()

foreign import ccall "glp_set_obj_coef" glp_set_obj_coef
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable
  -> CDouble
  -- ^ The objective coefficient
  -> IO ()

foreign import ccall "glp_set_mat_row" glp_set_mat_row
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint being modified
  -> CInt
  -- ^ The number of variables being set
  -> GlpkArray VariableIndex
  -- ^ The variables being set
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> IO ()

foreign import ccall "glp_set_mat_col" glp_set_mat_col
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable being modified
  -> CInt
  -- ^ The number of coefficients being set
  -> Ptr ConstraintIndex
  -- ^ The constraints being modified
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> IO ()

foreign import ccall "glp_load_matrix" glp_load_matrix
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of nonzero elements to be loaded
  -> GlpkArray ConstraintIndex
  -- ^ The constraint indices
  -> GlpkArray VariableIndex
  -- ^ The variable indices
  -> GlpkArray CDouble
  -- ^ The coefficients
  -> IO ()

foreign import ccall "glp_check_dup" glp_check_dup
  :: CInt
  -- ^ The number of rows in the matrix
  -> CInt
  -- ^ The number of columns in the matrix
  -> CInt
  -- ^ The number of nonzeros in the matrix
  -> GlpkArray CInt
  -- ^ The rows being checked
  -> GlpkArray CInt
  -- ^ The columns being checked
  -> CInt

foreign import ccall "glp_sort_matrix" glp_sort_matrix
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_del_rows" glp_del_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of constraints to delete
  -> GlpkArray ConstraintIndex
  -- ^ The indices of the constraints to delete
  -> IO ()

foreign import ccall "glp_del_cols" glp_del_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> CInt
  -- ^ The number of variables to delete
  -> GlpkArray VariableIndex
  -- ^ The indices of the variables to delete
  -> IO ()

foreign import ccall "glp_copy_prob" glp_copy_prob
  :: Ptr Problem
  -- ^ The destination problem instance
  -> Ptr Problem
  -- ^ The problem instance to be copied
  -> GlpkControl
  -- ^ Whether to copy symbolic names
  -> IO ()

foreign import ccall "glp_erase_prob" glp_erase_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_get_prob_name" glp_get_prob_name
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CString
  -- ^ The name of the problem

foreign import ccall "glp_get_obj_name" glp_get_obj_name
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CString
  -- ^ The name of the objective

foreign import ccall "glp_get_obj_dir" glp_get_obj_dir
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkDirection
  -- ^ The direction of the objective

foreign import ccall "glp_get_num_rows" glp_get_num_rows
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of constraints

foreign import ccall "glp_get_num_cols" glp_get_num_cols
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of variables

foreign import ccall "glp_get_row_name" glp_get_row_name
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The index of the constraint
  -> IO CString
  -- ^ The constraint name

foreign import ccall "glp_get_col_name" glp_get_col_name
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The index of the variable
  -> IO CString
  -- ^ The variable name

foreign import ccall "glp_get_row_type" glp_get_row_type
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The index of the constraint
  -> IO GlpkConstraintType
  -- ^ The constraint type

foreign import ccall "glp_get_row_lb" glp_get_row_lb
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The index of the constraint
  -> IO CDouble
  -- ^ The constraint lower bound

foreign import ccall "glp_get_row_ub" glp_get_row_ub
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The index of the constraint
  -> IO CDouble
  -- ^ The constraint upper bound

foreign import ccall "glp_get_col_type" glp_get_col_type
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The index of the variable
  -> IO GlpkVariableType
  -- ^ The constraint type

foreign import ccall "glp_get_col_lb" glp_get_col_lb
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The variable lower bound

foreign import ccall "glp_get_col_ub" glp_get_col_ub
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The variable upper bound

foreign import ccall "glp_get_obj_coef" glp_get_obj_coef
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The index of the variable
  -> IO CDouble
  -- ^ The objective coefficient

foreign import ccall "glp_get_num_nz" glp_get_num_nz
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The number of nonzero constraint coefficients

foreign import ccall "glp_get_mat_row" glp_get_mat_row
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to retrieve
  -> GlpkArray VariableIndex
  -- ^ The variable indices in the constraint
  -> GlpkArray CDouble
  -- ^ The variable coefficients in the constraint
  -> IO CInt
  -- ^ The length of the arrays

foreign import ccall "glp_get_mat_col" glp_get_mat_col
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The constraint to retrieve
  -> GlpkArray ConstraintIndex
  -- ^ The constraint indices the variable is in
  -> GlpkArray CDouble
  -- ^ The constraint coefficients for the variable
  -> IO CInt
  -- ^ The length of the arrays

foreign import ccall "glp_create_index" glp_create_index
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_delete_index" glp_delete_index
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_find_row" glp_find_row
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The name of the constraint
  -> IO ConstraintIndex
  -- ^ The index of the constraint

foreign import ccall "glp_find_col" glp_find_col
  :: Ptr Problem
  -- ^ The problem instance
  -> CString
  -- ^ The name of the variable
  -> IO VariableIndex
  -- ^ The index of the variable

foreign import ccall "glp_set_rii" glp_set_rii
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to scale
  -> CDouble
  -- ^ The scaling factor
  -> IO ()

foreign import ccall "glp_get_rii" glp_get_rii
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint index
  -> IO CDouble

foreign import ccall "glp_set_sjj" glp_set_sjj
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable to scale
  -> CDouble
  -- ^ The scaling factor
  -> IO ()

foreign import ccall "glp_get_sjj" glp_get_sjj
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable index
  -> IO CDouble

foreign import ccall "glp_scale_prob" glp_scale_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkScaling
  -- ^ The type of scaling to apply
  -> IO ()

foreign import ccall "glp_unscale_prob" glp_unscale_prob
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_set_row_stat" glp_set_row_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to modify
  -> GlpkVariableStatus
  -- ^ The status to apply
  -> IO ()

foreign import ccall "glp_set_col_stat" glp_set_col_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable to modify
  -> GlpkVariableStatus
  -- ^ The status to apply
  -> IO ()

foreign import ccall "glp_std_basis" glp_std_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_adv_basis" glp_adv_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> Unused CInt
  -- ^ Reserved for future use, must be zero
  -> IO ()

foreign import ccall "glp_cpx_basis" glp_cpx_basis
  :: Ptr Problem
  -- ^ The problem instance
  -> IO ()

foreign import ccall "glp_simplex" glp_simplex
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr SimplexMethodControlParameters
  -- ^ Simplex control parameters
  -> IO GlpkSimplexStatus
  -- ^ The exit status

foreign import ccall "glp_exact" glp_exact
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr SimplexMethodControlParameters
  -- ^ Simplex control parameters
  -> IO GlpkSimplexStatus
  -- ^ The exit status

foreign import ccall "glp_init_smcp" glp_init_smcp
  :: Ptr SimplexMethodControlParameters
  -- ^ The Simplex control parameters to initialize
  -> IO ()

foreign import ccall "glp_get_status" glp_get_status
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_prim_stat" glp_get_prim_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_dual_stat" glp_get_dual_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> IO GlpkSolutionStatus

foreign import ccall "glp_get_obj_val" glp_get_obj_val
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CDouble

foreign import ccall "glp_get_row_stat" glp_get_row_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to query
  -> IO GlpkVariableStatus
  -- ^ The status of the associated with the auxiliary variable

foreign import ccall "glp_get_col_stat" glp_get_col_stat
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable to query
  -> IO GlpkVariableStatus
  -- ^ The status of the variable

foreign import ccall "glp_get_row_prim" glp_get_row_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to query
  -> IO CDouble
  -- ^ The primal auxiliary variable value

foreign import ccall "glp_get_row_dual" glp_get_row_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> ConstraintIndex
  -- ^ The constraint to query
  -> IO CDouble
  -- ^ The dual auxiliary variable value

foreign import ccall "glp_get_col_prim" glp_get_col_prim
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The primal variable value

foreign import ccall "glp_get_col_dual" glp_get_col_dual
  :: Ptr Problem
  -- ^ The problem instance
  -> VariableIndex
  -- ^ The variable to query
  -> IO CDouble
  -- ^ The dual variable value

foreign import ccall "glp_get_unbnd_ray" glp_get_unbnd_ray
  :: Ptr Problem
  -- ^ The problem instance
  -> IO CInt
  -- ^ The index, k, of the variable producing unboundedness. If 1 <=
  -- k <= m, then k is the kth auxiliary variable. If m + 1 <= k <= m
  -- + n, it is the (k - m)th structural variable.

foreign import ccall "glp_get_bfcp" glp_get_bfcp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr BasisFactorizationControlParameters
  -- ^ A pointer that will hold the basis factorization control
  -- parameters
  -> IO ()

foreign import ccall "glp_set_bfcp" glp_set_bfcp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr BasisFactorizationControlParameters
  -- ^ The basis factorization control parameters
  -> IO ()

foreign import ccall "glp_interior" glp_interior
  :: Ptr Problem
  -> Ptr InteriorPointControlParameters
  -> IO GlpkInteriorPointStatus

foreign import ccall "glp_init_iptcp" glp_init_iptcp
  :: Ptr InteriorPointControlParameters
  -> IO ()

foreign import ccall "glp_ipt_status" glp_ipt_status
  :: Ptr InteriorPointControlParameters
  -> IO GlpkSolutionStatus

foreign import ccall "glp_intopt" glp_intopt
  :: Ptr Problem
  -> Ptr (MIPControlParameters a)
  -> IO GlpkMIPStatus

foreign import ccall "glp_init_iocp" glp_init_iocp
  :: Ptr (MIPControlParameters a)
  -> IO ()

foreign import ccall "glp_ios_reason" glp_ios_reason
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO GlpkCallbackReason
  -- ^ The reason the callback is being called

foreign import ccall "glp_ios_get_prob" glp_ios_get_prob
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO (Ptr Problem)
  -- ^ The active problem

foreign import ccall "glp_ios_tree_size" glp_ios_tree_size
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> Ptr CInt
  -- ^ The number of active nodes
  -> Ptr CInt
  -- ^ The total number of active and inactive nodes
  -> Ptr CInt
  -- ^ The total number of nodes that have been added to the tree
  -> IO ()

foreign import ccall "glp_ios_curr_node" glp_ios_curr_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO GlpkNodeIndex
  -- ^ The current node in the search tree

foreign import ccall "glp_ios_next_node" glp_ios_next_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO GlpkNodeIndex
  -- ^ The next node in the search tree after the target node

foreign import ccall "glp_ios_prev_node" glp_ios_prev_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO GlpkNodeIndex
  -- ^ The parent node in the search tree after the target node

foreign import ccall "glp_ios_up_node" glp_ios_up_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO GlpkNodeIndex
  -- ^ The parent of the target node

foreign import ccall "glp_ios_node_level" glp_ios_node_level
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO CInt
  -- ^ The level of the target in the search tree; the root problem
  -- has level 0.

foreign import ccall "glp_ios_node_bound" glp_ios_node_bound
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO CDouble
  -- ^ The objective bound on the target

foreign import ccall "glp_ios_best_node" glp_ios_best_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO GlpkNodeIndex
  -- ^ The node in the search tree with the best objective bound

foreign import ccall "glp_ios_mip_gap" glp_ios_mip_gap
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO CDouble
  -- ^ The current MIP gap

foreign import ccall "glp_ios_node_data" glp_ios_node_data
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The target node in the search tree
  -> IO (Ptr a)
  -- ^ The data associated with the target

foreign import ccall "glp_ios_row_attr" glp_ios_row_attr
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CInt
  -- ^ The index of the target cut
  -> Ptr GlpkCutAttribute
  -- ^ The information about the target cut
  -> IO ()

foreign import ccall "glp_ios_pool_size" glp_ios_pool_size
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO CInt
  -- ^ The number of cutting planes added to the problem

foreign import ccall "glp_ios_add_row" glp_ios_add_row
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CString
  -- ^ The name of the cutting plane to add
  -> GlpkUserCutType
  -- ^ The type of cut being added
  -> Unused CInt
  -- ^ Unused; must be zero
  -> CInt
  -- ^ The number of variable indices specified
  -> GlpkArray CInt
  -- ^ The variable indices
  -> GlpkArray CDouble
  -- ^ The variable coefficients
  -> GlpkConstraintType
  -- ^ The type of the constraint
  -> CDouble
  -- ^ The right-hand side of the constraint
  -> IO ()

foreign import ccall "glp_ios_del_row" glp_ios_del_row
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> CInt
  -- ^ The index of the cut to delete
  -> IO ()

foreign import ccall "glp_ios_clear_pool" glp_ios_clear_pool
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO ()

foreign import ccall "glp_ios_can_branch" glp_ios_can_branch
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> VariableIndex

foreign import ccall "glp_ios_branch_upon" glp_ios_branch_upon
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> VariableIndex
  -- ^ The index of the variable to branch on
  -> GlpkBranchOption
  -- ^ The branching decision
  -> IO ()

foreign import ccall "glp_ios_select_node" glp_ios_select_node
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkNodeIndex
  -- ^ The subproblem to explore
  -> IO ()

foreign import ccall "glp_ios_heur_sol" glp_ios_heur_sol
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> GlpkArray CDouble
  -- ^ The variable values of an integer heuristic
  -> IO ()

foreign import ccall "glp_ios_terminate" glp_ios_terminate
  :: Ptr (GlpkTree a)
  -- ^ The search tree
  -> IO ()

foreign import ccall "glp_set_col_kind" glp_set_col_kind
  :: Ptr Problem
  -> VariableIndex
  -> GlpkVariableType
  -> IO ()

foreign import ccall "glp_get_col_kind" glp_vet_col_kind
  :: Ptr Problem
  -> VariableIndex
  -> IO GlpkVariableType

foreign import ccall "glp_init_mpscp" glp_init_mpscp
  :: Ptr MPSControlParameters
  -- ^ The MPS control parameters to initialize
  -> IO ()

foreign import ccall "glp_read_mps" glp_read_mps
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkMPSFormat
  -- ^ The MPS format to read
  -> Ptr MPSControlParameters
  -- ^ The MPS control parameters
  -> CString
  -- ^ The name of the file to read
  -> IO ()

foreign import ccall "glp_write_mps" glp_write_mps
  :: Ptr Problem
  -- ^ The problem instance
  -> GlpkMPSFormat
  -- ^ The MPS format to read
  -> Ptr MPSControlParameters
  -- ^ The MPS control parameters
  -> CString
  -- ^ The name of the file to write to
  -> IO ()

foreign import ccall "glp_init_cpxcp" glp_init_cpxcp
  :: Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters to initialize
  -> IO ()

foreign import ccall "glp_read_lp" glp_read_lp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters
  -> CString
  -- ^ The name of the file to read
  -> IO ()

foreign import ccall "glp_write_lp" glp_write_lp
  :: Ptr Problem
  -- ^ The problem instance
  -> Ptr CplexLPFormatControlParameters
  -- ^ The CPLEX LP control parameters
  -> CString
  -- ^ The name of the file to write to
  -> IO ()

foreign import ccall "glp_mpl_alloc_wksp" glp_mpl_alloc_wksp
  :: IO (Ptr MathProgWorkspace)
  -- ^ The allocated MathProg workspace

foreign import ccall "glp_mpl_free_wksp" glp_mpl_free_wksp
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace to deallocate
  -> IO ()

foreign import ccall "glp_mpl_init_rand" glp_mpl_init_rand
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CInt
  -- ^ The random number generator seed
  -> IO MathProgResult

foreign import ccall "glp_mpl_read_model" glp_mpl_read_model
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The name of the file to read
  -> CInt
  -- ^ If nonzero, skip the data section
  -> IO MathProgResult

foreign import ccall "glp_mpl_read_data" glp_mpl_read_data
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The name of the file to read
  -> IO MathProgResult

foreign import ccall "glp_mpl_generate" glp_mpl_generate
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> CString
  -- ^ The output file. If NULL, output is written to standard output
  -> IO MathProgResult

foreign import ccall "glp_mpl_build_prob" glp_mpl_build_prob
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> Ptr Problem
  -- ^ The problem instance to write to
  -> IO MathProgResult

foreign import ccall "glp_mpl_postsolve" glp_mpl_postsolve
  :: Ptr MathProgWorkspace
  -- ^ The MathProg workspace
  -> Ptr Problem
  -- ^ The solved problem instance
  -> GlpkSolutionType
  -- ^ The type of solution to be copied
  -> IO MathProgResult

newtype GlpkMajorVersion
  = GlpkMajorVersion { fromGlpkMajorVersion :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

newtype GlpkMinorVersion
  = GlpkMinorVersion { fromGlpkMinorVersion :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkMajorVersion
 , GlpkMajorVersion
 , glpkMajorVersion = GLP_MAJOR_VERSION
 }

#{enum
   GlpkMinorVersion
 , GlpkMinorVersion
 , glpkMinorVersion = GLP_MINOR_VERSION
 }

newtype GlpkDirection
  = GlpkDirection { fromGlpkDirection :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkDirection
 , GlpkDirection
 , glpkMin = GLP_MIN
 , glpkMax = GLP_MAX
 }

newtype GlpkVariableType
  = GlpkVariableType { fromGlpkVariableType :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkVariableType
 , GlpkVariableType
 , glpkContinuous = GLP_CV
 , glpkInteger = GLP_IV
 , glpkBinary = GLP_BV
 }

newtype GlpkConstraintType
  = GlpkConstraintType { fromGlpkConstraintType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkConstraintType
 , GlpkConstraintType
 , glpkFree = GLP_FR
 , glpkGT = GLP_LO
 , glpkLT = GLP_UP
 , glpkEQ = GLP_DB
 , glpkFixed = GLP_FX
 }

newtype GlpkVariableStatus
  = GlpkVariableStatus { fromGlpkVariableStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkVariableStatus
 , GlpkVariableStatus
 , glpkBasic = GLP_BS
 , glpkNonBasicLower = GLP_NL
 , glpkNonBasicUpper = GLP_NU
 , glpkNonBasicFree = GLP_NF
 , glpkNonBasicFixed = GLP_NS
 }

newtype GlpkScaling
  = GlpkScaling { fromGlpkScaling :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkScaling
 , GlpkScaling
 , glpkGeometricMeanScaling = GLP_SF_GM
 , glpkEquilibrationScaling = GLP_SF_EQ
 , glpkPowerOfTwoScaling = GLP_SF_2N
 , glpkSkipScaling = GLP_SF_SKIP
 , glpkAutoScaling = GLP_SF_AUTO
 }

newtype GlpkSolutionType
  = GlpkSolutionType { fromGlpkSolutionType :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkSolutionType
 , GlpkSolutionType
 , glpkBasicSolution = GLP_SOL
 , glpkInteriorPointSolution = GLP_IPT
 , glpkMIPSolution = GLP_MIP
 }

newtype GlpkSolutionStatus
  = GlpkSolutionStatus { fromGlpkSolutionStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkSolutionStatus
 , GlpkSolutionStatus
 , glpkOptimal = GLP_OPT
 , glpkFeasible = GLP_FEAS
 , glpkInfeasible = GLP_INFEAS
 , glpkNoFeasible = GLP_NOFEAS
 , glpkUnbounded = GLP_UNBND
 , glpkUndefined = GLP_UNDEF
 }

newtype GlpkMessageLevel
  = GlpkMessageLevel { fromGlpkMessageLevel :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkMessageLevel
 , GlpkMessageLevel
 , glpkMessageOff = GLP_MSG_OFF
 , glpkMessageError = GLP_MSG_ERR
 , glpkMessageOn = GLP_MSG_ON
 , glpkMessageAll = GLP_MSG_ALL
 , glpkMessageDebug = GLP_MSG_DBG
 }

newtype GlpkSimplexMethod
  = GlpkSimplexMethod { fromGlpkSimplexMethod :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkSimplexMethod
 , GlpkSimplexMethod
 , glpkPrimalSimplex = GLP_PRIMAL
 , glpkDualSimplex = GLP_DUAL
 , glpkDualPSimplex = GLP_DUALP
 }

newtype GlpkPricing
  = GlpkPricing { fromGlpkPricing :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkPricing
 , GlpkPricing
 , glpkTextbookPricing = GLP_PT_STD
 , glpkStandardPricing = GLP_PT_STD
 , glpkProjectedSteepestEdge = GLP_PT_PSE
 }

newtype GlpkRatioTest
  = GlpkRatioTest { fromGlpkRatioTest :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkRatioTest
 , GlpkRatioTest
 , glpkStandardRatioTest = GLP_RT_STD
 , glpkHarrisTwoPassRatioTest = GLP_RT_HAR
 }

newtype GlpkPreCholeskyOrdering
  = GlpkPreCholeskyOrdering { fromGlpkPreCholeskyOrdering :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkPreCholeskyOrdering
 , GlpkPreCholeskyOrdering
 , glpkNatural = GLP_ORD_NONE
 , glpkQuotientMinimumDegree = GLP_ORD_QMD
 , glpkApproximateMinimumDegree = GLP_ORD_AMD
 , glpkSymmetricApproximateMinimumDegree = GLP_ORD_SYMAMD
 }

newtype GlpkBranchingTechnique
  = GlpkBranchingTechnique { fromGlpkBranchingTechnique :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkBranchingTechnique
 , GlpkBranchingTechnique
 , glpkFirstFractional = GLP_BR_FFV
 , glpkLastFractional = GLP_BR_LFV
 , glpkMostFractional = GLP_BR_MFV
 , glpkDriebeckTomlin = GLP_BR_DTH
 , glpkHybridPseudoCost = GLP_BR_PCH
 }

newtype GlpkBacktrackingTechnique
  = GlpkBacktrackingTechnique { fromGlpkBacktrackingTechnique :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkBacktrackingTechnique
 , GlpkBacktrackingTechnique
 , glpkDepthFirstSearch = GLP_BT_DFS
 , glpkBreadthFirstSearch = GLP_BT_BFS
 , glpkBestLocalBound = GLP_BT_BLB
 , glpkBestProjectionHeuristic = GLP_BT_BPH
 }

newtype GlpkPreProcessingTechnique
  = GlpkPreProcessingTechnique { fromGlpkPreProcessingTechnique :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkPreProcessingTechnique
 , GlpkPreProcessingTechnique
 , glpkPreProcessNone = GLP_PP_NONE
 , glpkPreProcessRoot = GLP_PP_ROOT
 , glpkPreProcessAll = GLP_PP_ALL
 }

newtype GlpkFeasibilityPump
  = GlpkFeasibilityPump { fromGlpkFeasibilityPump :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkFeasibilityPump
 , GlpkFeasibilityPump
 , glpkFeasibilityPumpOn = GLP_ON
 , glpkFeasibilityPumpOff = GLP_OFF
 }

newtype GlpkProximitySearch
  = GlpkProximitySearch { fromGlpkProximitySearch :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkProximitySearch
 , GlpkProximitySearch
 , glpkProximitySearchOn = GLP_ON
 , glpkProximitySearchOff = GLP_OFF
 }

newtype GlpkGomoryCuts
  = GlpkGomoryCuts { fromGlpkGomoryCuts :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkGomoryCuts
 , GlpkGomoryCuts
 , glpkGomoryCutsOn = GLP_ON
 , glpkGomoryCutsOff = GLP_OFF
 }

newtype GlpkMIRCuts
  = GlpkMIRCuts { fromGlpkMIRCuts :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkMIRCuts
 , GlpkMIRCuts
 , glpkMIRCutsOn = GLP_ON
 , glpkMIRCutsOff = GLP_OFF
 }

newtype GlpkCoverCuts
  = GlpkCoverCuts { fromGlpkCoverCuts :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkCoverCuts
 , GlpkCoverCuts
 , glpkCoverCutsOn = GLP_ON
 , glpkCoverCutsOff = GLP_OFF
 }

newtype GlpkCliqueCuts
  = GlpkCliqueCuts { fromGlpkCliqueCuts :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkCliqueCuts
 , GlpkCliqueCuts
 , glpkCliqueCutsOn = GLP_ON
 , glpkCliqueCutsOff = GLP_OFF
 }

newtype GlpkPresolve
  = GlpkPresolve { fromGlpkPresolve :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkPresolve
 , GlpkPresolve
 , glpkPresolveOn = GLP_ON
 , glpkPresolveOff = GLP_OFF
 }

newtype GlpkBinarization
  = GlpkBinarization { fromGlpkBinarization :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkBinarization
 , GlpkBinarization
 , glpkBinarizationOn = GLP_ON
 , glpkBinarizationOff = GLP_OFF
 }

newtype GlpkSimpleRounding
  = GlpkSimpleRounding { fromGlpkSimpleRounding :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkSimpleRounding
 , GlpkSimpleRounding
 , glpkSimpleRoundingOn = GLP_ON
 , glpkSimpleRoundingOff = GLP_OFF
 }

newtype GlpkConstraintOrigin
  = GlpkConstraintOrigin { fromGlpkConstraintOrigin :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkConstraintOrigin
 , GlpkConstraintOrigin
 , glpkRegularConstraint = GLP_RF_REG
 , glpkLazyConstraint = GLP_RF_LAZY
 , glpkCuttingPlaneConstraint = GLP_RF_CUT
 }

newtype GlpkCutType
  = GlpkCutType { fromGlpkCutType :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkCutType
 , GlpkCutType
 , glpkGomoryCut = GLP_RF_GMI
 , glpkMIRCut = GLP_RF_MIR
 , glpkCoverCut = GLP_RF_COV
 , glpkCliqueCut = GLP_RF_CLQ
 }

newtype GlpkControl
  = GlpkControl { fromGlpkControl :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkControl
 , GlpkControl
 , glpkOn = GLP_ON
 , glpkOff = GLP_OFF
 }

newtype GlpkCallbackReason
  = GlpkCallbackReason { fromGlpkCallbackReason :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkCallbackReason
 , GlpkCallbackReason
 , glpkSubproblemSelection = GLP_ISELECT
 , glpkPreprocessing = GLP_IPREPRO
 , glpkRowGeneration = GLP_IROWGEN
 , glpkHeuristicSolution = GLP_IHEUR
 , glpkCutGeneration = GLP_ICUTGEN
 , glpkBranching = GLP_IBRANCH
 , glpkNewIncumbent = GLP_IBINGO
 }

newtype GlpkBranchOption
  = GlpkBranchOption { fromGlpkBranchOption :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkBranchOption
 , GlpkBranchOption
 , glpkBranchUp = GLP_UP_BRNCH
 , glpkBranchDown = GLP_DN_BRNCH
 , glpkBranchAuto = GLP_NO_BRNCH
 }

newtype GlpkFactorizationResult
  = GlpkFactorizationResult { fromGlpkFactorizationResult :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkFactorizationResult
 , GlpkFactorizationResult
 , glpkFactorizationSuccess = 0
 , glpkFactorizationBadBasis = GLP_EBADB
 , glpkFactorizationSingular = GLP_ESING
 , glpkFactorizationIllConditioned = GLP_ECOND
 }

newtype GlpkSimplexStatus
  = GlpkSimplexStatus { fromGlpkSimplexStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkSimplexStatus
 , GlpkSimplexStatus
 , glpkSimplexSuccess = 0
 , glpkSimplexBadBasis = GLP_EBADB
 , glpkSimplexSingular = GLP_ESING
 , glpkSimplexIllConditioned = GLP_ECOND
 , glpkSimplexBadBound = GLP_EBOUND
 , glpkSimplexFailure = GLP_EFAIL
 , glpkSimplexDualLowerLimitFailure = GLP_EOBJLL
 , glpkSimplexDualUpperLimitFailure = GLP_EOBJUL
 , glpkSimplexIterationLimit = GLP_EITLIM
 , glpkSimplexTimeLimit = GLP_ETMLIM
 , glpkSimplexPrimalInfeasible = GLP_ENOPFS
 , glpkSimplexDualInfeasible = GLP_ENODFS
 }

newtype GlpkMIPStatus
  = GlpkMIPStatus { fromGlpkMIPStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkMIPStatus
 , GlpkMIPStatus
 , glpkMIPSuccess = 0
 , glpkMIPBadBound = GLP_EBOUND
 , glpkMIPNoBasis = GLP_EROOT
 , glpkMIPPrimalInfeasible = GLP_ENOPFS
 , glpkMIPDualInfeasible =  GLP_ENODFS
 , glpkMIPFailure = GLP_EFAIL
 , glpkMIPRelativeGap = GLP_EMIPGAP
 , glpkMIPTimeLimit = GLP_ETMLIM
 , glpkMIPStopped = GLP_ESTOP
 }

newtype GlpkInteriorPointStatus
  = GlpkInteriorPointStatus { fromGlpkInteriorPointStatus :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkInteriorPointStatus
 , GlpkInteriorPointStatus
 , glpkInteriorPointSuccess = 0
 , glpkInteriorPointFailure = GLP_EFAIL
 , glpkInteriorPointNoConvergence = GLP_ENOCVG
 , glpkInteriorPointIterationLimit = GLP_EITLIM
 , glpkInteriorPointNumericalInstability = GLP_EINSTAB
 }

newtype GlpkKKTCheck
  = GlpkKKTCheck { fromGlpkKKTCheck :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkKKTCheck
 , GlpkKKTCheck
 , glpkKKTPrimalEquality = GLP_KKT_PE
 , glpkKKTPrimalBound = GLP_KKT_PB
 , glpkKKTDualEquality = GLP_KKT_DE
 , glpkKKTDualBound = GLP_KKT_DB
 }

newtype GlpkMPSFormat
  = GlpkMPSFormat { fromGlpkMPSFormat :: CInt }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkMPSFormat
 , GlpkMPSFormat
 , glpkMPSAncient = GLP_MPS_DECK
 , glpkMPSDeck = GLP_MPS_DECK
 , glpkMPSModern = GLP_MPS_FILE
 }

newtype GlpkFactorizationType
  = GlpkFactorizationType { fromGlpkFactorizationType :: CInt }
  deriving
    ( Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

#{enum
   GlpkFactorizationType
 , GlpkFactorizationType
 , glpkLUForrestTomlin = GLP_BF_LUF + GLP_BF_FT
 , glpkLUSchurCompBartelsGolub = GLP_BF_LUF + GLP_BF_BG
 , glpkLUSchurGivensRotation = GLP_BF_LUF + GLP_BF_GR
 , glpkBTSchurBartelsGolub = GLP_BF_BTF + GLP_BF_BG
 , glpkBTSchurGivensRotation = GLP_BF_BTF + GLP_BF_GR
 }

-- | Control parameters for basis factorization.
--
-- This represents the `glp_bfcp` struct.
data BasisFactorizationControlParameters
  = BasisFactorizationControlParameters
    { bfcpMessageLevel :: Unused GlpkMessageLevel
    , bfcpType :: GlpkFactorizationType
    , bfcpLUSize :: Unused CInt
    , bfcpPivotTolerance :: CDouble
    , bfcpPivotLimit :: CInt
    , bfcpSuhl :: GlpkControl
    , bfcpEpsilonTolerance :: CDouble
    , bfcpMaxGro :: Unused CDouble
    , bfcpNfsMax :: CInt
    , bfcpUpdateTolerance :: Unused CDouble
    , bfcpNrsMax :: CInt
    , bfcpRsSize :: Unused CInt
    , bfcpFooBar :: Unused (FixedLengthArray BfcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable BasisFactorizationControlParameters

data BfcpFooBar

instance FixedLength BfcpFooBar where
  fixedLength _ = 38

data SimplexMethodControlParameters
  = SimplexMethodControlParameters
    { smcpMessageLevel :: GlpkMessageLevel
    , smcpMethod :: GlpkSimplexMethod
    , smcpPricing :: GlpkPricing
    , smcpRatioTest :: GlpkRatioTest
    , smcpPrimalFeasibilityTolerance :: Double
    , smcpDualFeasibilityTolerance :: Double
    , smcpPivotTolerance :: Double
    , smcpLowerObjectiveLimit :: Double
    , smcpUpperObjectiveLimit :: Double
    , smcpIterationLimit :: CInt
    , smcpTimeLimitMillis :: CInt
    , smcpOutputFrequencyMillis :: CInt
    , smcpOutputDelayMillis :: CInt
    , smcpPresolve :: GlpkPresolve
    , smcpExcl :: Unused CInt
    , smcpShift :: Unused CInt
    , smcpAOrN :: Unused CInt
    , smcpFooBar :: Unused (FixedLengthArray SmcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable SimplexMethodControlParameters

data SmcpFooBar

instance FixedLength SmcpFooBar where
  fixedLength _ = 33

data InteriorPointControlParameters
  = InteriorPointControlParameters
    { iptcpMessageLevel :: GlpkMessageLevel
    , iptcpOrderingAlgorithm :: GlpkPreCholeskyOrdering
    , iptcpFooBar :: Unused (FixedLengthArray IptcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable InteriorPointControlParameters

data IptcpFooBar

instance FixedLength IptcpFooBar where
  fixedLength _ = 48

data GlpkTree a

data MIPControlParameters a
  = MIPControlParameters
    { iocpMessageLevel :: GlpkMessageLevel
    , iocpBranchingTechnique :: GlpkBranchingTechnique
    , iocpBacktrackingTechnique :: GlpkBacktrackingTechnique
    , iocpAbsoluteFeasibilityTolerance :: CDouble
    , iocpRelativeObjectiveTolerance :: CDouble
    , iocpTimeLimitMillis :: CInt
    , iocpOutputFrequencyMillis :: CInt
    , iocpOutputDelayMillis :: CInt
    , iocpCallback :: FunPtr (Ptr (GlpkTree a) -> Ptr a -> IO ())
    , iocpNodeData :: Ptr a
    , iocpNodeDataSize :: CInt
    , iocpPreprocessingTechnique :: GlpkPreProcessingTechnique
    , iocpRelativeMIPGap :: CDouble
    , iocpMIRCuts :: GlpkMIRCuts
    , iocpGormoryCuts :: GlpkGomoryCuts
    , iocpCoverCuts :: GlpkCoverCuts
    , iocpCliqueCuts :: GlpkCliqueCuts
    , iocpPresolve :: GlpkPresolve
    , iocpBinarization :: GlpkBinarization
    , iocpFeasibilityPump :: GlpkFeasibilityPump
    , iocpProximitySearch :: GlpkProximitySearch
    , iocpProximityTimeLimitMillis :: CInt
    , iocpSimpleRounding :: GlpkSimpleRounding
    , iocpUseExistingSolution :: Unused CInt
    , iocpNewSolutionFileName :: Unused (Ptr CChar)
    , iocpUseAlienSolver :: Unused CInt
    , iocpUseLongStepDual :: Unused CInt
    , iocpFooBar :: Unused (FixedLengthArray IocpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable (MIPControlParameters a)

data IocpFooBar

instance FixedLength IocpFooBar where
  fixedLength _ = 23

data GlpkCutAttribute
  = GlpkCutAttribute
    { attrLevel :: CInt
    , attrContraintOrigin :: GlpkConstraintOrigin
    , attrCutType :: GlpkCutType
    , attrFooBar :: Unused (FixedLengthArray AttrFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable GlpkCutAttribute

data AttrFooBar

instance FixedLength AttrFooBar where
  fixedLength _ = 7

newtype GlpkNodeIndex
  = GlpkNodeIndex { fromGlpkNodeIndex :: CInt }
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    )

-- | A value between 101 and 200 used to distinguish between
-- user-generated cut classes.
newtype GlpkUserCutType
  = GlpkUserCutType { fromGlpkUserCutType :: CInt }
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    )

data MPSControlParameters
  = MPSControlParameters
    { mpscpBlank :: CInt
    , mpscpObjectiveName :: CString
    , mpscpZeroTolerance :: CDouble
    , mpscpFooBar :: Unused (FixedLengthArray MpscpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable MPSControlParameters

data MpscpFooBar

instance FixedLength MpscpFooBar where
  fixedLength _ = 17

data CplexLPFormatControlParameters
  = CplexLPFormatControlParameters
    { cpxcpFooBar :: Unused (FixedLengthArray CpxcpFooBar CDouble)
    }
  deriving
    ( Eq
    , Generic
    , Show
    )

instance GStorable CplexLPFormatControlParameters

data CpxcpFooBar

instance FixedLength CpxcpFooBar where
  fixedLength _ = 20

data MathProgWorkspace

-- Return codes from the MathProg translator.
--
-- Zero indicates success. Nonzero values indicate failure, and will
-- be accompanied by a printed message.
newtype MathProgResult
  = MathProgResult { fromMathProgResult :: CInt }
  deriving
    ( Enum
    , Eq
    , Ord
    , Read
    , Show
    , Storable
    )

-- A type used to represent an unused or undocumented struct member.
newtype Unused a
  = Unused { fromUnused :: a }
  deriving
    ( Enum
    , Eq
    , GStorable
    , Ord
    , Read
    , Show
    , Storable
    )

-- | The class of arrays of fixed length.
class FixedLength a where
  fixedLength :: a -> Int

-- | A type representing fixed-length array members of structs.
newtype FixedLengthArray a b
  = FixedLengthArray { fromFixedLengthArray :: [b] }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

instance (FixedLength a, Storable b) => GStorable (FixedLengthArray a b) where
  gsizeOf _ = (fixedLength (undefined :: a)) * (sizeOf (undefined :: b))

  galignment _ = alignment (undefined :: b)

  gpeekByteOff ptr offset
    = FixedLengthArray <$> peekArray arrayLength (plusPtr ptr offset)
    where
      arrayLength = fixedLength (undefined :: a)

  gpokeByteOff ptr offset (FixedLengthArray array)
    = pokeArray (plusPtr ptr offset) array
