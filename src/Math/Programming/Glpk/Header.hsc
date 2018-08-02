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

foreign import ccall "glp_add_cols" glp_add_cols
  :: Ptr Problem
  -> CInt
  -> IO VariableIndex

foreign import ccall "glp_add_rows" glp_add_rows
  :: Ptr Problem
  -> CInt
  -> IO ConstraintIndex

foreign import ccall "glp_set_row_bnds" glp_set_row_bnds
  :: Ptr Problem
  -> ConstraintIndex
  -> GlpkConstraintType
  -> CDouble
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_col_bnds" glp_set_col_bnds
  :: Ptr Problem
  -> VariableIndex
  -> GlpkConstraintType
  -> CDouble
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_obj_coef" glp_set_obj_coef
  :: Ptr Problem
  -> VariableIndex
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_mat_row" glp_set_mat_row
  :: Ptr Problem
  -> ConstraintIndex
  -> CInt
  -> GlpkArray VariableIndex
  -> GlpkArray CDouble
  -> IO ()

foreign import ccall "glp_set_mat_col" glp_set_mat_col
  :: Ptr Problem
  -> VariableIndex
  -> CInt
  -> Ptr ConstraintIndex
  -> GlpkArray CDouble
  -> IO ()

foreign import ccall "glp_write_lp" glp_write_lp
  :: Ptr Problem
  -> Ptr CInt
  -> CString
  -> IO ()

foreign import ccall "glp_simplex" glp_simplex
  :: Ptr Problem
  -> Ptr SimplexMethodControlParameters
  -> IO CInt

foreign import ccall "glp_init_smcp" glp_init_smcp
  :: Ptr SimplexMethodControlParameters
  -> IO ()

foreign import ccall "glp_get_bfcp" glp_get_bfcp
  :: Ptr Problem
  -> Ptr BasisFactorizationControlParameters
  -> IO ()

foreign import ccall "glp_set_bfcp" glp_set_bfcp
  :: Ptr Problem
  -> Ptr BasisFactorizationControlParameters
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
  -> IO GlpkCallbackReason

foreign import ccall "glp_ios_get_prob" glp_ios_get_prob
  :: Ptr (GlpkTree a)
  -> IO (Ptr Problem)

foreign import ccall "glp_ios_tree_size" glp_ios_tree_size
  :: Ptr (GlpkTree a)
  -> Ptr CInt
  -> Ptr CInt
  -> Ptr CInt
  -> IO ()

foreign import ccall "glp_ios_curr_node" glp_ios_curr_node
  :: Ptr (GlpkTree a)
  -> IO CInt

foreign import ccall "glp_ios_next_node" glp_ios_next_node
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO CInt

foreign import ccall "glp_ios_prev_node" glp_ios_prev_node
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO CInt

foreign import ccall "glp_ios_up_node" glp_ios_up_node
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO CInt

foreign import ccall "glp_ios_node_level" glp_ios_node_level
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO CInt

foreign import ccall "glp_ios_node_bound" glp_ios_node_bound
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO CDouble

foreign import ccall "glp_ios_best_node" glp_ios_best_node
  :: Ptr (GlpkTree a)
  -> IO CInt

foreign import ccall "glp_ios_mip_gap" glp_ios_mip_gap
  :: Ptr (GlpkTree a)
  -> IO CDouble

foreign import ccall "glp_ios_node_data" glp_ios_node_data
  :: Ptr (GlpkTree a)
  -> IO (Ptr a)

foreign import ccall "glp_ios_row_attr" glp_ios_row_attr
  :: Ptr (GlpkTree a)
  -> CInt
  -> Ptr GlpkCutAttribute
  -> IO ()

foreign import ccall "glp_ios_pool_size" glp_ios_pool_size
  :: Ptr (GlpkTree a)
  -> IO CInt

foreign import ccall "glp_ios_add_row" glp_ios_add_row
  :: Ptr (GlpkTree a)
  -> CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CInt
  -> Ptr CDouble
  -> CInt
  -> CDouble
  -> IO ()

foreign import ccall "glp_ios_del_row" glp_ios_del_row
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO ()

foreign import ccall "glp_ios_clear_pool" glp_ios_clear_pool
  :: Ptr (GlpkTree a)
  -> IO ()

foreign import ccall "glp_ios_can_branch" glp_ios_can_branch
  :: Ptr (GlpkTree a)
  -> VariableIndex

foreign import ccall "glp_ios_branch_upon" glp_ios_branch_upon
  :: Ptr (GlpkTree a)
  -> VariableIndex
  -> GlpkBranchOption
  -> IO ()

foreign import ccall "glp_ios_select_node" glp_ios_select_node
  :: Ptr (GlpkTree a)
  -> CInt
  -> IO ()

foreign import ccall "glp_ios_heur_sol" glp_ios_heur_sol
  :: Ptr (GlpkTree a)
  -> Ptr CDouble
  -> IO ()

foreign import ccall "glp_ios_terminate" glp_ios_terminate
  :: Ptr (GlpkTree a)
  -> IO ()

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
