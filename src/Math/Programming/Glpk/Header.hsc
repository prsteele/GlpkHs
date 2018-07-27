{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Programming.Glpk.Header where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <glpk.h>

data Problem

type ProblemPtr = Ptr Problem

-- | An array whose data begins at index 1
newtype GlpkArray a
  = GlpkArray
  { fromGplkArray :: Ptr a
  }
  deriving
    ( Show
    , Ord
    , Eq
    , Storable
    )

newtype VariableIndex
  = VariableIndex { fromVariableIndex :: CInt}
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    , Storable
    , Enum
    )

newtype ConstraintIndex
  = ConstraintIndex { fromConstraintIndex :: CInt}
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

foreign import ccall "glp_create_prob" glp_create_prob
  :: IO ProblemPtr

foreign import ccall "glp_add_cols" glp_add_cols
  :: ProblemPtr
  -> CInt
  -> IO VariableIndex

foreign import ccall "glp_add_rows" glp_add_rows
  :: ProblemPtr
  -> CInt
  -> IO ConstraintIndex

foreign import ccall "glp_set_row_bnds" glp_set_row_bnds
  :: ProblemPtr
  -> ConstraintIndex
  -> GlpkConstraintType
  -> CDouble
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_col_bnds" glp_set_col_bnds
  :: ProblemPtr
  -> VariableIndex
  -> GlpkConstraintType
  -> CDouble
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_obj_coef" glp_set_obj_coef
  :: ProblemPtr
  -> VariableIndex
  -> CDouble
  -> IO ()

foreign import ccall "glp_set_mat_row" glp_set_mat_row
  :: ProblemPtr
  -> ConstraintIndex
  -> CInt
  -> GlpkArray VariableIndex
  -> GlpkArray CDouble
  -> IO ()

foreign import ccall "glp_set_mat_col" glp_set_mat_col
  :: ProblemPtr
  -> VariableIndex
  -> CInt
  -> Ptr ConstraintIndex
  -> GlpkArray CDouble
  -> IO ()

foreign import ccall "glp_write_lp" glp_write_lp
  :: ProblemPtr
  -> Ptr CInt
  -> CString
  -> IO ()

foreign import ccall "glp_simplex" glp_simplex
  :: ProblemPtr
  -> Ptr CInt
  -> IO CInt

newtype GlpkMajorVersion = GlpkMajorVersion { fromGlpkMajorVersion :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

newtype GlpkMinorVersion = GlpkMinorVersion { fromGlpkMinorVersion :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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

newtype GlpkInteriorPointParams
  = GlpkInteriorPointParams { fromGlpkInteriorPointParams :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkInteriorPointParams
 , GlpkInteriorPointParams
 , glpkInteriorMessageOff = GLP_MSG_OFF
 , glpkInteriorMessageError = GLP_MSG_ERR
 , glpkInteriorMessageOn = GLP_MSG_ON
 , glpkInteriorMessageAll = GLP_MSG_ALL
 , glpkInteriorMessageDebug = GLP_MSG_DBG
 }

newtype GlpkSimplexMethod
  = GlpkSimplexMethod { fromGlpkSimplexMethod :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkBacktrackingTechnique
 , GlpkBacktrackingTechnique
 , glpkDepthFirstSearch = GLP_BT_DFS
 , glpkBreadthFirstSearch = GLP_BT_BFS
 , glpkBestLocalBound = GLP_BT_BLB
 , glpkBestProjectionHeuristic = GLP_BT_BPH
 }

newtype GlpkPostProcessingTechnique
  = GlpkPostProcessingTechnique { fromGlpkPostProcessingTechnique :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkPostProcessingTechnique
 , GlpkPostProcessingTechnique
 , glpkPostProcessNone = GLP_PP_NONE
 , glpkPostProcessRoot = GLP_PP_ROOT
 , glpkPostProcessAll = GLP_PP_ALL
 }

newtype GlpkFeasibilityPump
  = GlpkFeasibilityPump { fromGlpkFeasibilityPump :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkCliqueCuts
 , GlpkCliqueCuts
 , glpkCliqueCutsOn = GLP_ON
 , glpkCliqueCutsOff = GLP_OFF
 }

newtype GlpkMIPPresolve
  = GlpkMIPPresolve { fromGlpkMIPPresolve :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkMIPPresolve
 , GlpkMIPPresolve
 , glpkMIPPresolveOn = GLP_ON
 , glpkMIPPresolveOff = GLP_OFF
 }

newtype GlpkBinarization
  = GlpkBinarization { fromGlpkBinarization :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkBinarization
 , GlpkBinarization
 , glpkBinarizationOn = GLP_ON
 , glpkBinarizationOff = GLP_OFF
 }

newtype GlpkConstraintOrigin
  = GlpkConstraintOrigin { fromGlpkConstraintOrigin :: CInt }
  deriving
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
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
    ( Show
    , Read
    , Ord
    , Eq
    )

#{enum
   GlpkMPSFormat
 , GlpkMPSFormat
 , glpkMPSAncient = GLP_MPS_DECK
 , glpkMPSDeck = GLP_MPS_DECK
 , glpkMPSModern = GLP_MPS_FILE
 }
