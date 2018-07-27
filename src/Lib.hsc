{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data Problem

type ProblemPtr = Ptr Problem

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

#include <glpk.h>

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
  -> Ptr VariableIndex
  -> Ptr CDouble
  -> IO ()

foreign import ccall "glp_set_mat_col" glp_set_mat_col
  :: ProblemPtr
  -> VariableIndex
  -> CInt
  -> Ptr ConstraintIndex
  -> Ptr CDouble
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


glp_SF_GM = CInt #const GLP_SF_GM
glp_SF_EQ = CInt #const GLP_SF_EQ
glp_SF_2N = CInt #const GLP_SF_2N
glp_SF_SKIP = CInt #const GLP_SF_SKIP
glp_SF_AUTO = CInt #const GLP_SF_AUTO
glp_SOL = CInt #const GLP_SOL
glp_IPT = CInt #const GLP_IPT
glp_MIP = CInt #const GLP_MIP
glp_UNDEF = CInt #const GLP_UNDEF
glp_FEAS = CInt #const GLP_FEAS
glp_INFEAS = CInt #const GLP_INFEAS
glp_NOFEAS = CInt #const GLP_NOFEAS
glp_OPT = CInt #const GLP_OPT
glp_UNBND = CInt #const GLP_UNBND
glp_MSG_OFF = CInt #const GLP_MSG_OFF
glp_MSG_ERR = CInt #const GLP_MSG_ERR
glp_MSG_ON = CInt #const GLP_MSG_ON
glp_MSG_ALL = CInt #const GLP_MSG_ALL
glp_MSG_DBG = CInt #const GLP_MSG_DBG
glp_PRIMAL = CInt #const GLP_PRIMAL
glp_DUALP = CInt #const GLP_DUALP
glp_DUAL = CInt #const GLP_DUAL
glp_PT_STD = CInt #const GLP_PT_STD
glp_PT_PSE = CInt #const GLP_PT_PSE
glp_RT_STD = CInt #const GLP_RT_STD
glp_RT_HAR = CInt #const GLP_RT_HAR
glp_ORD_NONE = CInt #const GLP_ORD_NONE
glp_ORD_QMD = CInt #const GLP_ORD_QMD
glp_ORD_AMD = CInt #const GLP_ORD_AMD
glp_ORD_SYMAMD = CInt #const GLP_ORD_SYMAMD
glp_BR_FFV = CInt #const GLP_BR_FFV
glp_BR_LFV = CInt #const GLP_BR_LFV
glp_BR_MFV = CInt #const GLP_BR_MFV
glp_BR_DTH = CInt #const GLP_BR_DTH
glp_BR_PCH = CInt #const GLP_BR_PCH
glp_BT_DFS = CInt #const GLP_BT_DFS
glp_BT_BFS = CInt #const GLP_BT_BFS
glp_BT_BLB = CInt #const GLP_BT_BLB
glp_BT_BPH = CInt #const GLP_BT_BPH
glp_PP_NONE = CInt #const GLP_PP_NONE
glp_PP_ROOT = CInt #const GLP_PP_ROOT
glp_PP_ALL = CInt #const GLP_PP_ALL
glp_RF_REG = CInt #const GLP_RF_REG
glp_RF_LAZY = CInt #const GLP_RF_LAZY
glp_RF_CUT = CInt #const GLP_RF_CUT
glp_RF_GMI = CInt #const GLP_RF_GMI
glp_RF_MIR = CInt #const GLP_RF_MIR
glp_RF_COV = CInt #const GLP_RF_COV
glp_RF_CLQ = CInt #const GLP_RF_CLQ
glp_ON = CInt #const GLP_ON
glp_OFF = CInt #const GLP_OFF
glp_IROWGEN = CInt #const GLP_IROWGEN
glp_IBINGO = CInt #const GLP_IBINGO
glp_IHEUR = CInt #const GLP_IHEUR
glp_ICUTGEN = CInt #const GLP_ICUTGEN
glp_IBRANCH = CInt #const GLP_IBRANCH
glp_ISELECT = CInt #const GLP_ISELECT
glp_IPREPRO = CInt #const GLP_IPREPRO
glp_NO_BRNCH = CInt #const GLP_NO_BRNCH
glp_DN_BRNCH = CInt #const GLP_DN_BRNCH
glp_UP_BRNCH = CInt #const GLP_UP_BRNCH
glp_EBADB = CInt #const GLP_EBADB
glp_ESING = CInt #const GLP_ESING
glp_ECOND = CInt #const GLP_ECOND
glp_EBOUND = CInt #const GLP_EBOUND
glp_EFAIL = CInt #const GLP_EFAIL
glp_EOBJLL = CInt #const GLP_EOBJLL
glp_EOBJUL = CInt #const GLP_EOBJUL
glp_EITLIM = CInt #const GLP_EITLIM
glp_ETMLIM = CInt #const GLP_ETMLIM
glp_ENOPFS = CInt #const GLP_ENOPFS
glp_ENODFS = CInt #const GLP_ENODFS
glp_EROOT = CInt #const GLP_EROOT
glp_ESTOP = CInt #const GLP_ESTOP
glp_EMIPGAP = CInt #const GLP_EMIPGAP
glp_ENOFEAS = CInt #const GLP_ENOFEAS
glp_ENOCVG = CInt #const GLP_ENOCVG
glp_EINSTAB = CInt #const GLP_EINSTAB
glp_EDATA = CInt #const GLP_EDATA
glp_ERANGE = CInt #const GLP_ERANGE
glp_KKT_PE = CInt #const GLP_KKT_PE
glp_KKT_PB = CInt #const GLP_KKT_PB
glp_KKT_DE = CInt #const GLP_KKT_DE
glp_KKT_DB = CInt #const GLP_KKT_DB
glp_KKT_CS = CInt #const GLP_KKT_CS
glp_MPS_DECK = CInt #const GLP_MPS_DECK
glp_MPS_FILE = CInt #const GLP_MPS_FILE
