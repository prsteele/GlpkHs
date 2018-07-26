{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

data Problem

type ProblemPtr = Ptr Problem

#include <glpk.h>

foreign import ccall "glp_create_prob" glp_create_prob :: IO ProblemPtr
foreign import ccall "glp_add_cols" glp_add_cols :: ProblemPtr -> CInt -> IO CInt
foreign import ccall "glp_add_rows" glp_add_rows :: ProblemPtr -> CInt -> IO CInt
foreign import ccall "glp_set_row_bnds" glp_set_row_bnds :: ProblemPtr -> CInt -> CInt -> CDouble -> CDouble -> IO ()
foreign import ccall "glp_set_col_bnds" glp_set_col_bnds :: ProblemPtr -> CInt -> CInt -> CDouble -> CDouble -> IO ()
foreign import ccall "glp_set_col_bnds" glp_set_obj_coef :: ProblemPtr -> CInt -> CDouble -> IO ()
foreign import ccall "glp_set_mat_row" glp_set_mat_row :: ProblemPtr -> CInt -> CInt -> Ptr CInt -> IO ()
foreign import ccall "glp_set_mat_col" glp_set_mat_col :: ProblemPtr -> CInt -> CInt -> Ptr CInt -> IO ()
foreign import ccall "glp_write_lp" glp_write_lp :: ProblemPtr -> CString -> IO ()

glp_MAJOR_VERSION = #const GLP_MAJOR_VERSION
glp_MINOR_VERSION = #const GLP_MINOR_VERSION
glp_MIN = #const GLP_MIN
glp_MAX = #const GLP_MAX
glp_CV = #const GLP_CV
glp_IV = #const GLP_IV
glp_BV = #const GLP_BV
glp_FR = #const GLP_FR
glp_LO = #const GLP_LO
glp_UP = #const GLP_UP
glp_DB = #const GLP_DB
glp_FX = #const GLP_FX
glp_BS = #const GLP_BS
glp_NL = #const GLP_NL
glp_NU = #const GLP_NU
glp_NF = #const GLP_NF
glp_NS = #const GLP_NS
glp_SF_GM = #const GLP_SF_GM
glp_SF_EQ = #const GLP_SF_EQ
glp_SF_2N = #const GLP_SF_2N
glp_SF_SKIP = #const GLP_SF_SKIP
glp_SF_AUTO = #const GLP_SF_AUTO
glp_SOL = #const GLP_SOL
glp_IPT = #const GLP_IPT
glp_MIP = #const GLP_MIP
glp_UNDEF = #const GLP_UNDEF
glp_FEAS = #const GLP_FEAS
glp_INFEAS = #const GLP_INFEAS
glp_NOFEAS = #const GLP_NOFEAS
glp_OPT = #const GLP_OPT
glp_UNBND = #const GLP_UNBND
glp_MSG_OFF = #const GLP_MSG_OFF
glp_MSG_ERR = #const GLP_MSG_ERR
glp_MSG_ON = #const GLP_MSG_ON
glp_MSG_ALL = #const GLP_MSG_ALL
glp_MSG_DBG = #const GLP_MSG_DBG
glp_PRIMAL = #const GLP_PRIMAL
glp_DUALP = #const GLP_DUALP
glp_DUAL = #const GLP_DUAL
glp_PT_STD = #const GLP_PT_STD
glp_PT_PSE = #const GLP_PT_PSE
glp_RT_STD = #const GLP_RT_STD
glp_RT_HAR = #const GLP_RT_HAR
glp_ORD_NONE = #const GLP_ORD_NONE
glp_ORD_QMD = #const GLP_ORD_QMD
glp_ORD_AMD = #const GLP_ORD_AMD
glp_ORD_SYMAMD = #const GLP_ORD_SYMAMD
glp_BR_FFV = #const GLP_BR_FFV
glp_BR_LFV = #const GLP_BR_LFV
glp_BR_MFV = #const GLP_BR_MFV
glp_BR_DTH = #const GLP_BR_DTH
glp_BR_PCH = #const GLP_BR_PCH
glp_BT_DFS = #const GLP_BT_DFS
glp_BT_BFS = #const GLP_BT_BFS
glp_BT_BLB = #const GLP_BT_BLB
glp_BT_BPH = #const GLP_BT_BPH
glp_PP_NONE = #const GLP_PP_NONE
glp_PP_ROOT = #const GLP_PP_ROOT
glp_PP_ALL = #const GLP_PP_ALL
glp_RF_REG = #const GLP_RF_REG
glp_RF_LAZY = #const GLP_RF_LAZY
glp_RF_CUT = #const GLP_RF_CUT
glp_RF_GMI = #const GLP_RF_GMI
glp_RF_MIR = #const GLP_RF_MIR
glp_RF_COV = #const GLP_RF_COV
glp_RF_CLQ = #const GLP_RF_CLQ
glp_ON = #const GLP_ON
glp_OFF = #const GLP_OFF
glp_IROWGEN = #const GLP_IROWGEN
glp_IBINGO = #const GLP_IBINGO
glp_IHEUR = #const GLP_IHEUR
glp_ICUTGEN = #const GLP_ICUTGEN
glp_IBRANCH = #const GLP_IBRANCH
glp_ISELECT = #const GLP_ISELECT
glp_IPREPRO = #const GLP_IPREPRO
glp_NO_BRNCH = #const GLP_NO_BRNCH
glp_DN_BRNCH = #const GLP_DN_BRNCH
glp_UP_BRNCH = #const GLP_UP_BRNCH
glp_EBADB = #const GLP_EBADB
glp_ESING = #const GLP_ESING
glp_ECOND = #const GLP_ECOND
glp_EBOUND = #const GLP_EBOUND
glp_EFAIL = #const GLP_EFAIL
glp_EOBJLL = #const GLP_EOBJLL
glp_EOBJUL = #const GLP_EOBJUL
glp_EITLIM = #const GLP_EITLIM
glp_ETMLIM = #const GLP_ETMLIM
glp_ENOPFS = #const GLP_ENOPFS
glp_ENODFS = #const GLP_ENODFS
glp_EROOT = #const GLP_EROOT
glp_ESTOP = #const GLP_ESTOP
glp_EMIPGAP = #const GLP_EMIPGAP
glp_ENOFEAS = #const GLP_ENOFEAS
glp_ENOCVG = #const GLP_ENOCVG
glp_EINSTAB = #const GLP_EINSTAB
glp_EDATA = #const GLP_EDATA
glp_ERANGE = #const GLP_ERANGE
glp_KKT_PE = #const GLP_KKT_PE
glp_KKT_PB = #const GLP_KKT_PB
glp_KKT_DE = #const GLP_KKT_DE
glp_KKT_DB = #const GLP_KKT_DB
glp_KKT_CS = #const GLP_KKT_CS
glp_MPS_DECK = #const GLP_MPS_DECK
glp_MPS_FILE = #const GLP_MPS_FILE
