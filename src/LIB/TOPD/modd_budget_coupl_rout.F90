!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      MODULE MODD_BUDGET_COUPL_ROUT
!     ######################
!
!!****  *MODD_BUDGET_COUPL_ROUT* - declaration of variables
!                                  useful for budget computations when
!                                  coupling with TOPMODEL
!!
!!    PURPOSE
!!    -------
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!                
!!    AUTHOR
!!    ------
!!      B. Vincendon *Meteo France*
!!
!!    MODIFICATIONS
!!      Original   11/2006
!!                 03/2014 (B. Vincendon) add horton runoff variable
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
! Water entering the system
REAL,ALLOCATABLE,DIMENSION(:) :: XB_RAIN, XB_SNOW ! Rain and Snow
! Water going out of the system
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WR ! Interception
REAL,ALLOCATABLE,DIMENSION(:) :: XB_EVAP! Evaporation
REAL,ALLOCATABLE,DIMENSION(:) :: XB_RUNOFF_TOPD,XB_RUNOFF_ISBA !Runoff
REAL,ALLOCATABLE,DIMENSION(:) :: XB_HORTON !Horton Runoff
REAL,ALLOCATABLE,DIMENSION(:) :: XB_DRAIN !Drainage
! Water in the ground
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WG2, XB_WG3, XB_WGTOT !liquid
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WGI2, XB_WGI3, XB_WGITOT !solid
!REAL,ALLOCATABLE,DIMENSION(:) :: XB_DWR_TOT,XB_DWI_TOT, XB_DW_TOT
REAL,ALLOCATABLE,DIMENSION(:) :: XB_SWE1, XB_SWE2, XB_SWE3, XB_SWETOT ! snow melt
!REAL,ALLOCATABLE,DIMENSION(:) :: XB_RUN2,XB_RUN3!bv pour verif
!
! Values at the previous time step
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WRM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_EVAPM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_RUNOFF_TOPDM,XB_RUNOFF_ISBAM 
REAL,ALLOCATABLE,DIMENSION(:) :: XB_HORTONM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_DRAINM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WG2M, XB_WG3M, XB_WGTOTM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_WGI2M,XB_WGI3M, XB_WGITOTM
REAL,ALLOCATABLE,DIMENSION(:) :: XB_SWE1M, XB_SWE2M, XB_SWE3M, XB_SWETOTM
!REAL,ALLOCATABLE,DIMENSION(:) :: XB_RUN2M,XB_RUN3M!bv pour verif
! Useful
REAL,ALLOCATABLE,DIMENSION(:)   :: XB_DG2, XB_DG3
REAL,ALLOCATABLE,DIMENSION(:)   :: XB_MESH_SIZE
REAL,ALLOCATABLE,DIMENSION(:,:) :: XB_ABV_BYMESH !fraction de chaque BV ds chaque maille 
REAL,ALLOCATABLE,DIMENSION(:)   :: XB_ATOP_BYMESH!fraction de tous BV ds chaque maille 
!
! Variable to keep and write budget terms
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XB_VAR_BV
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XB_VAR_NOBV
REAL,ALLOCATABLE,  DIMENSION(:,:)   :: XB_VAR_TOT
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XB_VAR_Q
!
CHARACTER(LEN=6),ALLOCATABLE,  DIMENSION(:)     :: YB_VAR  
!
! Discharge variables
!
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_QTOT,XB_QRUN
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_QDR
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_STOCK_TOT,XB_STOCK_RUN
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_STOCK_DR
! Values at the previous time step
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_QTOTM,XB_QRUNM
REAL, ALLOCATABLE, DIMENSION(:)  :: XB_QDRM
!
CHARACTER(LEN=6),ALLOCATABLE,  DIMENSION(:)  :: YB_VARQ  
!
END MODULE MODD_BUDGET_COUPL_ROUT
