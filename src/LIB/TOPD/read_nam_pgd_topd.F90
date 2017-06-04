!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!----------------------------------------------------------------------------!
!     ##############################################################
      SUBROUTINE READ_NAM_PGD_TOPD(HPROGRAM,OCOUPL_TOPD,HCAT,PF_PARAM_BV,PC_DEPTH_RATIO_BV)
!     ##############################################################
!
!!**** *READ_NAM_TOPD_n* reads namelist NAM_TOPD
!!
!!    PURPOSE
!!    -------
!!    NAM_TOPD is a namelist used only for Topmodel coupling
!!    It permits to define the different catchments studied.
!!    This routine aims at reading and initialising those names.
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Vincendon        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original   11/2006
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODD_TOPD_PAR, ONLY : JPCAT
USE MODD_TOPODYN, ONLY : NNCAT
!
USE MODE_POS_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)   :: HPROGRAM     ! Type of program
LOGICAL, INTENT(OUT) :: OCOUPL_TOPD
 CHARACTER(LEN=15), DIMENSION(JPCAT),INTENT(OUT)  :: HCAT         ! Names of catchments         
REAL, DIMENSION(JPCAT),INTENT(OUT)               :: PF_PARAM_BV
REAL, DIMENSION(JPCAT),INTENT(OUT)               :: PC_DEPTH_RATIO_BV 
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=15), DIMENSION(JPCAT) :: CCAT
LOGICAL                           :: LCOUPL_TOPD
REAL, DIMENSION(JPCAT)            :: XF_PARAM_BV
REAL, DIMENSION(JPCAT)            :: XC_DEPTH_RATIO_BV
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!  
!
NAMELIST/NAM_PGD_TOPD/CCAT, LCOUPL_TOPD, XF_PARAM_BV, XC_DEPTH_RATIO_BV   
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TOPD',0,ZHOOK_HANDLE)
!
!*    1.      Initializations of defaults
!             ---------------------------
!
LCOUPL_TOPD = .FALSE.
CCAT(:) = '   '
XF_PARAM_BV(:) = 2.5
XC_DEPTH_RATIO_BV(:) = 1.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!CALL OPEN_NAMELIST(HPROGRAM,'SURF  ',ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_PGD_TOPD',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGD_TOPD)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!         2.   Initialises number of catchments and time step variables
!              -------------------------------------------------------
!
NNCAT=COUNT(CCAT(:)/='   ')
!
!-------------------------------------------------------------------------------
!
!*    3.      Fills output arguments
!             ----------------------
!
OCOUPL_TOPD = LCOUPL_TOPD
HCAT(1:NNCAT) = CCAT(1:NNCAT)
PF_PARAM_BV(1:NNCAT) = XF_PARAM_BV(1:NNCAT)
PC_DEPTH_RATIO_BV(1:NNCAT) = XC_DEPTH_RATIO_BV(1:NNCAT)
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TOPD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_TOPD
