!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE WRITE_HEADER_MNH
!     #############################################################
!
!!****  * - routine to header-type fields in a lfi file to emulate a MesoNH file
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     21/05/08
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
#ifdef SFX_LFI
USE MODI_FMWRIT
#endif
!
USE MODD_IO_SURF_LFI,        ONLY : CFILEOUT_LFI, CLUOUT_LFI, LMNH_COMPATIBLE, LCARTESIAN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
!*      0.2   Declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT=' '
INTEGER            :: IRESP
INTEGER            :: INB ! number of articles in the file
 CHARACTER(LEN=28) :: YNAME
 CHARACTER(LEN=10) :: YBIBUSER =' '
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_HEADER_MNH',0,ZHOOK_HANDLE)
#ifdef SFX_LFI
 CALL FMOPEN(CFILEOUT_LFI,'UNKNOWN',CLUOUT_LFI,0,1,1,INB,IRESP)
!
 CALL FMWRITN0(CFILEOUT_LFI,'MASDEV',CLUOUT_LFI,1,47,4,100,YCOMMENT,IRESP)
 CALL FMWRITN0(CFILEOUT_LFI,'BUGFIX',CLUOUT_LFI,1,0,4,100,YCOMMENT,IRESP)
 CALL FMWRITC0(CFILEOUT_LFI,'BIBUSER',CLUOUT_LFI,1,YBIBUSER,4,100,YCOMMENT,IRESP)
YNAME=CFILEOUT_LFI
 CALL FMWRITC0(CFILEOUT_LFI,'MY_NAME',CLUOUT_LFI,1,YNAME,4,100,YCOMMENT,IRESP)
YNAME=' '
 CALL FMWRITC0(CFILEOUT_LFI,'DAD_NAME',CLUOUT_LFI,1,YNAME,4,100,YCOMMENT,IRESP)
 CALL FMWRITC0(CFILEOUT_LFI,'PROGRAM',CLUOUT_LFI,1,'SURFEX',4,100,YCOMMENT,IRESP)
 CALL FMWRITN0(CFILEOUT_LFI,'KMAX',CLUOUT_LFI,1,0,4,100,YCOMMENT,IRESP)
 CALL FMWRITC0(CFILEOUT_LFI,'STORAGE_TYPE',CLUOUT_LFI,1,'SU',4,100,YCOMMENT,IRESP)
 CALL FMWRITL0(CFILEOUT_LFI,'CARTESIAN       ',CLUOUT_LFI,1,LCARTESIAN,4,100,YCOMMENT,IRESP)
 CALL FMWRITL0(CFILEOUT_LFI,'THINSHELL       ',CLUOUT_LFI,1,.TRUE.,4,100,YCOMMENT,IRESP)
!
 CALL FMCLOS(CFILEOUT_LFI,'KEEP',CLUOUT_LFI,IRESP)
#endif
IF (LHOOK) CALL DR_HOOK('WRITE_HEADER_MNH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_HEADER_MNH
