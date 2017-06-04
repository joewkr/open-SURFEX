!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SET_SURFEX_FILEIN(HPROGRAM,HMASK)
!     ############################################
!
!
!!****  *SET_SURFEX_FILEIN* - set file name to read
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC,ONLY : CFILEIN_SAVE, CFILEPGD
USE MODI_SET_SURFEX_FILE_NAME_ASC
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA, ONLY : CFILEIN_FA_SAVE, CFILEPGD_FA
USE MODI_SET_SURFEX_FILE_NAME_FA
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI,ONLY : CFILEIN_LFI_SAVE, CFILEPGD_LFI
USE MODI_SET_SURFEX_FILE_NAME_LFI
#endif
#ifdef SFX_NC
USE MODD_IO_SURF_NC, ONLY : CFILEIN_NC_SAVE, CFILEPGD_NC
USE MODI_SET_SURFEX_FILE_NAME_NC
#endif
#ifdef SFX_MNH
USE MODI_SET_SURFEX_FILE_NAME_MNH
#endif
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "set_surfex_file_name_aro.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
 CHARACTER(LEN=4),  INTENT(IN)  :: HMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28)              :: YFILE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SET_SURFEX_FILEIN',0,ZHOOK_HANDLE)
!
IF(HMASK/='PGD'.AND.HMASK/='PREP') CALL ABOR1_SFX('SET_SURFEX_FILEIN: MASK='//HMASK//' REQUIRES MASK = PGD or PREP')
!
IF (HPROGRAM=='ASCII ' ) THEN
!  
#ifdef SFX_ASC
  IF(HMASK=='PGD')THEN
    YFILE=CFILEPGD
  ELSE
    YFILE=CFILEIN_SAVE
  ENDIF
  CALL SET_SURFEX_FILE_NAME_ASC(HNAME_IN=YFILE)
#endif  
!  
ENDIF
!
IF (HPROGRAM=='FA    ' ) THEN
! 
#ifdef SFX_FA
  IF(HMASK=='PGD')THEN
    YFILE=CFILEPGD_FA
  ELSE
    YFILE=CFILEIN_FA_SAVE
  ENDIF        
  CALL SET_SURFEX_FILE_NAME_FA(HNAME_IN=YFILE)
#endif  
!
ENDIF
!
IF (HPROGRAM=='LFI   ' ) THEN
!
#ifdef SFX_LFI
  IF(HMASK=='PGD')THEN
    YFILE=CFILEPGD_LFI
  ELSE
    YFILE=CFILEIN_LFI_SAVE
  ENDIF 
  CALL SET_SURFEX_FILE_NAME_LFI(HNAME_IN=YFILE)
#endif  
!
ENDIF
!
IF (HPROGRAM=='NC    ' ) THEN
!
#ifdef SFX_NC
  IF(HMASK=='PGD')THEN
    YFILE=CFILEPGD_NC
  ELSE
    YFILE=CFILEIN_NC_SAVE
  ENDIF 
  CALL SET_SURFEX_FILE_NAME_NC(HNAME_IN=YFILE)
#endif  
!
ENDIF

!
IF (HPROGRAM=='AROME ' ) THEN
#ifdef SFX_ARO
  CALL SET_SURFEX_FILE_NAME_ARO(HMASK)
#endif  
ENDIF
!
IF (HPROGRAM=='MESONH' ) THEN
#ifdef SFX_MNH
  CALL SET_SURFEX_FILE_NAME_MNH(HMASK)
#endif  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SET_SURFEX_FILEIN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SET_SURFEX_FILEIN
