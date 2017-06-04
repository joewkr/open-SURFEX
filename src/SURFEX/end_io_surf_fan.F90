!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE END_IO_SURF_FA_n(HPROGRAM)
!     #######################################################
!
!!****  *END_IO_SURF_FA_n* - routine to close IO files
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NFULL, CMASK, LOPEN, CFILEIN_FA, CFILEOUT_FA, CFILE_FA, &
                            NMASK
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, WLOG_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER :: IRET ! error code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_FA_N',0,ZHOOK_HANDLE)
!
NFULL = 0
!
CMASK = '      '
!
IF ((CFILE_FA==CFILEOUT_FA .AND. NRANK==NPIO .OR. CFILE_FA==CFILEIN_FA).AND.LOPEN) THEN
  CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
  NUNIT_FA = 0
  LOPEN = .FALSE.
END IF
!
CFILE_FA = '                            '
!
NMASK=>NULL()
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_FA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE END_IO_SURF_FA_n
