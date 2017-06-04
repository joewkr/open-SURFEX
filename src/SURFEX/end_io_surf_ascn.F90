!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE END_IO_SURF_ASC_n(HPROGRAM)
!     #######################################################
!
!!****  *END_IO_SURF_ASC_n* - routine to close IO files
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
USE MODD_IO_SURF_ASC, ONLY : NUNIT, NFULL, CMASK, CFILEIN, CFILEOUT, &
                             CFILE, NMASK
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_ASC_N',0,ZHOOK_HANDLE)
!
IF (CFILE==CFILEOUT .AND. NRANK==NPIO .OR. CFILE==CFILEIN) THEN
  CLOSE(NUNIT)
ENDIF
!
CFILE = '                              '
!
NFULL = 0
!
NUNIT = 0
!
CMASK = '      '
!
NMASK=>NULL()
!
IF (LHOOK) CALL DR_HOOK('END_IO_SURF_ASC_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE END_IO_SURF_ASC_n
