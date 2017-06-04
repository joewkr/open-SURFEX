!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################
      SUBROUTINE CLOSE_AUX_IO_SURF_FA(HFILE,HFILETYPE)
!     #######################################################
!
!!****  *CLOSE_AUX_IO_SURF* - chooses the routine to OPENialize IO
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    B. Decharme (03/2014) read fa file in prep
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_SURF_FA, ONLY : NUNIT_FA, NFULL, CMASK, NLUOUT, NMASK, CFILEIN_FA, CDNOMC, NMASK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
INTEGER :: IRET ! error code
CHARACTER(LEN=28) :: YFILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_FA',0,ZHOOK_HANDLE)
!
NFULL = 0
CMASK='      '
DEALLOCATE(NMASK)
CALL FAIRME(IRET,NUNIT_FA,'UNKNOWN')
!
YFILE=HFILE(1:LEN_TRIM(HFILE))//'.fa'
WRITE(NLUOUT,*)'HFILETYPE ',HFILETYPE,'END EXTERNAL',NUNIT_FA,YFILE
!
CFILEIN_FA = '    '
NUNIT_FA = 0
!
CDNOMC = 'header'
!
IF (LHOOK) CALL DR_HOOK('CLOSE_AUX_IO_SURF_FA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CLOSE_AUX_IO_SURF_FA
