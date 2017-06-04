!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_DST(HPROGRAM)
!     #######################################################
!
USE MODI_DEFAULT_DST
!
USE MODI_READ_DEFAULT_DST
!
USE MODI_READ_DST_CONF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM ! program calling DST
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_DST',0,ZHOOK_HANDLE)
 CALL DEFAULT_DST
!
 CALL READ_DEFAULT_DST(HPROGRAM)
!
 CALL READ_DST_CONF(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_DST',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE READ_NAMELISTS_DST
