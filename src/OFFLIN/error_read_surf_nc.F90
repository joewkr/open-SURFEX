!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ERROR_READ_SURF_NC(HREC,KRESP)
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(IN) :: HREC
INTEGER,          INTENT(IN):: KRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!   WRITE(*,*) ' '
!   WRITE(*,*) 'WARNING'
!   WRITE(*,*) '-------'
!   WRITE(*,*) ' '
    IF (LHOOK) CALL DR_HOOK('ERROR_READ_SURF_NC',0,ZHOOK_HANDLE)
    WRITE(*,*) 'error when reading article ',HREC,'KRESP=',KRESP
    CALL ABOR1_SFX('READ_SURF_NC: ERROR WHEN READING '//HREC)
    IF (LHOOK) CALL DR_HOOK('ERROR_READ_SURF_NC',1,ZHOOK_HANDLE)
!   WRITE(*,*) "default value may be used; who knows?"
!   WRITE(*,*) ' '

END SUBROUTINE ERROR_READ_SURF_NC
