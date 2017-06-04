!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
        SUBROUTINE HANDLE_ERR(IRET,HNAME)

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER,           INTENT(IN) :: IRET
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
REAL(KIND=JPRB) :: ZHOOK_HANDLE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - 

        IF (LHOOK) CALL DR_HOOK('HANDLE_ERR',0,ZHOOK_HANDLE)
        IF (IRET /= NF90_NOERR) THEN
           WRITE(*,*)'HANDLE_ERR: ',NF90_STRERROR(IRET)
           CALL ABOR1_SFX('HANDLE_ERR: ABORTING PROGRAM TO WRITE A NETCDF FILE: '//HNAME)
        ENDIF
IF (LHOOK) CALL DR_HOOK('HANDLE_ERR',1,ZHOOK_HANDLE)

END SUBROUTINE HANDLE_ERR
