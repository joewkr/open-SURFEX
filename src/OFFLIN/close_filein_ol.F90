!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######
      SUBROUTINE CLOSE_FILEIN_OL
!     #######################################################
!!****  *CLOSE_FILEIN_OL* - 
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_OL_FILEID, ONLY : XID_IN, XVAR_TO_FILEIN, XID_VARIN

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE

INTEGER :: JFILE, JRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!**************************************************

IF (LHOOK) CALL DR_HOOK('CLOSE_FILEIN_OL',0,ZHOOK_HANDLE)
IF (NRANK==NPIO) THEN
  !
  DO JFILE=1,SIZE(XID_IN)
    JRET=NF90_CLOSE(XID_IN(JFILE))
  ENDDO
  !
  DEALLOCATE(XID_IN)
  DEALLOCATE(XVAR_TO_FILEIN)
  DEALLOCATE(XID_VARIN)
  !
ENDIF
IF (LHOOK) CALL DR_HOOK('CLOSE_FILEIN_OL',1,ZHOOK_HANDLE)

!******************************************

END SUBROUTINE CLOSE_FILEIN_OL
