!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CLOSE_FILEOUT_OL
!     #######################################################
!!****  *CLOSE_FILEOUT_OL* - closes netcdf files writen by surface
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
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010 
!-------------------------------------------------------------------------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_OL_FILEID, ONLY: XNETCDF_FILEID_OUT
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
INTEGER :: JFILE, IFILE_ID, JRET
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CLOSE_FILEOUT_OL',0,ZHOOK_HANDLE)
!
IF (NRANK==NPIO) THEN
  !
  DO JFILE = 1,SIZE(XNETCDF_FILEID_OUT)
    !
    IFILE_ID = XNETCDF_FILEID_OUT(JFILE)
    !
    JRET = NF90_CLOSE(IFILE_ID)
    !
  ENDDO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CLOSE_FILEOUT_OL',1,ZHOOK_HANDLE)
!
END SUBROUTINE CLOSE_FILEOUT_OL
