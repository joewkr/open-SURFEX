!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_MASKTOPD(KI)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon    * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   11/2011
!!                 03/2014 (B. Vincendon) modification of mask_surf files format
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN, ONLY : CCAT, NNCAT
USE MODD_COUPLING_TOPD, ONLY : NMASKI, NNPIX
USE MODD_SURF_PAR,        ONLY : NUNDEF
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODE_GRIDTYPE_CONF_PROJ
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)             :: KI    ! Grid dimensions
!
!*      0.2    declarations of local variables
INTEGER           :: JCAT,JMESH,JPIX
 CHARACTER(LEN=50) :: YNAME
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MASKTOPD',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
DO JCAT=1,NNCAT
  !
  YNAME = TRIM(CCAT(JCAT))//TRIM('.mask_surf')
  !
  CALL OPEN_FILE('ASCII ',NUNIT,YNAME,'FORMATTED',HACTION='WRITE')
  !
  DO JMESH=1,KI
    DO JPIX=1,NNPIX(JMESH)
    IF (NMASKI(JMESH,JCAT,JPIX)/=NUNDEF)&
      WRITE(NUNIT,*) JMESH,NMASKI(JMESH,JCAT,JPIX)
    ENDDO
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_MASKTOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_MASKTOPD
