!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE READ_FILE_MASKTOPD(KI)
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
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN
USE MODD_COUPLING_TOPD, ONLY : NMASKI, NNPIX, NMASKT
USE MODD_SURF_PAR,        ONLY : XUNDEF, NUNDEF
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_READ_TOPD_FILE
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
!
!*      0.2    declarations of local variables
INTEGER                    :: JCAT,JMESH,JPIX
INTEGER,DIMENSION(KI)      :: JP_IN_M
INTEGER                    :: INUMPIX,IEOF,ITMP
INTEGER :: IIMAX,IJMAX
 CHARACTER(LEN=50) :: YNAME
REAL, DIMENSION(:),ALLOCATABLE    :: ZTOPD_READ !Topgraphic variable read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_FILE_MASKTOPD',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
ALLOCATE(ZTOPD_READ(NPMAX))
!
DO JCAT=1,NNCAT
  !
  YNAME=TRIM(CCAT(JCAT))//TRIM('.mask_topd')
  !
  CALL READ_TOPD_FILE('OFFLIN',YNAME,'FORMATTED',NNPT(JCAT),ZTOPD_READ)
  !
  DO JPIX=1,NPMAX
    !
    IF ( NLINE(JCAT,JPIX)/=0 ) THEN
      IF  (ZTOPD_READ(JPIX)==XUNDEF ) THEN
        NMASKT(JCAT,NLINE(JCAT,JPIX)) = NUNDEF
      ELSE
        NMASKT(JCAT,NLINE(JCAT,JPIX)) = FLOOR(ZTOPD_READ(JPIX))
      ENDIF
    ENDIF
    !
  ENDDO
  !
ENDDO
!
ALLOCATE(NNPIX(KI))
NNPIX(:) = NUNDEF
DO JMESH=1,KI
  NNPIX(JMESH) = COUNT(NMASKT(:,:)==JMESH)
ENDDO
INUMPIX=MAXVAL(NNPIX)
!
ALLOCATE(NMASKI(KI,NNCAT,INUMPIX))
NMASKI(:,:,:) = NUNDEF
DO JCAT=1,NNCAT
JP_IN_M(:)=1
  !
  YNAME=TRIM(CCAT(JCAT))//TRIM('.mask_surf')
  CALL OPEN_FILE('ASCII ',NUNIT,YNAME,'FORMATTED','READ')
  ! 
  IEOF=0
  DO WHILE(IEOF==0)
      READ(NUNIT,*,IOSTAT=IEOF) JMESH,ITMP 
      NMASKI(JMESH,JCAT,JP_IN_M(JMESH))=ITMP
      JP_IN_M(JMESH)=JP_IN_M(JMESH)+1
  ENDDO
  !
  CALL CLOSE_FILE('ASCII ',NUNIT)
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_FILE_MASKTOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FILE_MASKTOPD
