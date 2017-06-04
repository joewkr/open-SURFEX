!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE READ_FILE_ISBAMAP (UG, KUNIT,PVAR,KI)
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
!!      K. Chancibault  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   25/01/2005
!!                 03/2014 (E. Artinian) manages the option CGRID='IGN'
!!                 07/2015 (E. Artinian) get the real ZDXI and ZDYI values
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_TOPODYN
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODE_GRIDTYPE_CONF_PROJ
USE MODE_GRIDTYPE_LONLAT_REG
USE MODE_GRIDTYPE_IGN
!!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
INTEGER, INTENT(IN)             :: KUNIT  ! file unit
REAL, DIMENSION(:), INTENT(OUT) :: PVAR   ! variable to write in the file
INTEGER, INTENT(IN)             :: KI    ! Grid dimensions
!
!
!*      0.2    declarations of local variables
INTEGER                    :: JJ,JI,IL
INTEGER                    :: INI,ILAMBERT
INTEGER                    :: JINDEX ! reference number of the pixel
REAL                       :: ZOUT
REAL                       :: ZMAX,ZMIN
REAL, DIMENSION(KI)       :: ZXI, ZYI   ! natural coordinates of ISBA grid (conformal projection)
REAL, DIMENSION(KI)       :: ZXN, ZYN     ! isba nodes coordinates in the Lambert II coordinates - Eram rajout
REAL, DIMENSION(KI)       :: ZDXI, ZDYI   ! Isba grid resolution in the conformal projection
INTEGER                    :: IIMAX,IJMAX
REAL :: ZLONMIN,ZLONMAX,ZLATMIN,ZLATMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_FILE_ISBAMAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
IF(UG%G%CGRID.EQ.'CONF PROJ') THEN
 CALL GET_GRIDTYPE_CONF_PROJ(UG%G%XGRID_PAR,PX=ZXI,PY=ZYI,KIMAX=IIMAX,KJMAX=IJMAX,PDX=ZDXI)
ELSE IF(UG%G%CGRID.EQ.'LONLAT REG') THEN
    CALL GET_GRIDTYPE_LONLAT_REG(UG%G%XGRID_PAR,PLONMIN=ZLONMIN,PLONMAX=ZLONMAX,             &
                                 PLATMIN=ZLATMIN,PLATMAX=ZLATMAX,KLON=IIMAX,KLAT=IJMAX, &
                                 KL=IL,PLON=ZXI,PLAT=ZYI)
    !
    ZDXI(:)=(ZLONMAX-ZLONMIN)/(IIMAX-1)
    ZDYI(:)=(ZLATMAX-ZLATMIN)/(IJMAX-1)
ELSE IF (UG%G%CGRID=='IGN') THEN 
  CALL GET_GRIDTYPE_IGN(UG%G%XGRID_PAR,KLAMBERT=ILAMBERT,KL=INI,PX=ZXN,PY=ZYN,PDX=ZDXI,PDY=ZDYI)
  INI=KI        
ELSE
    CALL ABOR1_SFX("READ_FILE_ISBAMAP: TYPE DE GRILLE NON GERE PAR LE CODE")
ENDIF
!
ZOUT = XUNDEF
!
DO JJ=1,5
  READ(KUNIT,*)
ENDDO
!
IF(UG%G%CGRID.EQ.'IGN') THEN
    !
    READ(KUNIT,*) ZXN(1)
    READ(KUNIT,*) ZYN(1)
    READ(KUNIT,*) INI
    READ(KUNIT,*) ZOUT
    READ(KUNIT,*) ZDXI(1)
    READ(KUNIT,*) ZMIN
    READ(KUNIT,*) ZMAX
    !
    DO JJ=1,INI
        READ(KUNIT,*) PVAR(JJ)
    ENDDO
ELSE
    READ(KUNIT,*) ZXI(1)
    READ(KUNIT,*) ZYI(1)
    READ(KUNIT,*) IIMAX
    READ(KUNIT,*) IJMAX
    READ(KUNIT,*) ZOUT
    READ(KUNIT,*) ZDXI(1)
    READ(KUNIT,*) ZMIN
    READ(KUNIT,*) ZMAX
!
    DO JJ=1,IJMAX
      DO JI=1,IIMAX
        JINDEX=(JJ - 1) * IIMAX + JI
        READ(KUNIT,*) PVAR(JINDEX)
      ENDDO
    ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_FILE_ISBAMAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FILE_ISBAMAP
