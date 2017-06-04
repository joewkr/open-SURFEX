!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_PERM_SNOW (IO, KK, PK, PEK)
!          ################################################
!
!
!!****  *PREP_PERM_SNOW* - takes into account permanent snow into prognostic snow
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme 03/2009: Consistency with Arpege permanent
!!                                          snow/ice treatment
!!      B. Decharme 07/2012: 3-L or Crocus adjustments
!!      M. Lafaysse 09/2012: adaptation with new snow age in Crocus
!!------------------------------------------------------------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_TYPE_SNOW
USE MODD_CSTS,           ONLY : XTT
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_SNOW_PAR,       ONLY : XRHOSMAX, XANSMAX, XANSMIN, &
                                XAGLAMAX, XAGLAMIN, XHGLA,  &
                                XRHOSMAX_ES
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_ISBA_PAR,       ONLY : XWGMIN
!
USE MODI_VEGTYPE_TO_PATCH
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_MKFLAG_SNOW
USE MODE_SURF_SNOW_FRAC
USE MODE_SNOW3L
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER :: ZRHOL1 = 150.
!
!*      0.3    declarations of local variables
!
INTEGER                             :: JL      ! loop counter on snow layers
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW_PERM ! snow total reservoir due to perm. snow
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW      ! initial snow total reservoir
REAL, DIMENSION(:),   ALLOCATABLE   :: ZD          ! new snow total depth
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZDEPTH      ! depth of each layer
REAL, DIMENSION(:,:), ALLOCATABLE :: ZT          ! new snow temperature profile
REAL, DIMENSION(:),   ALLOCATABLE   :: ZPSN        ! permanent snow fraction
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZWAT        ! 
!
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GWORK
INTEGER                              :: IWORK
!
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
INTEGER :: INFOMPI
INTEGER :: ISNOW          ! patch number where permanent snow is
!
REAL, DIMENSION(0:NPROC-1) :: ZPSN0
REAL :: ZSUM_PSN
REAL              ::ZRHOSMAX
REAL              ::ZAGE_NOW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*       1.    Snow where permanent snow is
!              ----------------------------
!
!* snow fraction must be at least equal to permanent snow fraction
!  The snow fraction is computed as Wsnow/(Wsnow+XWCRN)
!
!
IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',0,ZHOOK_HANDLE)
!
ZRHOSMAX=XRHOSMAX
IF(PEK%TSNOW%SCHEME=='3-L'.OR.PEK%TSNOW%SCHEME=='CRO')THEN
  ZRHOSMAX=XRHOSMAX_ES
ENDIF
!
ALLOCATE(ZPSN(SIZE(PEK%XTG,1)))

ZPSN(:) = MIN ( PK%XVEGTYPE_PATCH(:,NVT_SNOW) , 0.9999 )
!
!* if no permanent snow present
!
ZSUM_PSN = SUM(ZPSN(:))
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_ALLGATHER(ZSUM_PSN,KIND(ZSUM_PSN)/4,MPI_REAL,&
                     ZPSN0,KIND(ZPSN0)/4,MPI_REAL,NCOMM,INFOMPI)
#endif
ELSE
  ZPSN0(:) = ZSUM_PSN
ENDIF

IF (ALL(ZPSN0(:)==0.)) THEN
  DEALLOCATE(ZPSN) 
  IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!* total snow amount due to permanent snow
!
ALLOCATE(ZWSNOW_PERM(SIZE(PEK%XTG,1)))
ZWSNOW_PERM(:) = WSNOW_FROM_SNOW_FRAC_GROUND(ZPSN)
!
!* limitation of maximum snow amount
!
IF(IO%LGLACIER)THEN
!  limited to 33.3 meters of aged snow
   ZWSNOW_PERM(:) = MIN(ZWSNOW_PERM(:),XHGLA * ZRHOSMAX )
ELSE
!  limited to 2. meters of aged snow
   ZWSNOW_PERM(:) = MIN(ZWSNOW_PERM(:),2.0 * ZRHOSMAX )
ENDIF
!
!* permanent snow can be added only if deep soil temperature is below 5 C
!  (glaciers on subgrid mountains tops that are contained in the grid mesh are neglected)
!
IF(.NOT.IO%LGLACIER)THEN
  WHERE(PEK%XTG(:,SIZE(PEK%XTG,2))>XTT+5.) ZWSNOW_PERM(:) = 0.
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       2.    Other parameters of new snow, except temperature
!              ------------------------------------------------
!
!* rho must be defined for snow 3-L before temperature and heat computations
!
ALLOCATE(GWORK(SIZE(PEK%XTG,1),PEK%TSNOW%NLAYER))
!
DO JL=1,PEK%TSNOW%NLAYER
!
  GWORK(:,JL)=.FALSE.
!
  IF(IO%LGLACIER)THEN
    WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JL)=.TRUE.
  ELSE
    WHERE(ZWSNOW_PERM(:)>0..AND.PEK%TSNOW%WSNOW(:,JL)==0.)GWORK(:,JL)=.TRUE.
  ENDIF
!
!* rho
!
  WHERE(GWORK(:,JL))
    PEK%TSNOW%RHO(:,JL) = ZRHOSMAX
  END WHERE
!
!* albedo
!
  IF(IO%LGLACIER)THEN
    WHERE(GWORK(:,JL))
      PEK%TSNOW%ALB(:) = (XAGLAMAX+XAGLAMIN)/2.0
    END WHERE
  ELSE
    WHERE(GWORK(:,JL))
      PEK%TSNOW%ALB(:) = (XANSMAX+XANSMIN)/2.0
    END WHERE
  ENDIF
!
END DO
!
IF (PEK%TSNOW%SCHEME=='3-L'.OR.PEK%TSNOW%SCHEME=='CRO') THEN
!
! * optimized rho perm snow profile
!
  IF(IO%LGLACIER.AND.PEK%TSNOW%NLAYER>=6)THEN
    WHERE(GWORK(:,1))
      PEK%TSNOW%RHO(:,1) = ZRHOL1
    END WHERE 
    IF(PEK%TSNOW%NLAYER>=6.AND.PEK%TSNOW%NLAYER<12)THEN
      WHERE(GWORK(:,2))
       PEK%TSNOW%RHO(:,2) = ZRHOL1 + 100.
      END WHERE 
      WHERE(GWORK(:,3))
       PEK%TSNOW%RHO(:,3) = ZRHOL1 + 250.
      END WHERE 
    ELSE
      DO JL=2,PEK%TSNOW%NLAYER
        WHERE(GWORK(:,JL))
          PEK%TSNOW%RHO(:,JL) = MIN(ZRHOSMAX,PEK%TSNOW%RHO(:,JL-1)+100.)
        END WHERE     
      ENDDO
    ENDIF
  ENDIF
!
! * Snow age profile
!
  DO JL=1,PEK%TSNOW%NLAYER/4
    WHERE(GWORK(:,JL))
      PEK%TSNOW%AGE(:,JL) = 365.0*FLOAT(JL-1)/ FLOAT(PEK%TSNOW%NLAYER)
    END WHERE
  END DO
  DO JL=1+PEK%TSNOW%NLAYER/4,PEK%TSNOW%NLAYER
    WHERE(GWORK(:,JL))
      PEK%TSNOW%AGE(:,JL) = 3650.*FLOAT(JL-1)/ FLOAT(PEK%TSNOW%NLAYER) 
    END WHERE
  END DO
!
  IF(IO%LGLACIER)THEN
    WHERE(GWORK(:,:))PEK%TSNOW%AGE(:,:) = 0.0
  ENDIF
!
END IF
!
IF (PEK%TSNOW%SCHEME=='CRO') THEN
DO JL=1,PEK%TSNOW%NLAYER/4
  WHERE(GWORK(:,JL))
    PEK%TSNOW%GRAN1(:,JL) = MIN(-1.,-99.* (1.-4*FLOAT(JL)/FLOAT(PEK%TSNOW%NLAYER))) 
    PEK%TSNOW%GRAN2(:,JL) = 50. 
    PEK%TSNOW%HIST(:,JL) = 0 
  END WHERE
END DO
DO JL=1+PEK%TSNOW%NLAYER/4,PEK%TSNOW%NLAYER
  WHERE(GWORK(:,JL))
    PEK%TSNOW%GRAN1(:,JL) = 99. 
    PEK%TSNOW%GRAN2(:,JL) = 0.0003 
    PEK%TSNOW%HIST(:,JL) = 0 
  END WHERE
END DO
END IF
!
!-------------------------------------------------------------------------------------
!
!*       3.    Modification of snow reservoir profile
!              --------------------------------------
!
!* initial snow content
!
ALLOCATE(ZWSNOW(SIZE(PEK%XTG,1)))
ZWSNOW(:) = 0.
DO JL=1,PEK%TSNOW%NLAYER
  ZWSNOW(:) = ZWSNOW(:) + PEK%TSNOW%WSNOW(:,JL) 
END DO
!
!* new total snow content
!
ZWSNOW_PERM(:) = MAX(ZWSNOW_PERM(:),ZWSNOW(:))
!
!* new total snow depth
!
ALLOCATE(ZD(SIZE(PEK%XTG,1)))
ZD(:) = 0.
DO JL=1,PEK%TSNOW%NLAYER
  ZD(:) = ZD(:) + PEK%TSNOW%WSNOW(:,JL)/PEK%TSNOW%RHO(:,JL)
END DO
ZD(:) = ZD(:) + (ZWSNOW_PERM(:)-ZWSNOW(:))/ZRHOSMAX
!
!* modified snow content profile
!
SELECT CASE(PEK%TSNOW%SCHEME)
  CASE('D95','1-L','EBA')
    GWORK(:,1)=.FALSE.
    IF(IO%LGLACIER)THEN
       WHERE(ZWSNOW(:)>=0..AND.PEK%TSNOW%WSNOW(:,1)/=XUNDEF)GWORK(:,1)=.TRUE.
    ELSE
       WHERE(ZWSNOW(:)==0..AND.PEK%TSNOW%WSNOW(:,1)/=XUNDEF)GWORK(:,1)=.TRUE.
    ENDIF
    WHERE(GWORK(:,1))
      PEK%TSNOW%WSNOW(:,1) = ZWSNOW_PERM(:)
    END WHERE
  CASE('3-L','CRO')
    !* grid
    ALLOCATE(ZDEPTH(SIZE(PEK%XTG,1),PEK%TSNOW%NLAYER))
    CALL SNOW3LGRID(ZDEPTH,ZD)
    DO JL=1,PEK%TSNOW%NLAYER
      WHERE(ZWSNOW(:)>= 0. .AND. PEK%TSNOW%WSNOW(:,JL)/=XUNDEF)
        PEK%TSNOW%WSNOW(:,JL) = ZDEPTH(:,JL) * PEK%TSNOW%RHO(:,JL)
      END WHERE
   END DO
   DEALLOCATE(ZDEPTH)

END SELECT
!
DEALLOCATE(ZD)
!-------------------------------------------------------------------------------------
!
!*       4.    Temperature of new snow
!              -----------------------
!
ALLOCATE(ZT(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)))
!       
SELECT CASE(PEK%TSNOW%SCHEME)
  CASE('1-L')
    ZT(:,:) = PEK%TSNOW%T (:,:)
  CASE('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(PEK%TSNOW%HEAT,PEK%TSNOW%RHO,ZT)
END SELECT
!
!* new snow is set to deep ground temperature
!
DO JL=1,PEK%TSNOW%NLAYER
!
  GWORK(:,JL)=.FALSE.
!
  IF(IO%LGLACIER)THEN
      WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JL)=.TRUE.
  ELSE
      WHERE(ZWSNOW_PERM(:)>0. .AND. ZWSNOW(:)==0)GWORK(:,JL)=.TRUE.
  ENDIF
!  
  WHERE(GWORK(:,JL))
      ZT(:,JL) = MIN(PEK%XTG(:,SIZE(PEK%XTG,2)),XTT)
  END WHERE
!
END DO
!
!
SELECT CASE(PEK%TSNOW%SCHEME)
  CASE('1-L')
    PEK%TSNOW%T (:,:) = ZT(:,:)
  CASE('3-L','CRO')
    CALL SNOW_T_WLIQ_TO_HEAT(PEK%TSNOW%HEAT,PEK%TSNOW%RHO,ZT)
END SELECT
!
DEALLOCATE(ZT   )
DEALLOCATE(GWORK)
!
!
!-------------------------------------------------------------------------------------
!
!*       5.    Soil ice initialization for LGLACIER
!              -----------------------
!
ALLOCATE(ZWAT(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)))
!
IF(IO%LGLACIER)THEN
!
  IF (IO%CISBA == 'DIF') THEN
      IWORK=IO%NGROUND_LAYER
      ZWAT(:,:)=KK%XWFC(:,:)
  ELSE
      IWORK=2
      ZWAT(:,:)=KK%XWSAT(:,:)
  ENDIF
!
  DO JL=1,IWORK
     WHERE(PK%XVEGTYPE_PATCH(:,NVT_SNOW)>0.0)
       PEK%XWGI(:,JL) = MAX(PEK%XWGI(:,JL),ZWAT(:,JL)*ZPSN(:))
       PEK%XWG (:,JL) = MIN(PEK%XWG (:,JL), MAX(KK%XWSAT(:,JL)-PEK%XWGI(:,JL),XWGMIN))
     END WHERE
     WHERE(PEK%XWG(:,JL) /= XUNDEF .AND. (PEK%XWG(:,JL) + PEK%XWGI(:,JL)) > KK%XWSAT(:,JL) )
       PEK%XWGI(:,JL) = KK%XWSAT(:,JL)-PEK%XWG (:,JL) !WGT<=WSAT
     END WHERE
  ENDDO
!
ENDIF
!
DEALLOCATE(ZWAT)
DEALLOCATE(ZPSN)
!
!-------------------------------------------------------------------------------------
!
!*       6.    Masking where there is no snow
!              ------------------------------
!
 CALL MKFLAG_SNOW(PEK%TSNOW)
!
IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_PERM_SNOW
