!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DSLT_DEP (PSVT, PFSVT, PUSTAR, PRESA, PTA, PRHODREF,             &
                     PEMISSIG, PEMISRADIUS, KPMODE, PDENSITY, PMOLARWEIGHT, &
                     PCONVERTFACM0, PCONVERTFACM6, PCONVERTFACM3,           &
                     OVARSIG, ORGFIX, HVERMOD                               )  
!###########################################################                    
!!
!!    PURPOSE
!!    -------
!!      
!!    Compute dry deposition velocity for dust species 
!!
!!    AUTHOR
!!    ------
!!      P.Tulet      * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      20/02/05
!!      J.Escobar     06/2013  for REAL4/8 add EPSILON management
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS, ONLY : XPI, XAVOGADRO, XG
!
USE MODE_DSLT_SURF
USE MODI_DSLT_VELGRAV1D
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODD_SURF_PAR , ONLY : XSURF_TINY
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:),   INTENT(IN)    :: PSVT       ! friction velocity
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PFSVT      ! flux
REAL, DIMENSION(:),     INTENT(IN)    :: PUSTAR       ! friction velocity
REAL, DIMENSION(:),     INTENT(IN)    :: PRESA        ! aerodynamical resistance
REAL, DIMENSION(:),     INTENT(IN)    :: PTA          ! ait temperature
REAL, DIMENSION(:),     INTENT(IN)    :: PRHODREF     ! air density
REAL, DIMENSION(:),     INTENT(IN)    :: PEMISSIG  
REAL, DIMENSION(:),     INTENT(IN)    :: PEMISRADIUS
INTEGER,                INTENT(IN)    :: KPMODE
REAL,                   INTENT(IN)    :: PDENSITY
REAL,                   INTENT(IN)    :: PMOLARWEIGHT
REAL,                   INTENT(OUT)   :: PCONVERTFACM0
REAL,                   INTENT(OUT)   :: PCONVERTFACM6
REAL,                   INTENT(OUT)   :: PCONVERTFACM3
LOGICAL,                INTENT(IN)    :: OVARSIG
LOGICAL,                INTENT(IN)    :: ORGFIX
 CHARACTER(LEN=6),       INTENT(IN)    :: HVERMOD
!
!*       0.2   Declarations of local variables :
!
REAL , DIMENSION(SIZE(PSVT,1), KPMODE) :: ZSIG, ZRG, ZVG, ZDG
REAL , DIMENSION(SIZE(PSVT,1), KPMODE) :: ZSTN ! Stockes number
REAL , DIMENSION(SIZE(PSVT,1), KPMODE) :: ZSC  ! Schmidt number
REAL , DIMENSION(SIZE(PSVT,1), KPMODE) :: ZRD  ! surface  resistance
REAL , DIMENSION(SIZE(PSVT,1), KPMODE*3) :: ZVGK, ZDPK
REAL , DIMENSION(SIZE(PSVT,1), KPMODE*3) :: ZVD  ! [m/s] dry deposition velocity 
REAL , DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2)) :: ZSVT
REAL , DIMENSION(SIZE(PSVT,1)) :: ZUSTAR, ZRESA
REAL , DIMENSION(SIZE(PSVT,1)) :: ZNU
REAL , DIMENSION(SIZE(PSVT,1)) :: ZMU
INTEGER,DIMENSION(KPMODE) :: NM0                 ! [idx] index for Mode 0 in passed variables
INTEGER,DIMENSION(KPMODE) :: NM3                 ! [idx] indexes for Mode 3 in passed variables
INTEGER,DIMENSION(KPMODE) :: NM6                 ! [idx] indexes for Mode 6 in passed variables
INTEGER :: JN, J0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!============================================================================
!
!            Primilary
!            ---------
!Default values
!--------------
! Cf Ackermann (all to black carbon except water)
IF (LHOOK) CALL DR_HOOK('DSLT_DEP',0,ZHOOK_HANDLE)
!
ZUSTAR(:) = MAX(PUSTAR(:), 1.E-20)
ZRESA (:) = MIN(MAX(PRESA(:), 1.E-20), 9999.)
! Save scalars in local array
ZSVT(:,:) = MAX(PSVT(:,:), XSURF_TINY)
!
ZMU(:)    = 0.
ZVGK(:,:) = 0.
ZVG (:,:) = 0.
ZDPK(:,:) = 0.
!
IF (OVARSIG) THEN
  DO JN=1,KPMODE
    NM0(JN) = 1+(JN-1)*3
    NM3(JN) = 2+(JN-1)*3
    NM6(JN) = 3+(JN-1)*3
  END DO
ELSE IF (ORGFIX) THEN
  DO JN=1,KPMODE
    NM3(JN) = JN
  END DO
ELSE
  DO JN=1,KPMODE
    NM0(JN) = 1+(JN-1)*2
    NM3(JN) = 2+(JN-1)*2
  END DO
END IF
!
PCONVERTFACM0 = PMOLARWEIGHT / XAVOGADRO
PCONVERTFACM6 = PCONVERTFACM0 * 1.d6
PCONVERTFACM3 = 4./3. * XPI * PDENSITY / 1.d18
!
 CALL DSLTMOMENT2SIZE(ZSVT, PRHODREF, PEMISRADIUS, PEMISSIG, NM0, NM3, NM6, &
                     PCONVERTFACM0, PCONVERTFACM6, PCONVERTFACM3,          &
                     OVARSIG, ORGFIX, PSIG1D=ZSIG, PRG1D=ZRG               )
!
 CALL DSLT_VELGRAV1D(ZSIG, ZRG, PTA, PRHODREF, PDENSITY, ZMU, ZVGK, ZDPK, ZVG, ZDG)
!
ZNU (:)   = ZMU(:)/PRHODREF(:)
!
ZVGK(:,:) = MAX(ZVGK(:,:),1.E-20)
ZDPK(:,:) = MAX(ZDPK(:,:),1.E-40)
!
ZVG (:,:) = MAX(ZVG (:,:),1.E-20)
ZDG (:,:) = MAX(ZDG (:,:),1.E-40)
!
ZSTN(:,:) =0.
!
DO  JN = 1,KPMODE        
  !
  !         deposition velocity for each cover type
  !         ----------------------------------------
  !Stoke's number, Seinfeld & Pandis, pp 965
  ZSTN(:,JN) = ZVG(:,JN)*ZUSTAR(:)**2/(XG*ZNU(:))
  ZSTN(:,JN) = MAX(ZSTN(:,JN), 0.05)
  !
  !     compute Schmidt number
  !     ----------------------
  ZSC(:,JN) = ZNU(:)/ZDG(:,JN)
  !
  !Get nominator of equation 19.18 Seinfeld & Pandis==> 1/rd
  ZRD(:,JN) = ZUSTAR(:) * (ZSC(:,JN)**(-2./3.)+ 10**(-3./ZSTN(:,JN)))
  !
  !Limit to reasonable values
  !ZRD(:,JN) = MAX(ZRD(:,JN),1.E-10)
  !Get rd
  ZRD(:,JN) = 1. / ZRD(:,JN)
  !
ENDDO
!
DO  JN = 1,KPMODE*3
  !
  J0 = (JN-1)/3+1
  !
  !         deposition velocity for each cover type
  !         ----------------------------------------
  ZVD(:,JN) = 0.
  !
  !Get ra + rd + ra*rd*vg which is equal to 
  !getting nominator of equation 19.7 Seinfeld & Pandis 
  ZVD(:,JN)= ZRESA(:) + ZRD(:,J0) + ZRESA(:)*ZRD(:,J0)*ZVGK(:,JN)
  !
  !Limit to reasonable values
  ZVD(:,JN)= MAX(ZVD(:,JN), 1.E-10)
  !
  !Get the total dry dep velocity (Seinfeld & Pandis, eqn 19.7)
  IF (HVERMOD=='CMDVER') THEN 
    ZVD(:,JN)= ZVGK(:,JN)         &  ! Gravitation term 
               + 1./ZVD(:,JN)        ! turbulence and surface resistance term
  ELSE    
!     ZVD(:,JN)=zvs(:,JN)  &  !Gravitation term 
!         + 1./ZVD(:,JN)     !turbulence and surface resistance term
      ! The gravitation term as been computed by MesoNH (see sedim_dust.f90)
    ZVD(:,JN) =  1./ZVD(:,JN) ! turbulence and surface resistance term
  END IF
  !
END DO

! Only M3 flux (mass) has been used ; flux for over moment has been made after
DO JN = 1,KPMODE
  PFSVT(:,NM3(JN)) =  PFSVT(:,NM3(JN)) - PSVT(:,NM3(JN))  * ZVD(:,2+(JN-1)*3)
ENDDO
!
IF (LHOOK) CALL DR_HOOK('DSLT_DEP',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------
!
END SUBROUTINE DSLT_DEP
