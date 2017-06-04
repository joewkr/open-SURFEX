!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE CH_AER_DEP (PSVT, PFSVT,  PUSTAR, &
                      PRESA, PTA, PRHODREF)  
!###########################################################
  !
  !!                   
  !!                       
  !!
  !!    PURPOSE
  !!    -------
  !!      
  !!    Compute dry deposition velocity for aerosol species 
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
  USE MODE_AER_SURF
  USE MODI_CH_AER_VELGRAV1D
  USE MODD_CHS_AEROSOL
  !
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
  !
  !
  !*       0.2   Declarations of local variables :
  !
  REAL , DIMENSION(SIZE(PSVT,1), JPIN) :: ZRD ! surface  resistance
  REAL , DIMENSION(SIZE(PSVT,1), JPIN) :: ZVD  
  REAL , DIMENSION(SIZE(PSVT,1), JPIN) :: Stn ! Stockes number
  REAL , DIMENSION(SIZE(PSVT,1), JPIN) :: Sc
  REAL , DIMENSION(SIZE(PSVT,1))      :: WCn
  REAL , DIMENSION(SIZE(PSVT,1))      :: ZUSTAR, ZRESA
  REAL , DIMENSION(SIZE(PSVT,1), JPIN):: ZWORK
  REAL , DIMENSION(SIZE(PSVT,1),NSP+NCARB+NSOA,JPMODE):: ZCTOTA, ZCCTOT
  REAL, DIMENSION(SIZE(PSVT,1),JPMODE):: ZRHOP
  REAL, DIMENSION(SIZE(PSVT,1)) :: ZNU
  REAL, DIMENSION(SIZE(PSVT,1),JPIN)     :: Dg,zvs,zvsg, zdsg
  REAL, DIMENSION(SIZE(PSVT,1))        :: ZMU
  REAL, DIMENSION(SIZE(PSVT,1),JPIN)   :: ZVGK, ZDPK
  REAL, DIMENSION(SIZE(PSVT,1),JPMODE) :: ZSIG, ZRG, ZN
  REAL, DIMENSION(SIZE(PSVT,1),JPMODE) :: ZVG, ZDG
  REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2)) :: ZSVT
  REAL, DIMENSION(SIZE(PSVT,1))        :: ZSUM
  REAL, DIMENSION(NSP+NCARB+NSOA)      :: ZRHOI
  INTEGER :: JT, JJ, JSV, JN
  INTEGER :: M6I, M6J
  REAL :: ZDEN2MOL, ZG, ZTMP1, ZTMP2, ZTMP3, ZTMP4
  REAL(KIND=JPRB) :: ZHOOK_HANDLE


  !
  !============================================================================
  !
  !            Primilary
  !            ---------
  !Default values
  !--------------
! Cf Ackermann (all to black carbon except water)
IF (LHOOK) CALL DR_HOOK('CH_AER_DEP',0,ZHOOK_HANDLE)
!
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water

ZDEN2MOL = 1E-6 * XAVOGADRO / XMD
ZG       = 9.80665
ZMU(:) = 0.
ZVGK(:,:) = 0.
ZVG (:,:) = 0.
ZDPK(:,:) = 0.
ZUSTAR(:) = MAX(PUSTAR(:), 1.E-20)
ZRESA(:) = MIN(MAX(PRESA(:),   1.E-20), 9999.)
! molec./m3 to part/part
DO JSV=1,SIZE(PSVT,2)
ZSVT(:,JSV) = PSVT(:,JSV) * XMD / (XAVOGADRO * PRHODREF(:))
ENDDO
ZSVT(:,:) = MAX(ZSVT(:,:),XSURF_TINY)
 CALL PPP2AERO_SURF(ZSVT, PRHODREF, PSIG1D=ZSIG, PRG1D=ZRG, PN1D=ZN, PCTOTA=ZCTOTA)
ZRHOP(:,:) = 0.
DO JN=1,JPMODE
  ZSUM(:)=0.
  DO JJ=1,NSP+NCARB+NSOA
   ZSUM(:)=ZSUM(:)+ZCTOTA(:,JJ,JN)/ZRHOI(JJ)
  END DO

  DO JJ=1,NSP+NCARB+NSOA
   ZCCTOT(:,JJ,JN) = ZCTOTA(:,JJ,JN)/ZRHOI(JJ)/ZSUM(:)
   ZRHOP(:,JN)=ZRHOP(:,JN)+ZCCTOT(:,JJ,JN)*ZRHOI(JJ)
  ENDDO
ENDDO
 CALL CH_AER_VELGRAV1D(ZSIG, ZRG, PTA, PRHODREF, ZRHOP, ZMU, ZVGK,ZDPK, ZVG, ZDG)
Dg(:,:)  = MAX(ZDPK(:,:),1.E-40)
zvs(:,:) = MAX(ZVGK(:,:),1.E-20)
ZNU(:)   = ZMU(:)/PRHODREF(:)
DO JN=1,JPMODE
DO JJ= 0,2
zvsg(:,3*JN+JJ-2) =  MAX(ZVG(:,JN),1.E-20)
zdsg(:,3*JN+JJ-2) =  MAX(ZDG(:,JN),1.E-40)
END DO
END DO
!     compute Schmidt number
!     ----------------------
  DO  JN=1,JPIN
      !Sc(:,JN)= ZNU(:)/Dg(:,JN)
      Sc(:,JN)= ZNU(:)/zdsg(:,JN)
  END DO
!Scale for convective velocity 
! WCn(:) = MAX((PTKE(:) - 4.65* ZUSTAR(:)**2)/0.3, 1.E-20)


        ! verifier l'echelle convective sinon laisser la formulation de seinfeld
!    WHERE (WCn(:,:) /= XUNDEF)
!     WCn(:,:) = SQRT(WCn(:,:))
!    ELSEWHERE
     WCn(:) = 0.
!    END WHERE
!
!
Stn(:,:) =0.
ZVD(:,:) = 0.
ZWORK(:,:) = 0.
DO JT=1,SIZE(PSVT,1)
  IF (ZUSTAR(JT).GE.1.E-10) THEN
    DO JN=1,JPIN
      ZTMP1=0.
      ZTMP2=0.
      ZTMP3=0.
      ZTMP4=0.  
      Stn(JT,JN)= zvsg(JT,JN)*ZUSTAR(JT)**2/(ZG*ZNU(JT))
      ZTMP1=Sc(JT,JN)**(-2./3.)
      ZTMP2=(-3./Stn(JT,JN))
      IF (ZTMP2.gt.-10) then
        ZTMP3=10.**(ZTMP2)
      ELSE
        ZTMP3=0.
      ENDIF
      ZTMP4=ZTMP1+ZTMP3
     
     !ZRD(:,JN) = (Sc(:,JN)**(-2./3.)+ 10**(-3./Stn(:,JN)))&
     !      * (1 + 0.24 * WCn(:)**2 /ZUSTAR(:)**2) &
     !      * ZUSTAR(:)  
     !ZRD(:,JN) = ZUSTAR(:) * (Sc(:,JN)**(-2./3.)+ &
     !                           10**(-3./Stn(:,JN)))   
     ZRD(JT,JN) = ZUSTAR(JT) * ZTMP4                                
     ZRD(JT,JN) = MAX(ZRD(JT,JN),1.E-10) 
     ZRD(JT,JN) = 1. / ZRD(JT,JN) 
     ZWORK(JT,JN)= ZRESA(JT) + ZRD(JT,JN) + ZRESA(JT)*ZRD(JT,JN)*zvs(JT,JN)  
     ZWORK(JT,JN)= MAX(ZWORK(JT,JN), 1.E-10)
     ZWORK(JT,JN)= zvs(JT,JN) + 1./ ZWORK(JT,JN)
        !         deposition velocity for each cover type
        !         ----------------------------------------
     ZVD(JT,JN) = ZVD(JT,JN) + ZWORK(JT,JN)
  END DO
 ELSE
   ZVD(JT,:) = 0.
 END IF
ENDDO  

M6I=0
M6J=0
IF (LVARSIGI) M6I=1
IF (LVARSIGJ) M6J=1
DO JSV=1,SIZE(PSVT,2)-1-(JPMODE+M6I+M6J),2 ! mass deposition for I mode
  PFSVT(:,JSV) =  PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,2)
ENDDO
DO JSV=2,SIZE(PSVT,2)-(JPMODE+M6I+M6J),2   ! mass deposition for J mode
  PFSVT(:,JSV) =  PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,5)
ENDDO
! number particles deposition I
JSV = SIZE(PSVT,2)-(1+M6I+M6J)
PFSVT(:,JSV) = PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,1)
! number particles deposition J
JSV = SIZE(PSVT,2)-(M6I+M6J)
PFSVT(:,JSV) = PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,4)
! m6 deposition I
JSV = SIZE(PSVT,2)-M6J
IF (LVARSIGI) PFSVT(:,JSV) = PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,3)
! m6 deposition J
JSV = SIZE(PSVT,2)
IF (LVARSIGJ) PFSVT(:,JSV) = PFSVT(:,JSV) - PSVT(:,JSV)  * ZVD(:,6)
IF (LHOOK) CALL DR_HOOK('CH_AER_DEP',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------
!
END SUBROUTINE CH_AER_DEP
