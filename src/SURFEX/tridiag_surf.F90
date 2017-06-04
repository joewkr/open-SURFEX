!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE TRIDIAG_SURF(PVARM,PF,PDFDDTDZ,PEXT,PDEXTDV,PTSTEP,  &
                                   PDZZ,PDZM,PVARP,OIMPL,PALFA,PBETA     )  
!      #################################################
!
!
!!****   *TRIDIAG_SURF* - routine to solve a time implicit scheme
!!
!!
!!     PURPOSE
!!     -------
!        The purpose of this routine is to give a field PVARP at t+1, by 
!      solving an implicit TRIDIAGonal system obtained by the 
!      discretization of the vertical turbulent diffusion. It should be noted 
!      that the degree of implicitness can be varied (ZIMPL parameter) and that
!      the function of F(dT/dz) must have been linearized.
!      PVARP is localized at a mass point.
!
!!**   METHOD
!!     ------
!!
!!        [T(+) - T(-)]/Dt = -d{ F + dF/d(dT/dz) * impl * [dT/dz(+) - dT/dz(-)] }/dz
!!                           + Ext + dExt/dT * impl [ T(+) - T(-)]
!!
!!     It is discretized as follows:
!!
!!    PDZM(k)*PVARP(k)/PTSTEP
!!              = 
!!    PDZM(k)*PVARM(k)/PTSTEP 
!!  - (PDZM(k+1)+PDZM(k)  )/2. * PF(k+1)/PDZZ(k+1)
!!  + (PDZM(k)  +PDZM(k-1))/2. * PF(k)  /PDZZ(k)
!!  + (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARM(k+1)/PDZZ(k+1)**2
!!  - (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARP(k+1)/PDZZ(k+1)**2
!!  - (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARM(k)  /PDZZ(k+1)**2
!!  + (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARP(k)  /PDZZ(k+1)**2
!!  - (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARM(k)  /PDZZ(k)**2
!!  + (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARP(k)  /PDZZ(k)**2
!!  + (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARM(k-1)/PDZZ(k)**2
!!  - (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARP(k-1)/PDZZ(k)**2
!!  + PDZM(k) * PEXT(k)
!!  + PDZM(k) * ZIMPL* PDEXTDV(k) * PVARP(k)
!!  - PDZM(k) * ZIMPL* PDEXTDV(k) * PVARM(k)
!!  
!!
!!
!!    The system to solve is:
!!
!!      A*PVARP(k-1) + B*PVARP(k) + C*PVARP(k+1) = Y(k)
!!
!!
!!    The RHS of the linear system in PVARP writes:
!!
!! y(k)    = PDZM(k)*PVARM(k)/PTSTEP
!!  - (PDZM(k+1)+PDZM(k)  )/2. * PF(k+1)/PDZZ(k+1)
!!  + (PDZM(k)  +PDZM(k-1))/2. * PF(k)  /PDZZ(k)
!!  + (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARM(k+1)/PDZZ(k+1)**2
!!  - (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1) * PVARM(k)  /PDZZ(k+1)**2
!!  - (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARM(k)  /PDZZ(k)**2
!!  + (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)   * PVARM(k-1)/PDZZ(k)**2
!!  + PDZM(k) * PEXT(k) 
!!  - PDZM(k) * PDEXTDV(k) * PVARM(k)
!!
!!                      
!!        Then, the classical TRIDIAGonal algorithm is used to invert the 
!!     implicit operator. Its matrix is given by:
!!
!!     ( b(ikb)   c(ikb)      0        0        0         0        0        0  )
!!     (   0      a(ikb+1) b(ikb+1) c(ikb+1)    0  ...    0        0        0  ) 
!!     (   0         0     a(ikb+2) b(ikb+2) c(ikb+2).    0        0        0  ) 
!!      .......................................................................
!!     (   0   ...   0     a(k)     b(k)     c(k)         0   ...  0        0  ) 
!!      .......................................................................
!!     (   0         0        0        0        0 ...a(ike-1) b(ike-1) c(ike-1))
!!     (   0         0        0        0        0 ...     0   a(ike)   b(ike)  )
!!
!!     ikb and ike represent the first and the last inner mass levels of the
!!     model. The coefficients are:
!!         
!! a(k) = + (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)  /PDZZ(k)**2
!! b(k) =    PDZM(k) / PTSTEP
!!        - (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1)/PDZZ(k+1)**2
!!        - (PDZM(k)  +PDZM(k-1))/2. * ZIMPL* PDFDDTDZ(k)  /PDZZ(k)**2
!!        - PDZM(k) * PDEXTDV(k)
!! c(k) = + (PDZM(k+1)+PDZM(k)  )/2. * ZIMPL* PDFDDTDZ(k+1)/PDZZ(k+1)**2
!!
!!          for all k /= ikb or ike
!!
!!
!! b(ikb) =  PDZM(ikb) / PTSTEP
!!          -(PDZM(ikb+1)+PDZM(ikb))/2.*ZIMPL*PDFDDTDZ(ikb+1)/PDZZ(ikb+1)**2
!! c(ikb) = +(PDZM(ikb+1)+PDZM(ikb))/2.*ZIMPL*PDFDDTDZ(ikb+1)/PDZZ(ikb+1)**2
!!
!! b(ike) =  PDZM(ike) / PTSTEP
!!          -(PDZM(ike)+PDZM(ike-1))/2.*ZIMPL*PDFDDTDZ(ike)/PDZZ(ike)**2
!! a(ike) = +(PDZM(ike)+PDZM(ike-1))/2.*ZIMPL*PDFDDTDZ(ike)/PDZZ(ike)**2
!!
!!
!!     EXTERNAL
!!     --------
!!
!!       NONE
!!
!!     IMPLICIT ARGUMENTS
!!     ------------------
!!
!!     REFERENCE
!!     ---------
!!       Press et al: Numerical recipes (1986) Cambridge Univ. Press
!!
!!     AUTHOR
!!     ------
!!       V. Masson         * Meteo-France *   
!! 
!!     MODIFICATIONS
!!     -------------
!!       Original        04/2003 (from tridiag.f90)
!!                       09/2007 implicitation with surface flux at lowest level
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PVARM   ! variable at t-1      at mass point
REAL, DIMENSION(:,:), INTENT(IN) :: PF      ! flux in dV/dt=-dF/dz at flux point
REAL, DIMENSION(:,:), INTENT(IN) :: PDFDDTDZ! dF/d(dV/dz)          at flux point
REAL, DIMENSION(:,:), INTENT(IN) :: PEXT    ! External forces in dT/dt=Ext
!                                           ! (Except surf. flux)  at mass point
REAL, DIMENSION(:,:), INTENT(IN) :: PDEXTDV ! dExt/dV 
!                                           !                      at mass point
REAL,                 INTENT(IN) :: PTSTEP  ! time step
REAL, DIMENSION(:,:), INTENT(IN) :: PDZZ    ! Dz                   at flux point
REAL, DIMENSION(:,:), INTENT(IN) :: PDZM    ! Dz                   at mass point
!
REAL, DIMENSION(:,:), INTENT(OUT):: PVARP   ! variable at t+1      at mass point
LOGICAL, OPTIONAL,    INTENT(IN) :: OIMPL   ! true if implicit coupling
!                                           ! information between lvl 1 and
!                                           ! surface flux scheme must be
!                                           ! returned.
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PALFA  ! V+(1) = alfa F(1) + beta
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PBETA  ! V+(1) = alfa F(1) + beta
!
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))  :: ZVARP
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))  :: ZDZ_DFDDTDZ_O_DZ2
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))  :: ZA, ZB, ZC
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))  :: ZY ,ZGAM 
                                         ! RHS of the equation, 3D work array
REAL, DIMENSION(SIZE(PVARM,1))                :: ZBET
                                         ! 2D work array
INTEGER                              :: JK            ! loop counter
INTEGER                              :: IK            ! vertical limits
!
LOGICAL                              :: GIMPL
REAL                                 :: ZIMPL  ! implicitation coefficient
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                              ! for SBL scheme solving
! ---------------------------------------------------------------------------
!                                              
!*      1.  Preliminaries
!           -------------
!
IF (LHOOK) CALL DR_HOOK('TRIDIAG_SURF',0,ZHOOK_HANDLE)
ZIMPL = 1
!
IK = SIZE(PF,2)
!
ZDZ_DFDDTDZ_O_DZ2 = PDFDDTDZ/PDZZ
!
ZA=0.
ZB=0.
ZC=0.
ZY=0.
!
IF (PRESENT(OIMPL)) THEN
  GIMPL = OIMPL
ELSE
  GIMPL = .FALSE.
END IF
!
!*      2.  COMPUTE THE RIGHT HAND SIDE
!           ---------------------------
!
ZY(:,1) = PDZM(:,1)*PVARM(:,1)/PTSTEP              &
      - PF(:,2)        &
      + PF(:,1)        &
      - ZDZ_DFDDTDZ_O_DZ2(:,1) * ZIMPL * PVARM(:,1)&
      + ZDZ_DFDDTDZ_O_DZ2(:,2) * ZIMPL * PVARM(:,2)&
      - ZDZ_DFDDTDZ_O_DZ2(:,2) * ZIMPL * PVARM(:,1)&
      + PDZM(:,1)*PEXT(:,1)                        &
      - PDZM(:,1)*PDEXTDV(:,1) * ZIMPL * PVARM(:,1)  
!
DO JK=2,IK-1
  ZY(:,JK) = PDZM(:,JK)*PVARM(:,JK)/PTSTEP               &
      - PF(:,JK+1)          &
      + PF(:,JK  )          &
      + ZDZ_DFDDTDZ_O_DZ2(:,JK+1) * ZIMPL * PVARM(:,JK+1)  &
      - ZDZ_DFDDTDZ_O_DZ2(:,JK+1) * ZIMPL * PVARM(:,JK  )  &
      - ZDZ_DFDDTDZ_O_DZ2(:,JK  ) * ZIMPL * PVARM(:,JK  )  &
      + ZDZ_DFDDTDZ_O_DZ2(:,JK  ) * ZIMPL * PVARM(:,JK-1)  &
      + PDZM(:,JK)*PEXT(:,JK)                              &
      - PDZM(:,JK)*PDEXTDV(:,JK) * ZIMPL * PVARM(:,JK)  
END DO
!
!* upper level is fixed (atmospheric forcing)
!  turbulent flux divergence is supposed equal to ZERO (inertial sublayer).
!  other terms are kept
!
ZY(:,IK) = PDZM(:,IK)*PVARM(:,IK)/PTSTEP                 &
      + PDZM(:,IK)*PEXT(:,IK)                              &
      - PDZM(:,IK)*PDEXTDV(:,IK) * ZIMPL * PVARM(:,IK)  
!
!
!*       3.  INVERSION OF THE TRIDIAGONAL SYSTEM
!            -----------------------------------
!
!
!*       3.1 arrays A, B, C
!            --------------
!
  ZB(:,1) =   PDZM(:,1)/PTSTEP                   &
              - ZDZ_DFDDTDZ_O_DZ2(:,1) * ZIMPL     &
              - ZDZ_DFDDTDZ_O_DZ2(:,2) * ZIMPL     &
              - PDZM(:,1)*PDEXTDV(:,1)  
  ZC(:,1) =   ZDZ_DFDDTDZ_O_DZ2(:,2) * ZIMPL

  DO JK=2,IK-1
    ZA(:,JK) =   ZDZ_DFDDTDZ_O_DZ2(:,JK  ) * ZIMPL
    ZB(:,JK) =   PDZM(:,JK)/PTSTEP                   &
                   - ZDZ_DFDDTDZ_O_DZ2(:,JK+1) * ZIMPL &
                   - ZDZ_DFDDTDZ_O_DZ2(:,JK  ) * ZIMPL &
                   - PDZM(:,JK)*PDEXTDV(:,JK)  
    ZC(:,JK) =   ZDZ_DFDDTDZ_O_DZ2(:,JK+1) * ZIMPL
  END DO

  !* upper level is fixed (atmospheric forcing)
  !  turbulent flux divergence is supposed equal to ZERO (inertial sublayer).
  !  other terms are kept
  ZA(:,IK) =   0.
  ZB(:,IK) =   PDZM(:,IK)/PTSTEP                   &
                  - PDZM(:,IK)*PDEXTDV(:,IK)  
!
!*       3.2 going down
!            ----------
!
  ZBET(:) = ZB(:,IK)  ! bet = b(ik)
  ZVARP(:,IK) = ZY(:,IK) / ZBET(:)

  !
  DO JK = IK-1,2,-1
    ZGAM(:,JK) = ZA(:,JK+1) / ZBET(:)  
                                                    ! gam(k) = c(k-1) / bet
    ZBET(:)    = ZB(:,JK) - ZC(:,JK) * ZGAM(:,JK)
                                                    ! bet = b(k) - a(k)* gam(k)  
    ZVARP(:,JK)= ( ZY(:,JK) - ZC(:,JK) * ZVARP(:,JK+1) ) / ZBET(:)
                                        ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
  END DO 
  ! special treatment for the lowest level
  ZGAM(:,1) = ZA(:,2) / ZBET(:) 
                                                    ! gam(k) = c(k-1) / bet
  ZBET(:)     = ZB(:,1) - ZC(:,1) * ZGAM(:,1)
                                                    ! bet = b(k) - a(k)* gam(k)  
  ZVARP(:,1)= ( ZY(:,1) - ZC(:,1) * ZVARP(:,2) ) / ZBET(:)
                                       ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
!
!
!*       3.3 going up
!            --------
!
  DO JK = 2,IK
    ZVARP(:,JK) = ZVARP(:,JK) - ZGAM(:,JK-1) * ZVARP(:,JK-1)
  END DO

!
!
!*       3.4 updates prognostic variable
!            ---------------------------
!
IF (.NOT. GIMPL) THEN
  PVARP=ZVARP
ELSE
  PALFA=1./ZBET(:)
  PBETA=ZVARP(:,1)
  PVARP=PVARM
END IF
IF (LHOOK) CALL DR_HOOK('TRIDIAG_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIDIAG_SURF
