!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOIL_TEMP_ARP(PTSTEP,PA,PB,PC,PGAMMAT,PTDEEP,PSODELX,PTG)
!     ############################################################
!
!!****  *SOIL_TEMP_ARP*  
!
!!    PURPOSE
!!    -------
!     This subroutine solves the ARPEGE 1-d surface and deep force-restore 
!     'PTG' using the backward-difference scheme (implicit) as in soil_heatdiff. 
!     The eqs are solved rapidly by taking advantage of the
!     fact that the matrix is tridiagonal. Soln to the eqs:
!
!                   dTi       S1   
!                   --- =  Ct --  (Gi - Gi+1)
!                   dt        Si
!
!     with        |  G1 = Rn-H-LE
!                 |
!                 |         2Pi         1
!                 |  Gi = ------  -------------- (Ti-1 - Ti) 
!                 |       Ct*Day  S1 (Si-1 + Si)
!                 
!
!     where Si = pulsation depth, i=1 is the surface
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_SURF_PAR
!!    USE MODI_TRIDIAG_GROUND
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/01/09   B. Decharme
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XPI, XDAY
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_TRIDIAG_GROUND
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
REAL, INTENT(IN)                   :: PTSTEP ! Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)     :: PA,PB,PC ! terms for the linearization of Ts(t)
!
REAL, DIMENSION(:), INTENT(IN)     :: PTDEEP, PGAMMAT
!                                     PTDEEP   = Deep soil temperature (prescribed)
!                                                which models heating/cooling from
!                                                below the diurnal wave penetration
!                                               (surface temperature) depth.
!                                      PGAMMAT  = Deep soil heat transfer coefficient:
!                                                assuming homogeneous soil so that
!                                                this can be prescribed in units of 
!                                                (1/days): associated time scale with
!                                                PTDEEP.
REAL, DIMENSION(:), INTENT (IN)     ::  PSODELX   ! Pulsation for each layer (Only used if LTEMP_ARP=True)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PTG
!                                      PTG    = soil temperature (K)
!
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ ! Loop counter
!
INTEGER                             :: INLVLD ! Number of points and grid layers
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2))  :: ZTGM, ZFRCV, ZAMTRX, ZBMTRX,     &
                                             ZCMTRX, ZLAMBDA, ZALPHA  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Initialize local variables:
!
IF (LHOOK) CALL DR_HOOK('SOIL_TEMP_ARP',0,ZHOOK_HANDLE)
INLVLD = SIZE(PTG(:,:),2)
!
ZTGM(:,:)      = PTG(:,:)
!
ZFRCV  (:,:)   = 0.0
ZAMTRX (:,:)   = 0.0
ZBMTRX (:,:)   = 0.0
ZCMTRX (:,:)   = 0.0
ZALPHA (:,:)   = 0.0
ZLAMBDA(:,:)   = 0.0
!
!-------------------------------------------------------------------------------
!
! Calculate tri-diagonal matrix coefficients:
!
!
DO JJ=1,INLVLD-1
   ZALPHA (:,JJ) = PSODELX(JJ)*XDAY/(PTSTEP*PSODELX(1)*2.0*XPI)
   ZLAMBDA(:,JJ) = 1.0/(PSODELX(1)*(PSODELX(JJ)+PSODELX(JJ+1)))
ENDDO
ZALPHA(:,INLVLD) = PSODELX(INLVLD)*XDAY/(PTSTEP*PSODELX(1)*2.0*XPI)
!
!-------------------------------------------------------------------------------
!
! Upper BC
!
ZAMTRX(:,1) =  0.0
ZBMTRX(:,1) =  PA(:)+(2.0*XPI/XDAY)*(1.0/(PSODELX(1)*(PSODELX(1)+PSODELX(2)))-1.0)
ZCMTRX(:,1) = -2.0*XPI/(XDAY*PSODELX(1)*(PSODELX(1)+PSODELX(2)))
ZFRCV (:,1) =  PB(:)*ZTGM(:,1)+PC(:)-2.0*XPI*ZTGM(:,2)/XDAY
!
!
! Interior Grid
!
DO JJ=2,INLVLD-1
   ZAMTRX(:,JJ) = -ZLAMBDA(:,JJ-1) 
   ZBMTRX(:,JJ) =  ZALPHA(:,JJ) + ZLAMBDA(:,JJ-1) + ZLAMBDA(:,JJ)
   ZCMTRX(:,JJ) = -ZLAMBDA(:,JJ)
   ZFRCV(:,JJ)  =  ZALPHA(:,JJ)*ZTGM(:,JJ) 
ENDDO
!
! Lower BC: 2 currently accounted for, Either zero flux
! or a fixed temperature 'TDEEP' 
!
ZAMTRX(:,INLVLD) = -ZLAMBDA(:,INLVLD-1) 
ZCMTRX(:,INLVLD) =  0.0                
!
WHERE(PTDEEP(:) /= XUNDEF .AND. PGAMMAT(:) /= XUNDEF)
   ZBMTRX(:,INLVLD) =  ZALPHA(:,INLVLD) + ZLAMBDA(:,INLVLD-1) + PTSTEP*PGAMMAT(:)/XDAY
   ZFRCV (:,INLVLD) =  ZALPHA(:,INLVLD)*ZTGM(:,INLVLD) + PTSTEP*PGAMMAT(:)*PTDEEP(:)/XDAY
ELSEWHERE
   ZBMTRX(:,INLVLD) =  ZALPHA(:,INLVLD) + ZLAMBDA(:,INLVLD-1) 
   ZFRCV (:,INLVLD) =  ZALPHA(:,INLVLD)*ZTGM(:,INLVLD) 
END WHERE
!
!-------------------------------------------------------------------------------
!
! Compute ZTGM (solution vector) 
! used for systems of equations involving tridiagonal 
! matricies.
!
 CALL TRIDIAG_GROUND(ZAMTRX,ZBMTRX,ZCMTRX,ZFRCV,ZTGM)
!
! Update values in time:
!
PTG(:,:) = ZTGM(:,:)
!
IF (LHOOK) CALL DR_HOOK('SOIL_TEMP_ARP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOIL_TEMP_ARP
