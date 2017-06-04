!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOIL_HEATDIF(PTSTEP,PDZG,PDZDIF,PSOILCONDZ,   &
                              PSOILHCAPZ,PCT,PTERM1,PTERM2,    &
                              PTDEEP_A,PTDEEP_B,PTG,PDEEP_FLUX,&
                              PFLUX_COR                        )  
!     ############################################################################
!
!!****  *SOIL_HEATDIF*  
!
!!    PURPOSE
!!    -------
!     This subroutine solves the 1-d diffusion of 'PTG' using a
!     layer averaged set of equations which are time differenced
!     using the backward-difference scheme (implicit). 
!     The eqs are solved rapidly by taking advantage of the
!     fact that the matrix is tridiagonal. This is a very
!     general routine and can be used for the 1-d diffusion of any 
!     quantity as long as the diffusity is not a function of the
!     quantity being diffused. Aaron Boone 8-98. Soln to the eq:
!
!                    dQ    d    dQ       
!                 c  -- =  -- k -- 
!                    dt    dx   dx    
!
!     where k = k(x) (thermal conductivity), c = c(x) (heat capacity)
!     Diffusivity is k/c
!
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
!!    USE MODD_PARAMETERS
!!    USE MODI_TRIDIAG_GROUND
!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone (2000)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!      A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/02/00   Boone
!!      Modif       08/2011 B. Decharme : Optimization
!!      Modif       10/2014 B. Decharme : Use harmonic mean to compute 
!!                                        interfacial thermal conductivities
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
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
REAL, INTENT(IN)                    :: PTSTEP ! Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PCT, PTERM1, PTERM2, PTDEEP_A, PTDEEP_B
!                                      PCT    = thermal inertia [(m2 K)/J]
!                                      PTERM1 = coefficient of linearization
!                                               of surface energy budget 
!                                      PTERM2 = coefficient of linearization
!                                               of surface energy budget 
!                                      PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
!                                      PTDEEP_B = Deep soil temperature (prescribed)
!                                               which models heating/cooling from
!                                               below the diurnal wave penetration
!                                               (surface temperature) depth. If it
!                                               is FLAGGED as undefined, then the zero
!                                               flux lower BC is applied.
!                                      Tdeep = PTDEEP_B + PTDEEP_A * PDEEP_FLUX
!                                              (with PDEEP_FLUX in W/m2)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSOILCONDZ, PSOILHCAPZ, PFLUX_COR
!                                      PSOILCONDZ = soil thermal conductivity [W/(K m)]
!                                      PSOILHCAPZ = soil heat capacity [J/(m3 K)]
!                                      PFLUX_COR  = correction flux to close energy budget (W/m2)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PDZDIF, PDZG
!                                      PDZDIF = distance between consecuative layer mid-points
!                                      PDZG   = soil layers thicknesses
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PTG
!                                      PTG    = soil temperature (K)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PDEEP_FLUX ! Heat flux at bottom of ISBA (W/m2)
!
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: JJ, JL
!
INTEGER                                  :: INI, INLVLD ! Number of point and grid layers
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2)) :: ZTGM, ZDTERM, ZCTERM,   &
                                                    ZFRCV, ZAMTRX, ZBMTRX,     &
                                                    ZCMTRX  
!
REAL :: ZWORK1, ZWORK2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Initialize local variables:
!
IF (LHOOK) CALL DR_HOOK('SOIL_HEATDIF',0,ZHOOK_HANDLE)
ZDTERM(:,:)    = 0.0
ZCTERM(:,:)    = 0.0
ZFRCV(:,:)     = 0.0
ZAMTRX(:,:)    = 0.0
ZBMTRX(:,:)    = 0.0
ZCMTRX(:,:)    = 0.0
ZTGM(:,:)   = PTG(:,:)
!
INI    = SIZE(PTG(:,:),1)
INLVLD = SIZE(PTG(:,:),2)
!
!-------------------------------------------------------------------------------
!
! Calculate tri-diagonal matrix coefficients:
!
DO JL=1,INLVLD
  DO JJ=1,INI
     IF(JL<INLVLD)THEN
       ZWORK1 = PDZG(JJ,JL  )/(2.0*PDZDIF(JJ,JL)*PSOILCONDZ(JJ,JL  ))
       ZWORK2 = PDZG(JJ,JL+1)/(2.0*PDZDIF(JJ,JL)*PSOILCONDZ(JJ,JL+1))
     ELSE
       ZWORK1 = PDZG(JJ,JL)/(2.0*PDZDIF(JJ,JL)*PSOILCONDZ(JJ,JL))
       ZWORK2 = 0.0
     ENDIF
     ZDTERM(JJ,JL)=1.0/(PDZDIF(JJ,JL)*(ZWORK1+ZWORK2))
     ZCTERM(JJ,JL)= PSOILHCAPZ(JJ,JL)*PDZG(JJ,JL)/PTSTEP
  ENDDO
ENDDO 
!
! - - -------------------------------------------------
!
! Upper BC
!
ZAMTRX(:,1) =  0.0
ZBMTRX(:,1) =  1./(PCT(:)*PTSTEP)
ZCMTRX(:,1) = -PTERM2(:)*ZBMTRX(:,1)
ZFRCV (:,1) =  PTERM1(:)*ZBMTRX(:,1)
!
! Interior Grid
!
DO JL=2,INLVLD-1
   DO JJ=1,INI
   ZAMTRX(JJ,JL) = -ZDTERM(JJ,JL-1) 
   ZBMTRX(JJ,JL) =  ZCTERM(JJ,JL) + ZDTERM(JJ,JL-1) + ZDTERM(JJ,JL)
   ZCMTRX(JJ,JL) = -ZDTERM(JJ,JL)
   ZFRCV (JJ,JL) =  ZCTERM(JJ,JL)*ZTGM(JJ,JL)+PFLUX_COR(JJ,JL)
   ENDDO
ENDDO
!
! Lower BC: 2 currently accounted for, Either zero flux
! or a fixed temperature 'TDEEP' 
!
ZAMTRX(:,INLVLD) = -ZDTERM(:,INLVLD-1) 
ZCMTRX(:,INLVLD) =  0.0                
!

!ZDEEP_FLUX=ZDTERM(:,INLVLD)*(ZTGM(:,INLVLD)-ZTDEEP(:))
!ZTDEEP=PTDEEP_A*ZDEEP_FLUX+PTDEEP_B
!
!ZDEEP_FLUX=ZDTERM(:,INLVLD)*(ZTGM(:,INLVLD)-PTDEEP_A*ZDEEP_FLUX-PTDEEP_B)
!ZDEEP_FLUX=ZDTERM(:,INLVLD)*(ZTGM(:,INLVLD)-PTDEEP_B)/(1.+ZDTERM(:,INLVLD)*PTDEEP_A)
!
!
WHERE(PTDEEP_B(:) /= XUNDEF)
!   ZBMTRX(:,INLVLD) =  ZCTERM(:,INLVLD) + ZDTERM(:,INLVLD-1) + ZDTERM(:,INLVLD)
!   ZFRCV(:,INLVLD)  =  ZCTERM(:,INLVLD)*ZTGM(:,INLVLD) + ZDTERM(:,INLVLD)*PTDEEP(:)
   ZBMTRX(:,INLVLD) =  ZCTERM(:,INLVLD) + ZDTERM(:,INLVLD-1) &
                     + ZDTERM(:,INLVLD)/(1.+ZDTERM(:,INLVLD)*PTDEEP_A)
   ZFRCV(:,INLVLD)  =  ZCTERM(:,INLVLD)*ZTGM(:,INLVLD) &
                     + ZDTERM(:,INLVLD)*PTDEEP_B(:)/(1.+ZDTERM(:,INLVLD)*PTDEEP_A) &
                     + PFLUX_COR(:,INLVLD)
ELSEWHERE
   ZBMTRX(:,INLVLD) =  ZCTERM(:,INLVLD) + ZDTERM(:,INLVLD-1) 
   ZFRCV(:,INLVLD)  =  ZCTERM(:,INLVLD)*ZTGM(:,INLVLD)+PFLUX_COR(:,INLVLD)
END WHERE
!
! - - -------------------------------------------------
!
! Compute ZTGM (solution vector) 
! used for systems of equations involving tridiagonal 
! matricies.
!
 CALL TRIDIAG_GROUND(ZAMTRX,ZBMTRX,ZCMTRX,ZFRCV,ZTGM)
!
!
! Update values in time:
!
PTG(:,:) = ZTGM(:,:)
!
!* Deep soil Flux
!
PDEEP_FLUX(:) = 0.
WHERE(PTDEEP_B(:) /= XUNDEF)
  PDEEP_FLUX=ZDTERM(:,INLVLD)*(ZTGM(:,INLVLD)-PTDEEP_B)/(1.+ZDTERM(:,INLVLD)*PTDEEP_A)
END WHERE
!

IF (LHOOK) CALL DR_HOOK('SOIL_HEATDIF',1,ZHOOK_HANDLE)
!
!
!
END SUBROUTINE SOIL_HEATDIF
