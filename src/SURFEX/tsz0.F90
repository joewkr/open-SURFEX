!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TSZ0 (DTZ, PTIME, PTSTEP, KK, PEK )
!     ################################################################
!
!
!!****  *TSZ0*  
!!
!!    PURPOSE
!!    -------
!       This subroutine computes the surface fluxes when  the soil temperature,
!     humidity and rugisty length are prescribed. It uses these values and the 
!     atmospheric fields at the first level located at dz/2 to compute a
!     vertical gradient and a drag coefficient is computed according to a
!     stability index ( Richardson number )    
!     
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
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
!!	J. Stein           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     25/01/96 
!!                   25/03/96 spatialize the input TS, WG, SST fields
!!                   22/05/96 correct igrid value for the rain rate
!!                   27/11/96 set realistic values for z0 fields on sea
!!      V.Masson     09/07/97 add directional z0 computations and RESA correction
!!      V.Masson     15/03/99 some computations are now done in GROUND_PARAMn
!!      V.Masson     04/01/00 all computations are now done in ISBA
!!      P. Le Moigne 03/2015  tsz0 time management
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_PE_t
!
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
!
USE MODD_CSTS,       ONLY : XPI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!* general variables
!  -----------------
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
!
REAL,                   INTENT(IN)  :: PTIME      ! Current time
REAL,                   INTENT(IN)  :: PTSTEP     ! timestep of the integration
!
!
!*      0.2    declarations of local variables
!
!
!* local variables for Ts time interpolation
!   ----------------------------------------
!
REAL    :: ZA,ZTIMEP    ! weigths and instant for the temporal interpolation
INTEGER :: IHOURP       ! hourly data index for intant t +dt
INTEGER :: JPATCH       ! loop counter on patches
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! enter here the temporal variations of the soil fields increments
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! prescribed values of the surface temperature increment over land (K)
REAL :: ZDTS_HOUR 
! prescribed values of the soil humidity increment at every hour (fraction)
REAL :: ZDHUGRD_HOUR              
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     TEMPORAL INTERPOLATION OF THE SURFACE TEMPERATURES AT T+DT
!               ----------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TSZ0',0,ZHOOK_HANDLE)
!
IF (DTZ%NTIME==25) THEN
   ZTIMEP = MOD(PTIME+PTSTEP,86400.)  ! recover the time from O HTU
ELSE
   ZTIMEP = PTIME+PTSTEP              ! accumulated time since beginning of run
ENDIF
!
IHOURP = INT(ZTIMEP/3600.)+1
!
IF (DTZ%NTIME==1) THEN
  ZDTS_HOUR    = DTZ%XDATA_DTS   (1)
  ZDHUGRD_HOUR = DTZ%XDATA_DHUGRD(1)        
ELSE
  ZDTS_HOUR    = DTZ%XDATA_DTS   (IHOURP)
  ZDHUGRD_HOUR = DTZ%XDATA_DHUGRD(IHOURP)
ENDIF
!
! temporal interpolation of the surface temperature increment  over land at time t
ZA= ZDTS_HOUR /3600. * PTSTEP
WHERE (PEK%XTG(:,:)/=XUNDEF)
  PEK%XTG(:,:)= PEK%XTG(:,:) + ZA
END WHERE
!
! temporal interpolation of the soil humidity increment at time t
ZA= ZDHUGRD_HOUR /3600.* PTSTEP
WHERE (PEK%XWG(:,:)/=XUNDEF)
  PEK%XWG(:,:)= ACOS( 1.                                                               &
      - 2.* MIN( 0.5 * (1. - COS( XPI * MIN(PEK%XWG(:,:) /KK%XWFC(:,:),1.)  )) + ZA , 1.) &
            ) / XPI * KK%XWFC(:,:)  
END WHERE
!
IF (LHOOK) CALL DR_HOOK('TSZ0',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TSZ0
