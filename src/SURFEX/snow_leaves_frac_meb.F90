!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################################################
SUBROUTINE SNOW_LEAVES_FRAC_MEB(PPSN, PPALPHAN,      &
                                          PWRVN,     &
                                          PTV,       &
                                          PCHIP,     &
                                          PLAIV,     &
                                          PWRVNMAX,  &
                                          PDELTAVN, PMELTVN )
!   ############################################################################
!
!!****  *SNOW_LEAVES_FRAC_MEB*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate desnity, maximum snow load etc for intercepted snow
!     
!!**  METHOD
!!    ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson           * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XTT
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)   :: PPSN, PPALPHAN
!                                     PPSN       =
!                                     PPALPHAN   = snow/canopy transition coefficient
!
REAL, DIMENSION(:), INTENT(IN)   :: PWRVN
!                                     PWRVN      = snow retained on the foliage
!
REAL, DIMENSION(:), INTENT(IN)   :: PLAIV
!                                     PLAIV      = canopy vegetation leaf area index
!
REAL, DIMENSION(:), INTENT(IN)   :: PCHIP, PTV
!                                     PCHIP      = view factor (for LW) 
!                                     PTV        = Canopy T (K)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PWRVNMAX
!                                     PWRVNMAX   = maximum equivalent snow content
!                                                  in the canopy vegetation
!
REAL, DIMENSION(:), INTENT(OUT)  :: PDELTAVN
!                                     PDELTAVN   = fraction of the canopy foliage covered
!                                                  by intercepted snow
!
REAL, DIMENSION(:), INTENT(OUT)  :: PMELTVN
!                                     PMELTVN    = freeze/melt rate (kg m-2 s-1)
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAIV)) :: ZLAI,ZFCP,ZFRACVN, ZSNOWRHOV
!                                ZLAI      = weigthed leaf area index
!                                ZFCP      = snow interception factor
!                                ZFRACVN   = fraction of interception snow
!                                ZSNOWRHOV = density of snow intercepted by the canopy (kg m-3)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
!  For intercepted snow density
!
REAL, PARAMETER             :: ZRHOVNPAR1    = 67.92      ! (kg/m3)
REAL, PARAMETER             :: ZRHOVNPAR2    = 51.25      ! (kg/m3)
REAL, PARAMETER             :: ZRHOVNPAR3    = 2.59       ! (K)
!
!  For intercepted maximum snow load
!
REAL, PARAMETER             :: ZWRVNMAXPAR1  = 6.3        ! (kg/m2)
REAL, PARAMETER             :: ZWRVNMAXPAR2  = 0.27       ! (-)
REAL, PARAMETER             :: ZWRVNMAXPAR3  = 46.        ! (kg/m3)

!  For intercepted snow evaporation efficiency
!
REAL, PARAMETER             :: ZDVNPAR1      = 0.89       ! (-)
REAL, PARAMETER             :: ZDVNPAR2      = -4.7       ! (-)
REAL, PARAMETER             :: ZDVNPAR3      = 0.45       ! (-)
REAL, PARAMETER             :: ZDVNPAR4      = 0.3        ! (-)
REAL, PARAMETER             :: ZMELTF        = 5.556E-6   ! Snow melt factor
REAL, PARAMETER             :: ZLAI_MIN      = 0.001      ! (m2 m-2) Below this (numerical) threshold, interception
                                                          ! by the canopy is not assumed to occur
                                                          ! as canopy essentially buried.
REAL, PARAMETER             :: ZRHOVN_TMAX   = 279.85403  ! (K) corresponds to a snow density of 
                                                          ! 750 kg m-3 (presumably the max).
                                                          ! Obtained by inverting the snow density Eq
                                                          ! for Tv below assuming a density of 750

!-------------------------------------------------------------------------------
!
!*      0.     Initialization
!               --------------
!
IF (LHOOK) CALL DR_HOOK('SNOW_LEAVES_FRAC_MEB',0,ZHOOK_HANDLE)
!
ZSNOWRHOV(:)= ZRHOVNPAR1
!
ZFRACVN(:)  = 0.0
ZFCP(:)     = 0.0
!
PDELTAVN(:) = 0.0
PMELTVN(:)  = 0.0
PWRVNMAX(:) = 0.0
!
!
ZLAI(:)     = PLAIV(:)*(1.-PPSN(:)+PPSN(:)*(1.-PPALPHAN(:)))
!
! If snow buries the vegetation canopy (i.e. ZLAI~=0), we do not need the following:
! 
WHERE(ZLAI(:) > ZLAI_MIN .AND. PLAIV(:)/=XUNDEF)
!
! Snow density
!
   ZSNOWRHOV(:)= ZRHOVNPAR1 + ZRHOVNPAR2*EXP( (MIN(ZRHOVN_TMAX,PTV(:))-XTT)/ZRHOVNPAR3)
!
! Intercepted maximum snow load
!
   PWRVNMAX(:) = ZWRVNMAXPAR1*(ZWRVNMAXPAR2+ZWRVNMAXPAR3/ZSNOWRHOV(:)) * ZLAI(:)
!
! Fraction of snow on vegetation canopy
!
   ZFRACVN(:)  = PWRVN(:)/PWRVNMAX(:)
!
! Snow evaporation efficiency coefficient which corresponds to
! delta for intercepted water
!
   PDELTAVN(:) = ZDVNPAR1*ZFRACVN(:)**ZDVNPAR4/( 1.+EXP( ZDVNPAR2*( ZFRACVN(:) - ZDVNPAR3 ) ) )
!
! Melt rate (kg/m2/s)
!
   PMELTVN(:)  = ZMELTF * ( PTV(:)-XTT ) * ZFRACVN(:)
!
END WHERE
!
IF (LHOOK) CALL DR_HOOK('SNOW_LEAVES_FRAC_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW_LEAVES_FRAC_MEB

