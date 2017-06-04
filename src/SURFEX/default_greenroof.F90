!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_GREENROOF( HRUNOFF_GR,HSCOND_GR,                   &
                                    HKSAT_GR,HHORT_GR                       )  
!     ########################################################################
!
!!****  *DEFAULT_GREENROOF* - routine to set default values for the configuration for GREENROOF
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!     Based on default_isba
!!
!!    EXTERNAL
!!    --------
!!
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
!!      C. de Munck & A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,           ONLY: XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=4),  INTENT(OUT) :: HRUNOFF_GR    ! surface runoff formulation ('WSAT','DT92','SGH ')    
 CHARACTER(LEN=4),  INTENT(OUT) :: HSCOND_GR     ! thermal conductivity ('DEF ','PL98')
 CHARACTER(LEN=3),  INTENT(OUT) :: HKSAT_GR      ! soil hydraulic profile option ('DEF','SGH')
 CHARACTER(LEN=3),  INTENT(OUT) :: HHORT_GR      ! Horton runoff ('DEF','SGH')
!                                           
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_GREENROOF',0,ZHOOK_HANDLE)
!
HRUNOFF_GR    = 'WSAT'
HSCOND_GR     = 'PL98'
HKSAT_GR      = 'DEF'
HHORT_GR      = 'DEF'
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_GREENROOF
