!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TEBGRID3( PSOILDEPTH, PD_G, PD_G1 )

!     ##########################################################################
!
!!****  *TEBGRID*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the soil grid configuration using a simple
!     geometric relation for all sub-surface layers.
!     This algorithm assumes the total soil depth > 0 m
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
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone (2000)
!!    Boone et al. (2000)
!!    Habets et al. (2003)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/04/03
!!      B. Decharme    12/10 uppermost soil layer set to 1cm
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL,  INTENT(IN)  :: PSOILDEPTH  ! total soil depth            (m)
!                                   
REAL, DIMENSION(:), INTENT(OUT) :: PD_G        ! depth of base of soil layers (m)
REAL, OPTIONAL,       INTENT(IN)  :: PD_G1       ! depth of first layer
!
!
!*      0.2    declarations of local variables
!
INTEGER                           :: JJ, JI, JNLVL
!
!
REAL, PARAMETER                   :: ZGRIDFACTOR = 3.0 ! soil depth factor
!                                                      ! of increase with depth
!                                                      ! for all *sub-surface* 
!                                                      ! layers. Note, uppermost
!                                                      ! layer fixed by other
!                                                      ! constraints.          (-)
!
REAL                              :: ZD_G1 = 0.01      ! uppermost soil layer 
!                                                      ! thickness/depth       (m)
!                                                      ! Can not be too thin as 
!                                                      ! then definition of soil
!                                                      ! properties (i.e. phyiscal
!                                                      ! representation of) and 
!                                                      ! accuarcy of
!                                                      ! numerical solution come
!                                                      ! into question. If it is too
!                                                      ! thick, then resolution of
!                                                      ! diurnal cycle not as valid.
!                                                      ! Also chosen to comply with
!                                                      ! remotely sensed soil moisture.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
JNLVL = SIZE(PD_G)
!
IF (PRESENT(PD_G1)) ZD_G1 = PD_G1
!
IF (PSOILDEPTH < JNLVL*ZD_G1) THEN
  !
  !*       3.     In the LIMIT For extremely thin soils
  !               ------------------------------------------
  !               This should be a RARE occurance, but 
  !               accounted for none-the-less ...:
  !               hold the ratio between all layer 
  !               thicknesses constant. 
  DO JJ = 1,JNLVL
    PD_G(JJ) = JJ*PSOILDEPTH/JNLVL
  ENDDO
  !
ELSE
  !
  PD_G(1)     = ZD_G1
  PD_G(JNLVL) = PSOILDEPTH
  !
  DO JJ=JNLVL-1,2,-1
    !*       1.     Assign soil layer depths
    !               ------------------------
    !               using a geometric relation
    !               for layers 2...N
    !               This is GENERAL rule.
    !               Note that the first soil layer
    !               is FIXED except for VERY thin
    !               soils (see #3 below).
    PD_G(JJ) = PD_G(JJ+1)/ZGRIDFACTOR
    !*       2.     When the soil is sufficiently thin
    !               ------------------------------------------
    !               We recalculate layer depths such
    !               that all layer thicknesses are >= ZD_G1
    !               We favor keeping a minimum grid thickness
    !               OVER maintaining geometric relation
    !               for increasingly thin soils. This means
    !               that uppermost soil moisture is readily
    !               comparable (i.e. for same layer thickness)
    !               EVERYWHERE except for most thin soils (below).
    PD_G(JJ) = MAX(PD_G(JJ), JJ*ZD_G1)
    !
  ENDDO     
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEBGRID3
