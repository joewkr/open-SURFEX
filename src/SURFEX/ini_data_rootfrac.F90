!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_DATA_ROOTFRAC( PDG, PROOTDEPTH, PROOT_EXT, PROOT_LIN,  &
                                    PROOTFRAC, OGV                          )

!     ##########################################################################
!
!!****  *INI_DATA_ROOTFRAC*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the soil grid configuration using a reference grid
!     Also compute the root fraction
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
!!    Decharme et al. (2011)
!!      
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!      new version :
!!      B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/04/03
!!      new version :10/08/2011
!!      P. Samuelsson  02/2012  MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR, ONLY : NOPTIMLAYER, XOPTIMGRID
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:,:), INTENT(IN) :: PDG         ! depth of base of soil layers (m)
REAL,    DIMENSION(:),   INTENT(IN) :: PROOTDEPTH  ! effective root depth         (m)
REAL, DIMENSION(:), INTENT(IN)     :: PROOT_EXT
REAL, DIMENSION(:), INTENT(IN)     :: PROOT_LIN
LOGICAL, OPTIONAL, INTENT(IN)        :: OGV
!
REAL, DIMENSION(:,:), INTENT(OUT)  :: PROOTFRAC
!
!*      0.2    declarations of local variables
!
REAL               :: ZLOG1, ZLOG2
REAL               :: ZJACKSON ! Jackson (1996) formulation for cumulative root fraction
REAL               :: ZUNIF    ! linear formulation for cumulative root fraction
REAL               :: ZROOTFRGV ! Fraction of patch root depth given to
!                               ! grass root depth for understory ground vegetation.
!                               ! =1 for non-understory vegetation

!
INTEGER            :: INI,INL
INTEGER            :: JJ,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!        0.     Initialization
!               --------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_ROOTFRAC',0,ZHOOK_HANDLE)
!
INI    = SIZE(PDG,1)
INL    = SIZE(PDG,2)
!
ZROOTFRGV  = 1.0
IF (PRESENT(OGV)) THEN
  IF(OGV) ZROOTFRGV  = 0.5
ENDIF
!
PROOTFRAC(:,:) = XUNDEF
!
  DO JJ=1,INI
    !
    IF ( PROOTDEPTH(JJ)/=XUNDEF .AND. PROOTDEPTH(JJ)/=0.0 ) THEN 
      !
      DO JL=1,INL                
        ZLOG1    = 100. * LOG(PROOT_EXT(JJ)) * PDG    (JJ,JL)
        ZLOG2    = 100. * LOG(PROOT_EXT(JJ)) * ZROOTFRGV * PROOTDEPTH(JJ)
        ZJACKSON = MIN(1.0,(1.0-EXP(ZLOG1))/(1.0-EXP(ZLOG2)))
        ZUNIF    = MIN(1.0,(PDG(JJ,JL)/ZROOTFRGV/PROOTDEPTH(JJ))) 
        PROOTFRAC(JJ,JL) =      PROOT_LIN(JJ)  * ZUNIF    &
                                   + (1.0-PROOT_LIN(JJ)) * ZJACKSON
      ENDDO
!       No vegetation case                                    
    ELSE
      PROOTFRAC(JJ,:) = 0.0
    ENDIF
    !
  ENDDO
  
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_ROOTFRAC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE INI_DATA_ROOTFRAC
