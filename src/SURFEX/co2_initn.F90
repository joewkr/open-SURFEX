!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CO2_INIT_n (IO, S, PK, PEK, KSIZE, PCO2   )
!     #####################
!
!!****  *CO2_INIT_n* - routine to initialize ISBA-AGS variables
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2003 
!!      J.C. Calvet 01/2004 Externalization
!!      P Le Moigne 11/2004 cotwoinit changed into cotwoinit_n
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S Lafont    09/2008 Add initialisation of POI and ABC (needed for TORI)
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 04/2009 : Add carbon spinup
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_S_t, ISBA_PE_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK,NPIO
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_COTWOINIT_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
INTEGER, INTENT(IN) :: KSIZE
!
REAL, DIMENSION(:), INTENT(IN) :: PCO2 ! air CO2 concentration (kg/kg)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_CO2            ! air CO2 concentration (kg/kg)
!
INTEGER :: IPATCH
INTEGER :: INBIOMASS
INTEGER :: JP    ! loop on tiles
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_INIT_N',0,ZHOOK_HANDLE)
!
INBIOMASS = SIZE(PK%XINCREASE,2)
!
IF (KSIZE == 0 ) THEN
  IF (LHOOK) CALL DR_HOOK('CO2_INIT_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (MAXVAL(PEK%XGMES(:)).NE.XUNDEF .OR. MINVAL(PEK%XGMES(:)).NE.XUNDEF) THEN

  CALL PACK_CO2_INIT(PK%NR_P(:),KSIZE)
  !
  CALL COTWOINIT_n(IO, S, PK, PEK, ZP_CO2 )

  PK%XINCREASE = 0.
  PK%XTURNOVER = 0.
  !

ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_INIT_N',1,ZHOOK_HANDLE)
CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE PACK_CO2_INIT(KMASK,KSIZE)
IMPLICIT NONE
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
INTEGER JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PACK_CO2_INIT',0,ZHOOK_HANDLE)
!
ALLOCATE(ZP_CO2          (KSIZE))
!
DO JJ=1,KSIZE
  JI                     =    KMASK(JJ)
  ZP_CO2          (JJ)   =    PCO2          (JI)
END DO
!
IF (LHOOK) CALL DR_HOOK('PACK_CO2_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PACK_CO2_INIT
!
END SUBROUTINE CO2_INIT_n
