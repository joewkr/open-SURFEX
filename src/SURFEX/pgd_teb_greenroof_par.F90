!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GREENROOF_PAR (DTCO, DTV, UG, U, USS, IO, S, K, KDIM, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_GREENROOF_PAR* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    A. Lemonsu       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original                     09/2009
!!    A. Lemonsu / C. de Munck     04/2011    : TEB GreenRoof
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t
!
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_TEB_VEG,              ONLY : NLAYER_GR_MAX, NTIME_GR_MAX
!
USE MODD_PGDWORK,              ONLY : CATYPE
!
USE MODI_READ_NAM_PGD_TEB_GREENROOF
USE MODI_PGD_FIELD
USE MODI_TEST_NAM_VAR_SURF
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),    INTENT(IN)             :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                                     :: ILUOUT    ! output listing logical unit
INTEGER                                     :: ILUNAM    ! namelist file  logical unit
LOGICAL                                     :: GFOUND    ! true if namelist is found
!
INTEGER                                     :: JLAYER    ! loop counter on layers
INTEGER                                     :: JLAYER_GR ! loop counter on green roof layers
INTEGER                                     :: JTIME     ! loop counter on time
INTEGER                                     :: JPATCH    ! loop counter on patch
INTEGER                                     :: JVEGTYPE  ! loop counter on vegtypes
!
!
INTEGER, PARAMETER                          :: JPGROUND_MAX   = 20
INTEGER, PARAMETER                          :: JPVEGTYPE_MAX  = 12
!
! declaration of namelist variables
INTEGER                                     :: ILAYER_GR      ! number of green roof physical layers
INTEGER                                     :: ITIME_GR       ! ntime for green roof parameters
 CHARACTER(LEN=5)                            :: YTYP_GR        ! type of green roof
!
! uniform value
!
REAL,DIMENSION(NLAYER_GR_MAX)              :: ZUNIF_OM_GR      ! fraction of organic matter (OM) in green roof layer
REAL,DIMENSION(NLAYER_GR_MAX)              :: ZUNIF_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
REAL,DIMENSION(NLAYER_GR_MAX)              :: ZUNIF_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
REAL,DIMENSION(NTIME_GR_MAX)               :: ZUNIF_LAI_GR     ! LAI of green roof vegetation
!
! name of files containing data
!
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: YFNAM_OM_GR      ! fraction of organic matter (OM) in green roof layer
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: YFNAM_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: YFNAM_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
 CHARACTER(LEN=28),DIMENSION(NTIME_GR_MAX)  :: YFNAM_LAI_GR     ! LAI  of green roof
!
! type of files containing data
!
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: YFTYP_OM_GR      ! fraction of organic matter (OM) in green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: YFTYP_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: YFTYP_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NTIME_GR_MAX)  :: YFTYP_LAI_GR     ! LAI  of green roof
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF_PAR',0,ZHOOK_HANDLE)
!
ILAYER_GR        = 0
ITIME_GR         = 0
YTYP_GR          = '     '           
!
ZUNIF_OM_GR      = XUNDEF
ZUNIF_CLAY_GR    = XUNDEF
ZUNIF_SAND_GR    = XUNDEF
ZUNIF_LAI_GR     = XUNDEF
!
YFNAM_OM_GR      = '                            '
YFNAM_CLAY_GR    = '                            '
YFNAM_SAND_GR    = '                            '
YFNAM_LAI_GR     = '                            '
!
YFTYP_OM_GR      = '      '
YFTYP_CLAY_GR    = '      '
YFTYP_SAND_GR    = '      '
YFTYP_LAI_GR     = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input files for green roof characteristics
!             -------------------------------------------
!
 CALL READ_NAM_PGD_TEB_GREENROOF(HPROGRAM, ITIME_GR,ILAYER_GR,YTYP_GR,                     &
                                ZUNIF_OM_GR, ZUNIF_CLAY_GR, ZUNIF_SAND_GR, ZUNIF_LAI_GR,  &
                                YFNAM_OM_GR, YFNAM_CLAY_GR, YFNAM_SAND_GR, YFNAM_LAI_GR,  &
                                YFTYP_OM_GR, YFTYP_CLAY_GR, YFTYP_SAND_GR, YFTYP_LAI_GR)
!
DTV%NTIME              = ITIME_GR
IO%NGROUND_LAYER      = ILAYER_GR
IO%CTYP_COV           = YTYP_GR
!
!*           Coherence of options for the green roof type
!             -------------------------------------------
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CTYP_GR',IO%CTYP_COV,'GRASS','SEDUM')
!
!
ALLOCATE(S%XSOC       (KDIM,IO%NGROUND_LAYER))
ALLOCATE(K%XCLAY      (KDIM,IO%NGROUND_LAYER))
ALLOCATE(K%XSAND      (KDIM,IO%NGROUND_LAYER))
ALLOCATE(DTV%XPAR_LAI   (KDIM,DTV%NTIME ,1))
!
!-------------------------------------------------------------------------------
!
!*    3.2      Uniform fields are prescribed
!             -----------------------------
!
CATYPE = 'ARI'
!
!
DO JLAYER_GR=1,IO%NGROUND_LAYER
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'OM_GR: fraction of OM in GR layer','BLD',YFNAM_OM_GR(JLAYER_GR),   &
               YFTYP_OM_GR(JLAYER_GR), ZUNIF_OM_GR(JLAYER_GR), S%XSOC(:,JLAYER_GR))
ENDDO
!
DO JLAYER_GR=1,IO%NGROUND_LAYER
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'CLAY_GR: fraction of CLAY in the non-OM part of GR layer','BLD',YFNAM_CLAY_GR(JLAYER_GR),   &
               YFTYP_CLAY_GR(JLAYER_GR), ZUNIF_CLAY_GR(JLAYER_GR), K%XCLAY(:,JLAYER_GR))
ENDDO
!
DO JLAYER_GR=1,IO%NGROUND_LAYER
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'SAND_GR: fraction of SAND in the non-OM part of GR layer','BLD',YFNAM_SAND_GR(JLAYER_GR),   &
               YFTYP_SAND_GR(JLAYER_GR), ZUNIF_SAND_GR(JLAYER_GR), K%XSAND(:,JLAYER_GR))
ENDDO
!
DO JTIME=1,DTV%NTIME
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'LAI_GR: LAI of green roof','BLD',YFNAM_LAI_GR(JTIME),  &
                YFTYP_LAI_GR(JTIME),ZUNIF_LAI_GR(JTIME),DTV%XPAR_LAI(:,JTIME,1))
!
ENDDO
!
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_GREENROOF_PAR
