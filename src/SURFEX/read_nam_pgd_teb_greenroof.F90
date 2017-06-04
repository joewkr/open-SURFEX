!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_TEB_GREENROOF(HPROGRAM, KTIME_GR,KLAYER_GR,HTYP_GR,                   &
                                            PUNIF_OM_GR, PUNIF_CLAY_GR, PUNIF_SAND_GR, PUNIF_LAI_GR,&
                                            HFNAM_OM_GR, HFNAM_CLAY_GR, HFNAM_SAND_GR, HFNAM_LAI_GR,&
                                            HFTYP_OM_GR, HFTYP_CLAY_GR, HFTYP_SAND_GR, HFTYP_LAI_GR )
!     ##############################################################
!
!!**** *READ_NAM_PGD_TEB_GREENROOF* reading of greenroof namelist
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
!!    A. Lemonsu / C. de Munck        : TEB GreenRoof namelist
!!
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original                     07/2011
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_TEB_VEG,              ONLY : NTIME_GR_MAX, NLAYER_GR_MAX
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=6),                INTENT(IN)    :: HPROGRAM     ! Type of program
INTEGER,                         INTENT(OUT)   :: KTIME_GR
INTEGER,                         INTENT(OUT)   :: KLAYER_GR
 CHARACTER(LEN=5),                INTENT(OUT)   :: HTYP_GR
REAL,DIMENSION(:),               INTENT(OUT)   :: PUNIF_OM_GR
REAL,DIMENSION(:),               INTENT(OUT)   :: PUNIF_CLAY_GR
REAL,DIMENSION(:),               INTENT(OUT)   :: PUNIF_SAND_GR
REAL,DIMENSION(:),               INTENT(OUT)   :: PUNIF_LAI_GR
 CHARACTER(LEN=28),DIMENSION(:),  INTENT(OUT)   :: HFNAM_OM_GR
 CHARACTER(LEN=28),DIMENSION(:),  INTENT(OUT)   :: HFNAM_CLAY_GR
 CHARACTER(LEN=28),DIMENSION(:),  INTENT(OUT)   :: HFNAM_SAND_GR
 CHARACTER(LEN=28),DIMENSION(:),  INTENT(OUT)   :: HFNAM_LAI_GR
 CHARACTER(LEN=6),DIMENSION(:),   INTENT(OUT)   :: HFTYP_OM_GR
 CHARACTER(LEN=6),DIMENSION(:),   INTENT(OUT)   :: HFTYP_CLAY_GR
 CHARACTER(LEN=6),DIMENSION(:),   INTENT(OUT)   :: HFTYP_SAND_GR
 CHARACTER(LEN=6),DIMENSION(:),   INTENT(OUT)   :: HFTYP_LAI_GR

!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                                    :: NLAYER_GR      ! 
INTEGER                                    :: NTIME_GR       ! 
 CHARACTER(LEN=5)                           :: CTYP_GR        ! type of green roof
!
! uniform value
!
REAL,DIMENSION(NLAYER_GR_MAX)              :: XUNIF_OM_GR      ! fraction of organic matter (OM) in green roof layer
REAL,DIMENSION(NLAYER_GR_MAX)              :: XUNIF_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
REAL,DIMENSION(NLAYER_GR_MAX)              :: XUNIF_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
REAL,DIMENSION(NTIME_GR_MAX)               :: XUNIF_LAI_GR     ! LAI of green roof vegetation
!
! name of files containing data
!
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: CFNAM_OM_GR      ! fraction of organic matter (OM) in green roof layer
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: CFNAM_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
 CHARACTER(LEN=28),DIMENSION(NLAYER_GR_MAX) :: CFNAM_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
 CHARACTER(LEN=28),DIMENSION(NTIME_GR_MAX)  :: CFNAM_LAI_GR     ! LAI  of green roof
!
! type of files containing data
!
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: CFTYP_OM_GR      ! fraction of organic matter (OM) in green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: CFTYP_CLAY_GR    ! fraction of clay for the non-OM part of the green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NLAYER_GR_MAX) :: CFTYP_SAND_GR    ! fraction of sand for the non-OM part of the green roof layer
 CHARACTER(LEN=6 ),DIMENSION(NTIME_GR_MAX)  :: CFTYP_LAI_GR     ! LAI  of green roof
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TEB_GREENROOF/ NTIME_GR,NLAYER_GR,                                      &
                                 CTYP_GR,                                                 & 
                                 XUNIF_OM_GR, XUNIF_CLAY_GR, XUNIF_SAND_GR, XUNIF_LAI_GR, &
                                 CFNAM_OM_GR, CFNAM_CLAY_GR, CFNAM_SAND_GR, CFNAM_LAI_GR, &
                                 CFTYP_OM_GR, CFTYP_CLAY_GR, CFTYP_SAND_GR, CFTYP_LAI_GR
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF_PAR',0,ZHOOK_HANDLE)
!
NTIME_GR         = 1
NLAYER_GR        = 6
CTYP_GR          = 'GRASS'           ! Grasses - graminoïds
!
XUNIF_OM_GR      = XUNDEF
XUNIF_CLAY_GR    = XUNDEF
XUNIF_SAND_GR    = XUNDEF
XUNIF_LAI_GR     = XUNDEF
!
CFNAM_OM_GR      = '                            '
CFNAM_CLAY_GR    = '                            '
CFNAM_SAND_GR    = '                            '
CFNAM_LAI_GR     = '                            '
!
CFTYP_OM_GR      = '      '
CFTYP_CLAY_GR    = '      '
CFTYP_SAND_GR    = '      '
CFTYP_LAI_GR     = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of green roof namelist
!             ------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB_GREENROOF',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB_GREENROOF)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!--------------------------------------------------
IF (NTIME_GR/=1 .AND. NTIME_GR/=12) THEN
  CALL ABOR1_SFX('NTIME_GR must be either equal to 1 (uniform LAI) or 12 (monthly LAI)')
END IF
!--------------------------------------------------
!
KTIME_GR           = NTIME_GR
KLAYER_GR          = NLAYER_GR
HTYP_GR            = CTYP_GR
PUNIF_OM_GR        = XUNIF_OM_GR  
PUNIF_CLAY_GR      = XUNIF_CLAY_GR
PUNIF_SAND_GR      = XUNIF_SAND_GR
PUNIF_LAI_GR       = XUNIF_LAI_GR 
HFNAM_OM_GR        = CFNAM_OM_GR  
HFNAM_CLAY_GR      = CFNAM_CLAY_GR
HFNAM_SAND_GR      = CFNAM_SAND_GR
HFNAM_LAI_GR       = CFNAM_LAI_GR 
HFTYP_OM_GR        = CFTYP_OM_GR  
HFTYP_CLAY_GR      = CFTYP_CLAY_GR
HFTYP_SAND_GR      = CFTYP_SAND_GR
HFTYP_LAI_GR       = CFTYP_LAI_GR 
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_TEB_GREENROOF
