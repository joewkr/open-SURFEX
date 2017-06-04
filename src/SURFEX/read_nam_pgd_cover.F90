!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_COVER(HPROGRAM, HCOVER, HFILETYPE, PUNIF_COVER,  &
                                    PRM_COVER, PRM_COAST, PRM_LAKE, ORM_RIVER, &
                                    PRM_SEA, OORCA_GRID, PLAT_ANT, OIMP_COVER )  
!     ##############################################################
!
!!**** *READ_NAM_PGD_COVER* reads namelist for Cover
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
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2010
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : NCOVER
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
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
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM    ! Type of program
CHARACTER(LEN=28),   INTENT(OUT)   :: HCOVER      ! file name for cover types
CHARACTER(LEN=6),    INTENT(OUT)   :: HFILETYPE   ! data file type
REAL, DIMENSION(:),  INTENT(OUT)   :: PUNIF_COVER ! value of each cover (cover will be uniform on the horizontal)
REAL,                INTENT(OUT)   :: PRM_COVER   ! limit of coverage under which the cover is removed. Default is 1.E-6
REAL,                INTENT(OUT)   :: PRM_COAST   ! limit of coast coverage
REAL,                INTENT(OUT)   :: PRM_LAKE    ! limit of inland lake coverage                                       
LOGICAL,             INTENT(OUT)   :: ORM_RIVER   ! delete river coverage                                       
REAL,                INTENT(OUT)   :: PRM_SEA     ! limit of sea coverage
LOGICAL,             INTENT(OUT)   :: OORCA_GRID  ! flag to compatibility between Surfex and Orca grid 
REAL,                INTENT(OUT)   :: PLAT_ANT    ! Lattitude limit from Orca grid (Antartic)
LOGICAL,             INTENT(OUT)   :: OIMP_COVER  ! Imposed values for Cover from another PGD file
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL, DIMENSION(NCOVER) :: XUNIF_COVER ! value of each cover (cover will be
!                                                   uniform on the horizontal)
!
CHARACTER(LEN=28)        :: YCOVER      ! file name for cover types
CHARACTER(LEN=6)         :: YCOVERFILETYPE   ! data file type
REAL                     :: XRM_COVER   ! limit of coverage under which the
                                        ! cover is removed. Default is 1.E-6
REAL                     :: XRM_COAST   ! limit of coast coverage under which
                                        ! the coast is replaced by sea or
                                        ! inland water. Default is 1.
!
REAL                     :: XRM_LAKE    ! limit of inland lake coverage under which
                                        ! the water is removed. Default is 0.0
!
LOGICAL                  :: LRM_RIVER   ! delete inland river coverage. Default is false
!                                        
REAL                     :: XRM_SEA     ! limit of sea coverage under which
                                        ! the sea is removed. Default is 0.0
!
LOGICAL                  :: LORCA_GRID  ! flag to compatibility between Surfex and Orca grid 
                                        ! (Earth Model over Antarctic)
REAL                     :: XLAT_ANT    ! Lattitude limit from Orca grid (Antartic)
!
LOGICAL                  :: LIMP_COVER  ! Imposed values for Cover from another PGD file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_COVER/ YCOVER, YCOVERFILETYPE, XUNIF_COVER, XRM_COVER, XRM_COAST,     &
                    XRM_LAKE, LRM_RIVER, XRM_SEA, LORCA_GRID, XLAT_ANT, LIMP_COVER 
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_COVER',0,ZHOOK_HANDLE)
XUNIF_COVER(:) = 0.
YCOVER         = '                          '
YCOVERFILETYPE = '      '
XRM_COVER      = 1.E-6
XRM_COAST      = 1.0
XRM_LAKE       = 0.0
LRM_RIVER      = .FALSE.
XRM_SEA        = 0.0
!
LORCA_GRID     = .FALSE.
XLAT_ANT       = -77.0
!
LIMP_COVER     = .FALSE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_COVER',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_COVER)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
HCOVER      = YCOVER      ! file name for cover types
HFILETYPE   = YCOVERFILETYPE   ! data file type
PUNIF_COVER = XUNIF_COVER(1:SIZE(PUNIF_COVER)) ! value of each cover (cover will be uniform on the horizontal)
PRM_COVER   = XRM_COVER   ! limit of coverage under which the cover is removed. Default is 1.E-6
PRM_COAST   = XRM_COAST   ! limit of coast coverage
PRM_LAKE    = XRM_LAKE    ! limit of inland lake coverage                                       
ORM_RIVER   = LRM_RIVER   ! delete river coverage                                       
PRM_SEA     = XRM_SEA     ! limit of sea coverage
OORCA_GRID  = LORCA_GRID  ! flag to compatibility between Surfex and Orca grid 
PLAT_ANT    = XLAT_ANT    ! Lattitude limit from Orca grid (Antartic)
OIMP_COVER  = LIMP_COVER  ! Imposed values for Cover from another PGD file
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_COVER
