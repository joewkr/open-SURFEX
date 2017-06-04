!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_FLAKE (DTCO, FG, F, UG, U, USS, HPROGRAM,ORM_RIVER)
!     ##############################################################
!
!!**** *PGD_FLAKE* monitor for averaging and interpolations of FLAKE physiographic fields
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    04/2013, P. Le Moigne : allow limitation of lake depth
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_DATA_LAKE,      ONLY : CLAKELDB, CSTATUSLDB
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_PGDWORK,        ONLY : CATYPE
!
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
USE MODI_PGD_FIELD

USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODI_TREAT_GLOBAL_LAKE_DEPTH
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_WRITE_COVER_TEX_WATER
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(IN)    :: ORM_RIVER    ! delete river coverage (default = false)
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
INTEGER,DIMENSION(:),ALLOCATABLE  :: IWATER_STATUS
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YWATER_DEPTH  ! file name for water depth
 CHARACTER(LEN=28)        :: YWATER_DEPTH_STATUS  ! file name for water depth status
 CHARACTER(LEN=28)        :: YWATER_FETCH
 CHARACTER(LEN=28)        :: YT_BS
 CHARACTER(LEN=28)        :: YDEPTH_BS
 CHARACTER(LEN=28)        :: YEXTCOEF_WATER

 CHARACTER(LEN=6)         :: YWATER_DEPTHFILETYPE ! water depth file type
 CHARACTER(LEN=6)         :: YWATER_FETCHFILETYPE
 CHARACTER(LEN=6)         :: YT_BSFILETYPE
 CHARACTER(LEN=6)         :: YDEPTH_BSFILETYPE
 CHARACTER(LEN=6)         :: YEXTCOEF_WATERFILETYPE

REAL                     :: XUNIF_WATER_DEPTH   ! uniform value of water depth
REAL                     :: XUNIF_WATER_FETCH
REAL                     :: XUNIF_T_BS
REAL                     :: XUNIF_DEPTH_BS
REAL                     :: XUNIF_EXTCOEF_WATER
REAL                     :: XMAX_DEPTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_FLAKE/ YWATER_DEPTH, YWATER_DEPTH_STATUS, YWATER_DEPTHFILETYPE,     &
                         XUNIF_WATER_DEPTH, YWATER_FETCH, YWATER_FETCHFILETYPE,       &
                         XUNIF_WATER_FETCH, YT_BS, YT_BSFILETYPE, XUNIF_T_BS,         &
                         YDEPTH_BS, YDEPTH_BSFILETYPE, XUNIF_DEPTH_BS,                &
                         YEXTCOEF_WATER, YEXTCOEF_WATERFILETYPE, XUNIF_EXTCOEF_WATER, &
                         XMAX_DEPTH  
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_FLAKE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
XUNIF_WATER_DEPTH  = 10.
XUNIF_WATER_FETCH  = 1000.
XUNIF_T_BS         = 286.
XUNIF_DEPTH_BS     = 1.
XUNIF_EXTCOEF_WATER= 3.
!
YWATER_DEPTH        = '                          '
YWATER_DEPTH_STATUS = '                          '
YWATER_FETCH        = '                          '
YT_BS               = '                          '
YDEPTH_BS           = '                          '
YEXTCOEF_WATER      = '                          '
!
YWATER_DEPTHFILETYPE   = '      '
YWATER_FETCHFILETYPE   = '      '
YT_BSFILETYPE          = '      '
YDEPTH_BSFILETYPE      = '      '
YEXTCOEF_WATERFILETYPE = '      '
!
XMAX_DEPTH  = 1.E+20
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_FLAKE',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_FLAKE)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence of options
!             --------------------
!
!-------------------------------------------------------------------------------
!
!*    4.      Number of points and packing
!             ----------------------------
!
 CALL GET_SURF_SIZE_n(DTCO, U, 'WATER ',FG%NDIM)
!
ALLOCATE(F%LCOVER      (JPCOVER))
ALLOCATE(F%XZS         (FG%NDIM))
ALLOCATE(FG%XLAT       (FG%NDIM))
ALLOCATE(FG%XLON       (FG%NDIM))
ALLOCATE(FG%XMESH_SIZE (FG%NDIM))
!
 CALL PACK_PGD(DTCO, U, HPROGRAM, 'WATER ', FG, F%LCOVER, F%XCOVER, F%XZS  )  
!
!-------------------------------------------------------------------------------
!
!*    5.      Water depth
!             -----------
!
ALLOCATE(F%XWATER_DEPTH  (FG%NDIM)) 
!
IF (TRIM(YWATER_DEPTH)==TRIM(CLAKELDB) .AND. TRIM(YWATER_DEPTHFILETYPE)=='DIRECT') THEN
  !      
  IF (TRIM(YWATER_DEPTH_STATUS)=='') THEN
     WRITE(ILUOUT,*)'Depth Status file name not initialized'
     WRITE(ILUOUT,*)'add YWATER_DEPTH_STATUS="GlobalLakeStatus" in NAM_DATA_FLAKE'
     CALL ABOR1_SFX('PGD_FLAKE: STATUS INPUT FILE NAME NOT SET')
  ELSEIF (TRIM(YWATER_DEPTH_STATUS)==TRIM(CSTATUSLDB)) THEN
     ALLOCATE(IWATER_STATUS  (FG%NDIM))       
     CALL TREAT_GLOBAL_LAKE_DEPTH(DTCO, UG, U, USS, &
                                  HPROGRAM,F%XWATER_DEPTH(:),IWATER_STATUS(:))
  ELSE
     WRITE(ILUOUT,*)'Wrong name for Depth Status file :',' expected: ',TRIM(CSTATUSLDB),' input: ',TRIM(YWATER_DEPTH_STATUS)
     CALL ABOR1_SFX('PGD_FLAKE: WRONG STATUS INPUT FILE NAME')
  ENDIF
  !
ELSE
  !
  IF(U%LECOCLIMAP.AND.(.NOT.ORM_RIVER))THEN
     WRITE(ILUOUT,*)'With this version of Flake, river must be removed'
     WRITE(ILUOUT,*)'Indeed, river energy budget can not be computed  '
     WRITE(ILUOUT,*)'using static lake scheme without 2D informations.'
     WRITE(ILUOUT,*)'Please add LRM_RIVER = T in NAM_COVER            '
     WRITE(ILUOUT,*)' '
     WRITE(ILUOUT,*)'If you still want to use Flake to comput river   '
     WRITE(ILUOUT,*)'energy budget, please use the two files for the  '
     WRITE(ILUOUT,*)'Kourzeneva 2009 method: ',CLAKELDB(1:LEN_TRIM(CLAKELDB)),' ',&
                                               CSTATUSLDB(1:LEN_TRIM(CSTATUSLDB))
     CALL ABOR1_SFX('PGD_FLAKE: WITH THIS VERSION OF FLAKE, LRM_RIVER MUST BE TRUE')         
  ENDIF
  !
  CATYPE='INV'
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,'water depth','WAT',YWATER_DEPTH,YWATER_DEPTHFILETYPE,XUNIF_WATER_DEPTH,F%XWATER_DEPTH(:))
  !
ENDIF
!
F%XWATER_DEPTH(:) = MIN (F%XWATER_DEPTH(:),XMAX_DEPTH)
WRITE(ILUOUT,*)'MAXIMUM LAKE DEPTH = ',XMAX_DEPTH
!
!-------------------------------------------------------------------------------
!
!*    6.      Wind fetch
!             ----------
!
ALLOCATE(F%XWATER_FETCH  (FG%NDIM)) 
!
CATYPE='ARI'
CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,'wind fetch','WAT',YWATER_FETCH,YWATER_FETCHFILETYPE,XUNIF_WATER_FETCH,F%XWATER_FETCH(:))
!
!-------------------------------------------------------------------------------
!
!*    7.      Sediments bottom temperature
!             ----------------------------
!
ALLOCATE(F%XT_BS         (FG%NDIM)) 
!
CATYPE='ARI'
CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,'sediments bottom temperature ','WAT',YT_BS,YT_BSFILETYPE,XUNIF_T_BS,F%XT_BS(:))
!
!-------------------------------------------------------------------------------
!
!*    8.      Depth of sediments layer
!             ------------------------
!
ALLOCATE(F%XDEPTH_BS     (FG%NDIM)) 
!
CATYPE='INV'
CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,'depth of sediments layer','WAT',YDEPTH_BS,YDEPTH_BSFILETYPE,XUNIF_DEPTH_BS,F%XDEPTH_BS(:))
!
!-------------------------------------------------------------------------------
!
!*    9.      Water extinction coefficient
!             ----------------------------

ALLOCATE(F%XEXTCOEF_WATER(FG%NDIM)) 
!
CATYPE='ARI'
CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,'water extinction coefficient','WAT', &
                 YEXTCOEF_WATER,YEXTCOEF_WATERFILETYPE,XUNIF_EXTCOEF_WATER, &
                 F%XEXTCOEF_WATER(:))  
!
!-------------------------------------------------------------------------------
!
!*   10.     Prints of flake parameters in a tex file
!            ----------------------------------------
!
IF (NRANK==NPIO) CALL WRITE_COVER_TEX_WATER
!
IF (LHOOK) CALL DR_HOOK('PGD_FLAKE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FLAKE
