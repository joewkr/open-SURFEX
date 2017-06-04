!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_FRAC (DTCO, UG, U, USS, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_FRAC* monitor for averaging and interpolations of cover fractions
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
!!    Original    10/12/97
!!
!!       Modified 08/12/05, P. Le Moigne: user defined fields
!!       Modified 04/2013   V. Masson   : set a cover containing garden for TOWN default
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL, CGRID
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NCOVER, NTYPE
!
USE MODD_PGDWORK,        ONLY : CATYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_SUM_ON_ALL_PROCS
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JCOVER    ! loop counter on covers
!
REAL, DIMENSION(NL)   :: ZSUM      ! sum of 4 tiles fractions
!
!*    0.3    Declaration of namelists
!            ------------------------
!
LOGICAL  :: LECOCLIMAP  ! F if ecoclimap is not used
LOGICAL  :: LECOSG      ! F if ecosg is not used
REAL     :: XUNIF_SEA   ! value of sea    fraction
REAL     :: XUNIF_WATER ! value of water  fraction
REAL     :: XUNIF_NATURE! value of nature fraction
REAL     :: XUNIF_TOWN  ! value of town   fraction
!
! name of files containing data
!
 CHARACTER(LEN=28)     :: CFNAM_SEA    ! name of sea    file
 CHARACTER(LEN=28)     :: CFNAM_WATER  ! name of water  file
 CHARACTER(LEN=28)     :: CFNAM_NATURE ! name of nature file
 CHARACTER(LEN=28)     :: CFNAM_TOWN   ! name of town   file
!
! type of files containing data
!
 CHARACTER(LEN=6)      :: CFTYP_SEA    ! type of sea    file
 CHARACTER(LEN=6)      :: CFTYP_WATER  ! type of water  file
 CHARACTER(LEN=6)      :: CFTYP_NATURE ! type of nature file
 CHARACTER(LEN=6)      :: CFTYP_TOWN   ! type of town   file
!
INTEGER, DIMENSION(4) :: ID_COV
INTEGER               :: ICOVER       ! 0 if cover is not present, >1 if present somewhere
!                                     ! (even on another processor)
INTEGER               :: ICPT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
NAMELIST/NAM_FRAC/ LECOCLIMAP, LECOSG,                               &
                   XUNIF_SEA, XUNIF_WATER, XUNIF_NATURE, XUNIF_TOWN, &
                   CFNAM_SEA, CFNAM_WATER, CFNAM_NATURE, CFNAM_TOWN, &
                   CFTYP_SEA, CFTYP_WATER, CFTYP_NATURE, CFTYP_TOWN  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_FRAC',0,ZHOOK_HANDLE)
XUNIF_SEA      = XUNDEF
XUNIF_WATER    = XUNDEF
XUNIF_NATURE   = XUNDEF
XUNIF_TOWN     = XUNDEF
LECOCLIMAP     = .TRUE.
LECOSG         = .FALSE.
CFNAM_SEA   (:)= '                            '
CFNAM_WATER (:)= '                            '
CFNAM_NATURE(:)= '                            '
CFNAM_TOWN  (:)= '                            '
CFTYP_SEA   (:)= '      '
CFTYP_WATER (:)= '      '
CFTYP_NATURE(:)= '      '
CFTYP_TOWN  (:)= '      '
!
U%LECOCLIMAP = .TRUE.
U%LECOSG = .FALSE.
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_FRAC',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_FRAC)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
U%LECOSG = LECOSG
!
!-------------------------------------------------------------------------------
!
IF ((LEN_TRIM(CFNAM_SEA)/=0 .OR. XUNIF_SEA/=XUNDEF) .AND. (LEN_TRIM(CFNAM_WATER)/=0 .OR. XUNIF_WATER/=XUNDEF) .AND. &
    (LEN_TRIM(CFNAM_NATURE)/=0 .OR. XUNIF_NATURE/=XUNDEF) .AND. (LEN_TRIM(CFNAM_TOWN)/=0 .OR. XUNIF_TOWN/=XUNDEF)) THEN
!
  ALLOCATE(U%XSEA   (NL))
  ALLOCATE(U%XWATER (NL))
  ALLOCATE(U%XNATURE(NL))
  ALLOCATE(U%XTOWN  (NL))
!
!*    3.      Uniform fractions are prescribed
!             --------------------------------
!
  IF (XUNIF_SEA/=XUNDEF .AND. XUNIF_WATER/=XUNDEF .AND. XUNIF_NATURE/=XUNDEF .AND.  XUNIF_TOWN/=XUNDEF) THEN
!
!*    3.1     Verification of the total input cover fractions
!             -----------------------------------------------
!
    IF (ABS(XUNIF_SEA+XUNIF_WATER+XUNIF_NATURE+XUNIF_TOWN-1.)>1.E-6) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*********************************************************'
      WRITE(ILUOUT,*) '* Error in fractions preparation                        *'
      WRITE(ILUOUT,*) '* The prescribed fractions do not fit                   *'
      WRITE(ILUOUT,*) '* The sum of all 4 fractions must be equal to 1 exactly *'
      WRITE(ILUOUT,*) '*********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_FRAC: SUM OF ALL FRACTIONS MUST BE 1.')
!
!*    3.2     Use of the presribed cover fractions
!             ------------------------------------
!
    ELSE
!
      U%XSEA    = XUNIF_SEA 
      U%XWATER  = XUNIF_WATER
      U%XNATURE = XUNIF_NATURE
      U%XTOWN   = XUNIF_TOWN

    END IF
!
!*    3.3     No data
!             -------
!
  ELSE

    CATYPE = 'ARI'
    IF (XUNIF_SEA==XUNDEF) THEN
      CALL PGD_FIELD(DTCO, UG, U, USS, &
                     HPROGRAM,'XSEA: sea fraction      ','ALL', CFNAM_SEA   , &
                    CFTYP_SEA   , XUNIF_SEA   , U%XSEA(:)   )  
    ELSE                 
      U%XSEA(:) = XUNIF_SEA
    ENDIF
    IF (XUNIF_WATER==XUNDEF) THEN
      CALL PGD_FIELD(DTCO, UG, U, USS, &
                     HPROGRAM,'XWATER: water fraction  ','ALL', CFNAM_WATER , &
                    CFTYP_WATER , XUNIF_WATER , U%XWATER(:) )  
    ELSE                    
      U%XWATER(:) = XUNIF_WATER
    ENDIF
    IF (XUNIF_NATURE==XUNDEF) THEN
      CALL PGD_FIELD(DTCO, UG, U, USS, &
                     HPROGRAM,'XNATURE: nature fraction','ALL', CFNAM_NATURE, &
                    CFTYP_NATURE, XUNIF_NATURE, U%XNATURE(:))  
    ELSE                    
      U%XNATURE(:) = XUNIF_NATURE
    ENDIF
    IF (XUNIF_TOWN==XUNDEF) THEN
      CALL PGD_FIELD(DTCO, UG, U, USS, &
                     HPROGRAM,'XTOWN: town fraction    ','ALL', CFNAM_TOWN  , &
                    CFTYP_TOWN  , XUNIF_TOWN  , U%XTOWN(:)  )  
    ELSE                    
      U%XTOWN(:) = XUNIF_TOWN
    ENDIF
  ENDIF

ELSE
!
!*    4.      No prescription of fractions
!             ----------------------------
!
  IF (LHOOK) CALL DR_HOOK('PGD_FRAC',1,ZHOOK_HANDLE)
  RETURN
!
ENDIF
!-------------------------------------------------------------------------------
!         consistency check
!         ------------------
!
ZSUM(:) = U%XSEA(:) + U%XNATURE(:) + U%XWATER(:) + U%XTOWN(:)

U%XSEA(:)    = U%XSEA(:)    / ZSUM(:)
U%XNATURE(:) = U%XNATURE(:) / ZSUM(:)
U%XWATER(:)  = U%XWATER(:)  / ZSUM(:)
U%XTOWN(:)   = U%XTOWN(:)   / ZSUM(:)
!
!-------------------------------------------------------------------------------

WRITE(ILUOUT,*) ' '
!-------------------------------------------------------------------------------
!
U%LECOCLIMAP = LECOCLIMAP
!
!*    5.      List of cover present
!             ---------------------
!
IF (.NOT.LECOCLIMAP) THEN

  IF (.NOT.LECOSG) THEN
    ID_COV(1) = 1
    ID_COV(2) = 2
    ID_COV(3) = 4
    ID_COV(4) = 151
    JPCOVER   = NCOVER
  ELSE
    ID_COV(1) = 21
    ID_COV(2) = 22
    ID_COV(3) = 1
    ID_COV(4) = 20
    JPCOVER = SUM(NTYPE)    
  ENDIF

  ALLOCATE(U%LCOVER(JPCOVER))
  U%LCOVER(:) = .FALSE.
  ICOVER = 0
  ICPT= SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XSEA(:)/=0. ,'COV')
  IF (ICPT/=0) THEN
    U%LCOVER(ID_COV(1)) = .TRUE.
    ICOVER=ICOVER+1
  ENDIF
  ICPT= SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XWATER(:)/=0. ,'COV')
  IF (ICPT/=0) THEN  
    U%LCOVER(ID_COV(2)) = .TRUE.
    ICOVER=ICOVER+1
  ENDIF
  ICPT= SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XNATURE(:)/=0. ,'COV')
  IF (ICPT/=0) THEN  
    U%LCOVER(ID_COV(3)) = .TRUE.
    ICOVER=ICOVER+1
  ENDIF
  ICPT= SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XTOWN(:)/=0. ,'COV')
  IF (ICPT/=0) THEN  
    U%LCOVER(ID_COV(4)) = .TRUE.
    ICOVER=ICOVER+1
  ENDIF

  ALLOCATE(U%XCOVER (NL,ICOVER))

  ICPT = 0
  IF (U%LCOVER(ID_COV(1))) THEN
    ICPT = ICPT + 1
    U%XCOVER(:,ICPT) = U%XSEA(:)
  ENDIF
  IF (U%LCOVER(ID_COV(2))) THEN
    ICPT = ICPT + 1
    U%XCOVER(:,ICPT) = U%XWATER(:)
  ENDIF
  IF (U%LCOVER(ID_COV(3))) THEN
    ICPT = ICPT + 1
    U%XCOVER(:,ICPT) = U%XNATURE(:)
  ENDIF
  IF (U%LCOVER(ID_COV(4))) THEN
    ICPT = ICPT + 1
    U%XCOVER(:,ICPT) = U%XTOWN(:)
  ENDIF
  
  ! comment V. Masson: to use this cover type for town by default avoids crashes
  ! when garden fraction is specified but no garden vegetation parameters.
  ! In this cas, the properties for garden come from the cover 151
!
!
!-------------------------------------------------------------------------------
!
!*    6.      Land - sea fractions
!             --------------------
!
  U%NSIZE_NATURE    = COUNT(U%XNATURE(:) > 0.0)
  U%NSIZE_WATER     = COUNT(U%XWATER (:) > 0.0)
  U%NSIZE_SEA       = COUNT(U%XSEA   (:) > 0.0)
  U%NSIZE_TOWN      = COUNT(U%XTOWN  (:) > 0.0)
  U%NSIZE_FULL      = NL
!
  U%NDIM_NATURE    = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XNATURE(:) > 0.0, 'DIM')
  U%NDIM_WATER     = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XWATER (:) > 0.0, 'DIM')
  U%NDIM_SEA       = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XSEA   (:) > 0.0, 'DIM')
  U%NDIM_TOWN      = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,U%XTOWN  (:) > 0.0, 'DIM')
!  
ENDIF
IF (LHOOK) CALL DR_HOOK('PGD_FRAC',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FRAC
