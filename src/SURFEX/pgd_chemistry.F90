!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_CHEMISTRY (CHE, DTCO, UG, U, USS, &
                                HPROGRAM,OCH_EMIS)
!     ##############################################################
!
!!**** *PGD_CHEMISTRY* monitor for averaging and interpolations of physiographic fields
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
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PGD_GRID,           ONLY : NL
USE MODD_PGDWORK,            ONLY : CATYPE
USE MODD_SURF_PAR,           ONLY : XUNDEF
USE MODD_CH_SURF,            ONLY : JPEMISMAX_F
!
USE MODI_GET_LUOUT
USE MODI_PGD_FIELD
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_SURF_SIZE_n
USE MODI_UNPACK_SAME_RANK
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_SURF_MASK_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(CH_EMIS_FIELD_t), INTENT(INOUT) :: CHE
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL,             INTENT(OUT)   :: OCH_EMIS     ! emission flag
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
INTEGER                           :: JNBR      ! loop counter on dummy fields
INTEGER                           :: ILU, IL_SEA, IL_LAND, IL
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                                :: NEMIS_PGD_NBR
 CHARACTER(LEN=40), DIMENSION(JPEMISMAX_F):: CEMIS_PGD_NAME
 CHARACTER(LEN=40), DIMENSION(JPEMISMAX_F):: CEMIS_PGD_COMMENT
INTEGER,           DIMENSION(JPEMISMAX_F):: NEMIS_PGD_TIME
 CHARACTER(LEN=3),  DIMENSION(JPEMISMAX_F):: CEMIS_PGD_AREA
 CHARACTER(LEN=3),  DIMENSION(JPEMISMAX_F):: CEMIS_PGD_ATYPE
 CHARACTER(LEN=28), DIMENSION(JPEMISMAX_F):: CEMIS_PGD_FILE
 CHARACTER(LEN=6),  DIMENSION(JPEMISMAX_F):: CEMIS_PGD_FILETYPE
 CHARACTER(LEN=6)                       :: YMASK
REAL, DIMENSION(:), ALLOCATABLE :: ZEMIS_FIELD, ZEMIS_FIELDS
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
NAMELIST/NAM_CH_EMIS_PGD/ NEMIS_PGD_NBR,CEMIS_PGD_NAME,NEMIS_PGD_TIME,&
       CEMIS_PGD_COMMENT,CEMIS_PGD_AREA,CEMIS_PGD_ATYPE,CEMIS_PGD_FILE,&
       CEMIS_PGD_FILETYPE  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
!
IF (LHOOK) CALL DR_HOOK('PGD_CHEMISTRY',0,ZHOOK_HANDLE)
NEMIS_PGD_NBR = 0  
CEMIS_PGD_NAME(:)    = '                           '
NEMIS_PGD_TIME(:)    = 0
CEMIS_PGD_COMMENT(:) = ''
CEMIS_PGD_AREA(:)    = 'ALL'
CEMIS_PGD_FILETYPE(:)= 'DIRECT'
CEMIS_PGD_FILE(:)    = '                           '
CEMIS_PGD_ATYPE(:)   = 'ARI'
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_CH_EMIS_PGD',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_CH_EMIS_PGD)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocation
!             ----------
!
CHE%NEMIS_NBR = NEMIS_PGD_NBR
!
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'LAND', IL_LAND)
 CALL GET_SURF_SIZE_n(DTCO, U, &
                      'SEA   ',IL_SEA)
!
!
ALLOCATE(ZEMIS_FIELDS (NL))
!
ALLOCATE(CHE%XEMIS_FIELDS (NL,CHE%NEMIS_NBR))
ALLOCATE(CHE%CEMIS_AREA   (CHE%NEMIS_NBR))
ALLOCATE(CHE%CEMIS_COMMENT(CHE%NEMIS_NBR))
ALLOCATE(CHE%CEMIS_NAME   (CHE%NEMIS_NBR))
ALLOCATE(CHE%NEMIS_TIME   (CHE%NEMIS_NBR))
!
CHE%CEMIS_AREA   (:) = CEMIS_PGD_AREA   (1:CHE%NEMIS_NBR)
CHE%CEMIS_NAME   (:) = CEMIS_PGD_NAME   (1:CHE%NEMIS_NBR)
CHE%NEMIS_TIME   (:) = NEMIS_PGD_TIME   (1:CHE%NEMIS_NBR)
CHE%CEMIS_COMMENT(:) = CEMIS_PGD_COMMENT(1:CHE%NEMIS_NBR)
!
CHE%NTIME_MAX = MAXVAL(CHE%NEMIS_TIME)
!
!-------------------------------------------------------------------------------
OCH_EMIS = CHE%NEMIS_NBR > 0
!-------------------------------------------------------------------------------
!
!*    4.      Computations
!             ------------
!
DO JNBR=1,CHE%NEMIS_NBR
  CATYPE = CEMIS_PGD_ATYPE(JNBR)
  SELECT CASE (CHE%CEMIS_AREA(JNBR))
    CASE ('LAN')
      IL = IL_LAND
      YMASK='LAND  '
    CASE ('SEA')
      IL = IL_SEA
      YMASK='SEA   '
    CASE ('ALL')
      IL = NL
      YMASK='FULL  '
    CASE DEFAULT
      CALL ABOR1_SFX('PGD_CHEMISTRY (1): EMISSION AREA NOT SUPPORTED')
  END SELECT
  ALLOCATE(ZEMIS_FIELD (IL))
  ALLOCATE(IMASK(IL))
  !*    4.1     Computes the field on the surface points where it is defined
  CALL PGD_FIELD(DTCO, UG, U, USS, &
                 HPROGRAM,CHE%CEMIS_NAME(JNBR),CHE%CEMIS_AREA(JNBR),CEMIS_PGD_FILE(JNBR), &
                   CEMIS_PGD_FILETYPE(JNBR),XUNDEF,ZEMIS_FIELD(:)             )  
  CATYPE = 'ARI'
  
!*    4.2     Expends field on all surface points
  ILU=0
  CALL GET_SURF_MASK_n(DTCO, U, &
                       YMASK,IL,IMASK,ILU,ILUOUT)
  CALL UNPACK_SAME_RANK(IMASK,ZEMIS_FIELD(:),ZEMIS_FIELDS(:))
  DEALLOCATE(ZEMIS_FIELD)
  DEALLOCATE(IMASK)

  
!*    4.3      Weights field on all surface points 
!              (zero weight where field is not defined)
  SELECT CASE (CHE%CEMIS_AREA(JNBR))
    CASE ('LAN')
      CHE%XEMIS_FIELDS(:,JNBR) = (U%XNATURE(:)+U%XTOWN(:))*ZEMIS_FIELDS(:) 
    CASE ('SEA')
      CHE%XEMIS_FIELDS(:,JNBR) = U%XSEA*ZEMIS_FIELDS(:)
    CASE ('ALL')
      CHE%XEMIS_FIELDS(:,JNBR) = ZEMIS_FIELDS(:)
    CASE DEFAULT
      CALL ABOR1_SFX('PGD_CHEMISTRY (2): EMISSION AREA NOT SUPPORTED')
  END SELECT
END DO
DEALLOCATE(ZEMIS_FIELDS)
IF (LHOOK) CALL DR_HOOK('PGD_CHEMISTRY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    5.      Expends 
!             ------------
!

!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_CHEMISTRY
