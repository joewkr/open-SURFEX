!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
MODULE MODI_INI_VAR_FROM_DATA
!
INTERFACE INI_VAR_FROM_DATA
!
      MODULE PROCEDURE INI_VAR_FROM_DATA_NAT_1D
      MODULE PROCEDURE INI_VAR_FROM_DATA_1D
      MODULE PROCEDURE INI_VAR_FROM_DATA_NAT_2D
      MODULE PROCEDURE INI_VAR_FROM_DATA_2D
!
END INTERFACE INI_VAR_FROM_DATA
!
CONTAINS
!
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_NAT_1D (DTCO, UG, U, USS, PPAR_VEGTYPE, &
                                       HPROGRAM, HATYPE, HNAME ,HTYPE, HFNAM, HFTYP, PUNIF, PFIELD, OPRESENT)
!     ##############################################################
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
REAL, DIMENSION(:,:), INTENT(IN) :: PPAR_VEGTYPE
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), DIMENSION(:), INTENT(INOUT) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:), INTENT(INOUT) :: HFTYP
REAL, DIMENSION(:), INTENT(INOUT) :: PUNIF
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD
LOGICAL, DIMENSION(:), INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(PPAR_VEGTYPE,1)) :: ZMASK
 CHARACTER(LEN=40) :: YNAME
INTEGER            :: JV, JV2  ! loop counter on vegtypes
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_NAT_1D',0,ZHOOK_HANDLE)
!
OPRESENT(:) = .FALSE.
!
YNAME=ADJUSTL(HNAME)
!
IF (HFTYP(1)=='DIRTYP') THEN

  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(1), &
                            HFTYP(1), PUNIF(1), PFIELD, OPRESENT(1), PPAR_VEGTYPE)

  OPRESENT(2:) = OPRESENT(1)


ELSE

  IF (.NOT.ALL(LEN_TRIM(HFNAM(:))/=0) .AND. .NOT.ALL(LEN_TRIM(HFNAM(2:))==0)) THEN
    DO JV=1,SIZE(PFIELD,2)
      IF (LEN_TRIM(HFNAM(JV))==0) THEN
        DO JV2=JV-1,1,-1
          IF (LEN_TRIM(HFNAM(JV2))/=0) THEN
            HFNAM(JV) = HFNAM(JV2)
            HFTYP(JV) = HFTYP(JV2)
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDIF

  DO JV=1,SIZE(PFIELD,2)

    IF (ALL(LEN_TRIM(HFNAM(2:))==0)) THEN
      ZMASK(:) = 1.
    ELSE
      ZMASK(:) = PPAR_VEGTYPE(:,JV)
    ENDIF

    CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JV), &
                HFTYP(JV), PUNIF(JV), PFIELD(:,JV), OPRESENT(JV), ZMASK)
  ENDDO

ENDIF
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_NAT_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_NAT_1D
!
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_1D (DTCO, UG, U, USS, &
                                       HPROGRAM, HATYPE, HNAME ,HTYPE, HFNAM, HFTYP, PUNIF, PFIELD, OPRESENT)
!     ##############################################################
!
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    16/11/10
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), DIMENSION(:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:), INTENT(INOUT) :: HFTYP
REAL, DIMENSION(:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
 CHARACTER(LEN=40) :: YNAME
LOGICAL, DIMENSION(SIZE(PFIELD,2)) :: LPRESENT
INTEGER               :: JV, JJ  ! loop counter on vegtypes
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_1D',0,ZHOOK_HANDLE)
!
OPRESENT=.FALSE.
YNAME=ADJUSTL(HNAME)
!
DO JV=1,SIZE(PFIELD,2)
  CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JV), &
              HFTYP(JV), PUNIF(JV), PFIELD(:,JV), LPRESENT(JV))
ENDDO
!
IF (ANY(LPRESENT(:))) THEN

  OPRESENT=.TRUE.

  IF (.NOT.ALL(LPRESENT)) THEN
    CALL ABOR1_SFX("INI_VAR_FROM_DATA_1D: MISSING INPUT DATA FOR "//HNAME)
  ENDIF

ENDIF
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_1D
!
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_NAT_2D (DTCO, UG, U, USS, PPAR_VEGTYPE, &
                                       HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM, HFTYP, PUNIF, PFIELD_TIME, OPRESENT)
!     ##############################################################
!
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PGDWORK, ONLY : NSIZE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
REAL, DIMENSION(:,:), INTENT(IN) :: PPAR_VEGTYPE
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), DIMENSION(:,:), INTENT(INOUT) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:,:), INTENT(INOUT) :: HFTYP
REAL, DIMENSION(:,:), INTENT(INOUT) :: PUNIF
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD_TIME
LOGICAL, DIMENSION(:), INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(PPAR_VEGTYPE,1)) :: ZMASK
INTEGER               :: JV, JJ, JV2  ! loop counter on vegtypes
INTEGER               :: JTIME
INTEGER               :: ITIME, ISIZE_V, IDX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_NAT_2D',0,ZHOOK_HANDLE)
!
OPRESENT(:)=.FALSE.
ITIME=0
!
ISIZE_V = SIZE(PFIELD_TIME,3)
!
DO JTIME=1,SIZE(PFIELD_TIME,2)
!
  IF (HFTYP(1,JTIME)=='DIRTYP') THEN

    IF (SIZE(OPRESENT)>ISIZE_V) THEN
      IDX = (JTIME-1)*ISIZE_V+1
    ELSE
      IDX = 1
    ENDIF
    CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                              HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(1,JTIME), &
                              HFTYP(1,JTIME), PUNIF(1,JTIME), PFIELD_TIME(:,JTIME,:), &
                              OPRESENT(IDX), PPAR_VEGTYPE)
    OPRESENT(IDX+1:IDX+ISIZE_V-1) = OPRESENT(IDX)

  ELSE

    IF (.NOT.ALL(LEN_TRIM(HFNAM(:,JTIME))/=0) .AND. &
           COUNT(LEN_TRIM(HFNAM(:,JTIME))/=0)>1) THEN
      DO JV=1,SIZE(PFIELD_TIME,3)
        IF (LEN_TRIM(HFNAM(JV,JTIME))==0) THEN
          DO JV2=JV-1,1,-1
            IF (LEN_TRIM(HFNAM(JV2,JTIME))/=0) THEN
              HFNAM(JV,JTIME) = HFNAM(JV2,JTIME)
              HFTYP(JV,JTIME) = HFTYP(JV2,JTIME)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF

    DO JV=1,ISIZE_V

      IF (SIZE(OPRESENT)>ISIZE_V) THEN
        IDX = (JTIME-1)*ISIZE_V+JV
      ELSE
        IDX = JV
      ENDIF

      IF (ALL(LEN_TRIM(HFNAM(2:,JTIME))==0)) THEN
        ZMASK(:) = 1.
      ELSE
        ZMASK(:) = PPAR_VEGTYPE(:,JV)
      ENDIF

      CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS,                              &
                              HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JV,JTIME), &
                HFTYP(JV,JTIME), PUNIF(JV,JTIME), PFIELD_TIME(:,JTIME,JV),     &
                OPRESENT(IDX), ZMASK)
    ENDDO

  ENDIF

ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_NAT_2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_NAT_2D
!
!
!     #########
      SUBROUTINE INI_VAR_FROM_DATA_2D (DTCO, UG, U, USS, &
                                       HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM, HFTYP, PUNIF, PFIELD_TIME, OPRESENT)
!     ##############################################################
!
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!
!!    Original    16/11/10
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_PUT_IN_TIME
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=3), INTENT(IN) :: HATYPE
 CHARACTER(LEN=*), INTENT(IN) :: HNAME
 CHARACTER(LEN=3), INTENT(IN) :: HTYPE
 CHARACTER(LEN=28), DIMENSION(:,:), INTENT(IN) :: HFNAM
 CHARACTER(LEN=6), DIMENSION(:,:), INTENT(INOUT) :: HFTYP
REAL, DIMENSION(:,:), INTENT(IN) :: PUNIF
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD_TIME
LOGICAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL, DIMENSION(SIZE(PFIELD_TIME,3)) :: LPRESENT
LOGICAL, DIMENSION(SIZE(PFIELD_TIME,2)) :: LPRESENT_TIME
INTEGER               :: JV, JJ  ! loop counter on vegtypes
INTEGER               :: JTIME
INTEGER               :: ITIME
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_2D',0,ZHOOK_HANDLE)
!
OPRESENT=.FALSE.
LPRESENT_TIME(:)=.FALSE.
ITIME=0
!
DO JTIME=1,SIZE(PFIELD_TIME,2)

  DO JV=1,SIZE(PFIELD_TIME,3)

    CALL INI_VAR_FROM_DATA_0D(DTCO, UG, U, USS, &
                            HPROGRAM, HATYPE, HNAME, HTYPE, HFNAM(JV,JTIME), &
              HFTYP(JV,JTIME), PUNIF(JV,JTIME), PFIELD_TIME(:,JTIME,JV),&
              LPRESENT(JV))

  ENDDO

  IF (ANY(LPRESENT(:))) THEN

    LPRESENT_TIME(JTIME)=.TRUE.
    OPRESENT=.TRUE.
    ITIME=ITIME+1

    IF (.NOT.ALL(LPRESENT)) THEN
      CALL ABOR1_SFX("INI_VAR_FROM_DATA_2D: MISSING INPUT DATA FOR "//HNAME)
    ENDIF

  ENDIF

ENDDO
!
IF (OPRESENT) THEN
  IF (SIZE(PFIELD_TIME,2)==36) THEN
     CALL PUT_IN_TIME(HNAME,HTYPE,ITIME,36,PFIELD_TIME)
  ELSE
    IF (ANY(LPRESENT_TIME(:)) .AND. .NOT.ALL(LPRESENT_TIME(:))) &
      CALL ABOR1_SFX("INI_VAR_FROM_DATA_2D: MISSING INPUT DATA FOR "//HNAME)
  ENDIF
ENDIF
!
!
IF (LHOOK) &
  CALL DR_HOOK('MODI_INI_VAR_FROM_DATA:INI_VAR_FROM_DATA_2D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_DATA_2D
END MODULE MODI_INI_VAR_FROM_DATA
