!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODI_AV_PGD_1P
!     ##################
INTERFACE AV_PGD_1P
!
      SUBROUTINE AV_PGD_1D_1P (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,KMASK,KPATCH,KNPATCH,PDZ,KDECADE)
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:),  INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_1D_1P
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D_1P (DTCO, &
                                  PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,KMASK,KNPATCH,KPATCH,PDZ,KDECADE)
!     ################################################################
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN)  :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D_1P(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,&
                      OCOVER,KMASK,KNPATCH,KPATCH,KDECADE)
!     ################################################################
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_DATE_SURF
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE MAJOR_PATCH_PGD_1D_1P
!
END INTERFACE
END MODULE MODI_AV_PGD_1P
!
!     ################################################################
      SUBROUTINE AV_PGD_1D_1P (DTCO, PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,&
                               KMASK, KPATCH, KNPATCH, PDZ, KDECADE)
!     ################################################################
!
!!**** *AV_PGD* average a secondary physiographic variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
!!
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
!
!     F.Solmon patch modif: remove the case 'veg' as veg is defined for patches 
!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!    R. Alkama   05/2012  Add 6 tree vegtypes (9 rather than 3)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : XCDREF
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_AV_PGD
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
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:),  INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JI, ID0, IMASK0
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK
REAL, DIMENSION(SIZE(PFIELD)) :: ZWORK, ZDZ, ZVAL
REAL, DIMENSION(SIZE(PCOVER,2)) :: ZWEIGHT
REAL :: ZCOVER_WEIGHT
REAL, DIMENSION(SIZE(PFIELD)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
IF (HSFTYPE=='TRE' .OR. HSFTYPE=='GRT') PFIELD(:) = 0.
!
ZWORK(:)=0.
ZSUM_COVER_WEIGHT(:)=0.
!
JCOVER = 0
DO JJ = 1,SIZE(OCOVER)
  IF (OCOVER(JJ)) THEN
    JCOVER=JCOVER+1
    IMASK(JCOVER) = JJ
  ENDIF
ENDDO
!
 CALL GET_WEIGHT(DTCO,ICOVER,IMASK,HSFTYPE,ZWEIGHT)
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
IF (HATYPE=='ARI' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
  !
  DO JCOVER=1,ICOVER
    IF (ZWEIGHT(JCOVER)/=0.) THEN
      !
      JJ = IMASK(JCOVER)
      !
      IF (HATYPE=='ARI') THEN
        ZVAL(:) = PDATA(JJ)
      ELSEIF (HATYPE=='INV') THEN
        ZVAL(:) = 1./PDATA(JJ)
      ELSEIF (HATYPE=='CDN') THEN
        ZVAL(:) = 1./(LOG(ZDZ(:)/PDATA(JJ)))**2 
      ENDIF
      !
      DO JI = 1,SIZE(KMASK)
        !
        IMASK0 = KMASK(JI)
        !
        IF (PCOVER(IMASK0,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT = PCOVER(IMASK0,JCOVER) * ZWEIGHT(JCOVER)
          ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + ZCOVER_WEIGHT
          ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
        !
      ENDDO
      !
    ENDIF
  ENDDO
ELSEIF (HATYPE=='MAJ') THEN
  !
  DO JI = 1,SIZE(KMASK)
    !
    IMASK0 = KMASK(JI)
    !
    ID0 = MAXVAL(MAXLOC(PCOVER(IMASK0,:)*ZWEIGHT(:)))
    ZWORK(JI) = PDATA(IMASK(ID0))
    ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + SUM(PCOVER(IMASK0,:)*ZWEIGHT(:))
    !
  ENDDO
  !
ELSE
  CALL ABOR1_SFX('AV_PGD_1D_1P: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:) / ZSUM_COVER_WEIGHT(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZSUM_COVER_WEIGHT(:) / ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZDZ(:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:)/ZWORK(:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.4    Majoritary averaging
!            --------------------
!
  CASE('MAJ' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D_1P: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P_4',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D_1P (DTCO, PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,KMASK,&
                                     KNPATCH,KPATCH,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PATCH_PGD* average for each surface patch a secondary physiographic 
!!                    variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
!!
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
!!    F.Solmon /V. Masson       
!!
!!    MODIFICATION
!!    ------------

!!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!    R. Alkama   05/2012  Add 6 tree vegtypes (9 rather than 3)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, XCDREF
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_AV_PGD
!
USE MODI_VEGTYPE_TO_PATCH 
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
!
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN)  :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEG! loop on vegtype
INTEGER :: JJ, JI, JK
!
REAL         :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PFIELD)) :: ZVAL
!
REAL, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)         :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PFIELD))   :: ZSUM_COVER_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PFIELD))   :: ZWORK
REAL, DIMENSION(SIZE(PFIELD))   :: ZDZ
!
INTEGER  :: IMASK, JP
INTEGER, DIMENSION(SIZE(PCOVER,2))  :: IMASK0
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PATCH_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
!
ZWORK(:) = 0.
ZWEIGHT(:,:) = 0.0
ZSUM_COVER_WEIGHT_PATCH(:) = 0.
!
DO JVEG=1,NVEGTYPE
  PATCH_LIST(JVEG) = VEGTYPE_TO_PATCH (JVEG, KNPATCH)
ENDDO
!
JCOVER = 0
DO JJ = 1,SIZE(OCOVER)
  IF (OCOVER(JJ)) THEN
    JCOVER=JCOVER+1
    IMASK0(JCOVER) = JJ
  ENDIF
ENDDO
!
 CALL GET_WEIGHT_PATCH(DTCO,ICOVER,IMASK0,KDECADE,HSFTYPE,ZWEIGHT)
!
!-------------------------------------------------------------------------------
  !
  !
  !*    2.     Selection of the weighting function for vegtype
  !            -----------------------------------
  !
JCOVER=0
!
DO JCOVER=1,ICOVER
  !
  JJ = IMASK0(JCOVER)
  !
  DO JVEG=1,NVEGTYPE
    !
    JP= PATCH_LIST(JVEG)
    IF (JP/=KPATCH) CYCLE
    !
    IF (ZWEIGHT(JCOVER,JVEG)/=0.) THEN
      !
      IF (HATYPE=='ARI') THEN
        ZVAL(:) = PDATA(JJ,JVEG)
      ELSEIF (HATYPE=='INV') THEN
        ZVAL(:) = 1. / PDATA(JJ,JVEG)
      ELSEIF (HATYPE=='CDN') THEN
        DO JI=1,SIZE(ZVAL)
          ZVAL(JI) = 1./(LOG(ZDZ(JI)/PDATA(JJ,JVEG)))**2 
        ENDDO
      ELSE
        CALL ABOR1_SFX('AV_1PATCH_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED')
      ENDIF
      !
      DO JI=1,SIZE(PFIELD)

        IMASK = KMASK(JI)

        IF (PCOVER(IMASK,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JVEG)      
          ZSUM_COVER_WEIGHT_PATCH(JI) = ZSUM_COVER_WEIGHT_PATCH(JI) + ZCOVER_WEIGHT
          ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
      ENDDO
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
!-------------------------------------------------------------------------------
  
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!   
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) =  ZWORK(JI) / ZSUM_COVER_WEIGHT_PATCH(JI)
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZSUM_COVER_WEIGHT_PATCH(JI) / ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZDZ(JI) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(JI)/ZWORK(JI)) )
    ENDDO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_1PATCH_PGD_1D_1P: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_1PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PATCH_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D_1P(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,&
                      OCOVER,KMASK,KNPATCH,KPATCH,KDECADE)
!     ################################################################
!
!!**** *MAJOR_PATCH_PGD* find the dominant date for each vegetation type
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
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
!!    P. LE MOIGNE
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2006
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_DATE_SURF
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_VEGTYPE_TO_PATCH
USE MODE_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, IMASK
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: JVEG! loop on vegtype
!
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)      :: IDATA_DOY
INTEGER, DIMENSION(SIZE(PCOVER,1))               :: IDOY
REAL,    DIMENSION(365)                          :: ZCOUNT
INTEGER                                          :: JP, IMONTH, IDAY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
TFIELD(:)%TDATE%YEAR  = NUNDEF
TFIELD(:)%TDATE%MONTH = NUNDEF
TFIELD(:)%TDATE%DAY   = NUNDEF
TFIELD(:)%TIME        = XUNDEF
!
IDOY(:) = 0
!
 CALL DATE2DOY(TDATA,OCOVER,IDATA_DOY)
!-------------------------------------------------------------------------------
DO JP = 1,SIZE(TFIELD)
  !
  IMASK = KMASK(JP)
  !
  ZCOUNT(:) = 0.
  !
  DO JVEG=1,NVEGTYPE
    !
    IF(KPATCH==VEGTYPE_TO_PATCH(JVEG,KNPATCH)) THEN
      !
      DO JCOVER = 1,SIZE(PCOVER,2)
        !
        IF (IDATA_DOY(JCOVER,JVEG) /= NUNDEF .AND. PCOVER(IMASK,JCOVER)/=0.) THEN
          !
          ZCOUNT(IDATA_DOY(JCOVER,JVEG)) = ZCOUNT(IDATA_DOY(JCOVER,JVEG)) + PCOVER(IMASK,JCOVER)
          !
        END IF
        !
      END DO
      !
    ENDIF
    !
  ENDDO
  !
  IDOY(JP) = 0
  IF (ANY(ZCOUNT(:)/=0.)) IDOY(JP) = MAXLOC(ZCOUNT,1)
  !
  CALL DOY2DATE(IDOY(JP),IMONTH,IDAY)
  !
  TFIELD(JP)%TDATE%MONTH = IMONTH
  TFIELD(JP)%TDATE%DAY   = IDAY
  IF (IMONTH/=NUNDEF) TFIELD(JP)%TIME   = 0.
  !
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAJOR_PATCH_PGD_1D_1P
!
!-------------------------------------------------------------------------------
!
