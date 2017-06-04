!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODI_AV_PGD
!     ##################
INTERFACE AV_PGD
!
      SUBROUTINE AV_PGD_2D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_2D
!
      SUBROUTINE AV_PATCH_PGD (DTCO, &
                               PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD
!
      SUBROUTINE AV_PGD_1D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
  LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_1D
!
      SUBROUTINE AV_PATCH_PGD_1D (DTCO, &
                                  PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD_1D
!
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,OCOVER,KDECADE)
      
!
USE MODD_TYPE_DATE_SURF
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN) :: TDATA  ! secondary field to construct for each patch
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE MAJOR_PATCH_PGD_1D
!

!
END INTERFACE
END MODULE MODI_AV_PGD
!
!     ################################################################
      SUBROUTINE AV_PGD_1D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_BLD_HEIGHT 
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
!            
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
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JI, ID0
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: ISIZE_OMP
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZWORK, ZDZ, ZVAL
REAL, DIMENSION(SIZE(PCOVER,2)) :: ZWEIGHT
REAL :: ZCOVER_WEIGHT
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',1,ZHOOK_HANDLE)
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
CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',1,ZHOOK_HANDLE)
!
IF (HATYPE=='ARI' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
  !
  ISIZE_OMP = MAX(1,ICOVER/NBLOCKTOT)
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP) 
 CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_2',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(JCOVER,JJ,ZVAL,JI,ZCOVER_WEIGHT) &
!!$OMP & REDUCTION(+:ZSUM_COVER_WEIGHT,ZWORK)
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
      DO JI = 1,SIZE(PCOVER,1)
        IF (PCOVER(JI,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT = PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER)
          ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + ZCOVER_WEIGHT
          ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
      ENDDO
      !
    ENDIF
  ENDDO
!!$OMP END DO
 CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_2',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
ELSEIF (HATYPE=='MAJ') THEN
  !
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,ID0)
  DO JI = 1,SIZE(PCOVER,1) 
    ID0 = MAXVAL(MAXLOC(PCOVER(JI,:)*ZWEIGHT(:)))
    ZWORK(JI) = PDATA(IMASK(ID0))
    ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + SUM(PCOVER(JI,:)*ZWEIGHT(:))
  ENDDO
!!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_3',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
  !
ELSE
  CALL ABOR1_SFX('AV_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_4',0,ZHOOK_HANDLE)
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
    CALL ABOR1_SFX('AV_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_4',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_1D
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D (DTCO, &
                                  PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_VEG, XDATA_LAI
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVEGTYPE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
!
USE MODI_VEGTYPE_TO_PATCH 
USE MODE_AV_PGD
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
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN)  :: OCOVER
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
INTEGER :: JVEGTYPE! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JPATCH  ! PATCH index
INTEGER :: JJ, JI, JK
!
REAL         :: ZCOVER_WEIGHT
!
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK0
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZVAL
!
REAL, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)         :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZSUM_COVER_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZDZ
!
INTEGER, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))  :: IMASK
INTEGER, DIMENSION(SIZE(PFIELD,2)) :: JCOUNT
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
IPATCH=SIZE(PFIELD,2)
!
IF (PRESENT(PDZ)) THEN
  DO JPATCH=1,IPATCH
      ZDZ(:,JPATCH)=PDZ(:)
  END DO
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:) = 0.
ZWEIGHT(:,:) = 0.0
ZSUM_COVER_WEIGHT_PATCH(:,:) = 0.
!
DO JVEGTYPE=1,NVEGTYPE
  PATCH_LIST(JVEGTYPE) = VEGTYPE_TO_PATCH (JVEGTYPE, IPATCH)
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
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_2',0,ZHOOK_HANDLE)
!
 CALL GET_WEIGHT_PATCH(DTCO,ICOVER,IMASK0,KDECADE,HSFTYPE,ZWEIGHT)
 !
!
!-------------------------------------------------------------------------------
  !
  !
  !*    2.     Selection of the weighting function for vegtype
  !            -----------------------------------
  !
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_2',1,ZHOOK_HANDLE)
!
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP) REDUCTION(+:ZSUM_COVER_WEIGHT_PATCH,ZWORK)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_3',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JCOVER,JJ,JVEGTYPE,JPATCH, &
!!$OMP   ZVAL,JI,ZCOVER_WEIGHT)
DO JCOVER=1,ICOVER
  !
  JJ = IMASK0(JCOVER)
  !
  DO JVEGTYPE=1,NVEGTYPE
    !
    JPATCH= PATCH_LIST(JVEGTYPE)  
    !
    IF (ZWEIGHT(JCOVER,JVEGTYPE)/=0.) THEN
      !
      IF (HATYPE=='ARI') THEN
        ZVAL(:) = PDATA(JJ,JVEGTYPE)
      ELSEIF (HATYPE=='INV') THEN
        ZVAL(:) = 1. / PDATA(JJ,JVEGTYPE)
      ELSEIF (HATYPE=='CDN') THEN
        DO JI=1,SIZE(PCOVER,1)
          ZVAL(JI) = 1./(LOG(ZDZ(JI,JPATCH)/PDATA(JJ,JVEGTYPE)))**2 
        ENDDO
      ELSE
        CALL ABOR1_SFX('AV_PATCH_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED')
      ENDIF
      !
      DO JI=1,SIZE(PCOVER,1)
        IF (PCOVER(JI,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT =  PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER,JVEGTYPE)      
          ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH) = ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH) + ZCOVER_WEIGHT
          ZWORK(JI,JPATCH) = ZWORK(JI,JPATCH) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
      ENDDO
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_3',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_4',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
IMASK(:,:)=0
JCOUNT(:)=0
DO JPATCH=1,IPATCH
  DO JI=1,SIZE(PCOVER,1)
    IF ( ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH) >0.) THEN
      JCOUNT(JPATCH)=JCOUNT(JPATCH)+1
      IMASK(JCOUNT(JPATCH),JPATCH)=JI
    ENDIF
  ENDDO
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
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
          JI = IMASK(JJ,JPATCH)
          PFIELD(JI,JPATCH) =  ZWORK(JI,JPATCH) / ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH)
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
        JI = IMASK(JJ,JPATCH)
        PFIELD(JI,JPATCH) = ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH) / ZWORK(JI,JPATCH)
      ENDDO
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JPATCH=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JPATCH)
        JI = IMASK(JJ,JPATCH)
        PFIELD(JI,JPATCH) = ZDZ(JI,JPATCH) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(JI,JPATCH)/ZWORK(JI,JPATCH)) )
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_4',1,ZHOOK_HANDLE)
!
!IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PATCH_PGD_1D
!
!     ################################################################
      SUBROUTINE AV_PGD_2D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
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
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
 LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZWORK, ZDZ
REAL                                           :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZCOVER_WEIGHT
REAL                                           :: ZDATA
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(OCOVER)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:,:)=PDZ(:,:)
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:)=0.
ZSUM_COVER_WEIGHT(:,:)=0.
!-------------------------------------------------------------------------------
JCOVER = 0
DO JJ=1,ICOVER
  !
  IF (.NOT.OCOVER(JJ)) CYCLE
  !
  JCOVER = JCOVER + 1
  !
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)
       CASE('ALL')
         ZWEIGHT=1.

       CASE('NAT')
         ZWEIGHT=DTCO%XDATA_NATURE(JJ)

       CASE('GRD')
         ZWEIGHT=DTCO%XDATA_TOWN (JJ) * DTCO%XDATA_GARDEN(JJ)

       CASE('TWN')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ)

       CASE('WAT')
         ZWEIGHT=DTCO%XDATA_WATER (JJ)

       CASE('SEA')
         ZWEIGHT=DTCO%XDATA_SEA   (JJ)

       CASE('BLD')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ) *        DTCO%XDATA_BLD(JJ)

       CASE('STR')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ) * ( 1. - DTCO%XDATA_BLD(JJ) )

       CASE('TRE')
         PFIELD(:,:)=0.
         ZWEIGHT=DTCO%XDATA_NATURE(JJ) * (  DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TENE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BOND) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB) &                                           
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BONE) )  

       CASE('GRT')
         PFIELD(:,:)=0.
         ZWEIGHT=DTCO%XDATA_TOWN (JJ) * DTCO%XDATA_GARDEN(JJ)  &
                          * (  DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)  &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)  &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TENE) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BOND) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB) &                             
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BONE) )  

       CASE DEFAULT
         CALL ABOR1_SFX('AV_PGD: WEIGHTING FUNCTION NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
  ZCOVER_WEIGHT(:,:) = PCOVER(:,:,JCOVER) * ZWEIGHT
!
  ZSUM_COVER_WEIGHT(:,:) = ZSUM_COVER_WEIGHT(:,:) + ZCOVER_WEIGHT(:,:)
!
  ZDATA = PDATA(JJ)
!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.4    Arithmetic averaging
!            --------------------
!
  CASE ('ARI')
!
    ZWORK(:,:) = ZWORK(:,:) + ZDATA * ZCOVER_WEIGHT(:,:) 
!
!-------------------------------------------------------------------------------
!
!*    3.5    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    ZWORK (:,:)= ZWORK(:,:) + 1./ZDATA * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------!
!
!*    3.6    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    ZWORK (:,:)= ZWORK(:,:) + 1./(LOG(ZDZ(:,:)/ZDATA))**2 * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!
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
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZWORK(:,:) / ZSUM_COVER_WEIGHT(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZSUM_COVER_WEIGHT(:,:) / ZWORK(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZDZ(:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:,:)/ZWORK(:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_2D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_2D
!
!
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD (DTCO, &
                               PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_VEG, XDATA_LAI  
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVEGTYPE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
!
USE MODI_VEGTYPE_TO_PATCH 
!
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
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
 LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEGTYPE! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JPATCH  ! PATCH index
!
REAL, DIMENSION(NVEGTYPE)                                    :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),NVEGTYPE)      :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZCOVER_WEIGHT_PATCH
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZSUM_COVER_WEIGHT_PATCH
REAL, DIMENSION(NVEGTYPE)                                    :: ZDATA
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3)):: ZDZ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(OCOVER)
IPATCH=SIZE(PFIELD,3)
!
!
!
IF (PRESENT(PDZ)) THEN
  DO JPATCH=1,IPATCH
    ZDZ(:,:,JPATCH)=PDZ(:,:)
  END DO
ELSE
  ZDZ(:,:,:)=XCDREF
END IF
!
PFIELD(:,:,:)=XUNDEF
!
ZWORK(:,:,:)=0.
ZSUM_COVER_WEIGHT_PATCH(:,:,:)=0.
!
!-------------------------------------------------------------------------------
JCOVER = 0
DO JJ=1,ICOVER
  !
  IF (.NOT.OCOVER(JJ)) CYCLE
  !
  JCOVER = JCOVER + 1
  !
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)

     CASE('NAT')
       DO JVEGTYPE=1,NVEGTYPE
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)
       END DO

     CASE('GRD')
       DO JVEGTYPE=1,NVEGTYPE
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)
       END DO

     CASE('VEG')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             XDATA_VEG(JJ,KDECADE,JVEGTYPE)  
       END DO

     CASE('BAR')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             (1.-XDATA_VEG(JJ,KDECADE,JVEGTYPE)) 
       END DO

     CASE('GRV')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             XDATA_VEG(JJ,KDECADE,JVEGTYPE)  
       END DO

     CASE('GRB')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             (1.-XDATA_VEG(JJ,KDECADE,JVEGTYPE))
       ENDDO 
       
     CASE('DVG') ! average only on vegetated area
       ZWEIGHT(:) = 0.0
       DO JVEGTYPE=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JJ,:,JVEGTYPE)).GT.0.) &
           ZWEIGHT(JVEGTYPE)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)
       END DO     

     CASE('GDV') ! average only on vegetated area
       ZWEIGHT(:) = 0.0             
       DO JVEGTYPE=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JJ,:,JVEGTYPE)).GT.0.) &
           ZWEIGHT(JVEGTYPE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)
       END DO     

     CASE('LAI')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             XDATA_LAI(JJ,KDECADE,JVEGTYPE)  
       END DO

     CASE('GRL')
       DO JVEGTYPE=1,NVEGTYPE  
         ZWEIGHT(JVEGTYPE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEGTYPE)*&
                             XDATA_LAI(JJ,KDECADE,JVEGTYPE)  
       END DO

      CASE('TRE')
        ZWEIGHT(:)=0.
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)>0.) THEN
          ZWEIGHT(NVT_TEBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)>0.) THEN
          ZWEIGHT(NVT_BONE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)>0.) THEN
          ZWEIGHT(NVT_TRBE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)>0.) THEN
          ZWEIGHT(NVT_TRBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)>0.) THEN
          ZWEIGHT(NVT_TEBE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)>0.) THEN
          ZWEIGHT(NVT_TENE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)>0.) THEN
          ZWEIGHT(NVT_BOBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)>0.) THEN
          ZWEIGHT(NVT_BOND)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)>0.) THEN
          ZWEIGHT(NVT_SHRB)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)
        END IF

      CASE('GRT')
        ZWEIGHT(:)=0.
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)>0.) THEN
          ZWEIGHT(NVT_TEBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)>0.) THEN
          ZWEIGHT(NVT_BONE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)>0.) THEN
          ZWEIGHT(NVT_TRBE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)>0.) THEN
          ZWEIGHT(NVT_TRBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)>0.) THEN
          ZWEIGHT(NVT_TEBE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)>0.) THEN
          ZWEIGHT(NVT_TENE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)>0.) THEN
          ZWEIGHT(NVT_BOBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)>0.) THEN
          ZWEIGHT(NVT_BOND)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)>0.) THEN
          ZWEIGHT(NVT_SHRB)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)
        END IF

      CASE DEFAULT
         CALL ABOR1_SFX('AV_PATCH_PGD: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
! 
  ZCOVER_WEIGHT(:,:,:)=0. 
  ZCOVER_WEIGHT_PATCH(:,:,:)=0.
 
  DO JVEGTYPE=1,NVEGTYPE
     ZCOVER_WEIGHT(:,:,JVEGTYPE) =  ZCOVER_WEIGHT(:,:,JVEGTYPE) +&
                                      PCOVER(:,:,JCOVER) * ZWEIGHT(JVEGTYPE)    

     JPATCH= VEGTYPE_TO_PATCH (JVEGTYPE, IPATCH)
    
     ZCOVER_WEIGHT_PATCH(:,:,JPATCH) =  ZCOVER_WEIGHT_PATCH(:,:,JPATCH)+   &
                                          PCOVER(:,:,JCOVER) * ZWEIGHT(JVEGTYPE)  
  END DO 

!
  ZSUM_COVER_WEIGHT_PATCH(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) + ZCOVER_WEIGHT_PATCH(:,:,:)


  ZDATA(:) = PDATA(JJ,:)

!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.3    Arithmetic averaging
!            --------------------
!
    CASE ('ARI')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH= VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
        ZWORK(:,:,JPATCH) =  ZWORK(:,:,JPATCH) + ZDATA(JVEGTYPE) * ZCOVER_WEIGHT(:,:,JVEGTYPE)
      END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
    CASE('INV' )
!
     DO JVEGTYPE=1,NVEGTYPE 
       JPATCH=VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
       ZWORK(:,:,JPATCH)= ZWORK(:,:,JPATCH) + 1./ ZDATA(JVEGTYPE)* ZCOVER_WEIGHT(:,:,JVEGTYPE)
     END DO    
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
    CASE('CDN')
!
      DO JVEGTYPE=1,NVEGTYPE
        JPATCH=VEGTYPE_TO_PATCH (JVEGTYPE,IPATCH)
        ZWORK(:,:,JPATCH)= ZWORK(:,:,JPATCH) + 1./(LOG(ZDZ(:,:,JPATCH)/ ZDATA(JVEGTYPE)))**2    &
                                * ZCOVER_WEIGHT(:,:,JVEGTYPE)  
      END DO   
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
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
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) =  ZWORK(:,:,:) / ZSUM_COVER_WEIGHT_PATCH(:,:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) / ZWORK(:,:,:)
    END WHERE
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZDZ(:,:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(:,:,:)/ZWORK(:,:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PATCH_PGD
!
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,OCOVER,KDECADE)
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
!
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
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: JVEGTYPE! loop on vegtype
!
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)      :: IDATA_DOY
INTEGER, DIMENSION(SIZE(PCOVER,1))               :: IDOY
REAL,    DIMENSION(365)                          :: ZCOUNT
INTEGER                                          :: JP, IMONTH, IDAY
INTEGER                                          :: IPATCH, JPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',0,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
IPATCH=SIZE(TFIELD,2)
!
TFIELD(:,:)%TDATE%YEAR  = NUNDEF
TFIELD(:,:)%TDATE%MONTH = NUNDEF
TFIELD(:,:)%TDATE%DAY   = NUNDEF
TFIELD(:,:)%TIME        = XUNDEF
!
IDOY(:) = 0
!
 CALL DATE2DOY(TDATA,OCOVER,IDATA_DOY)
!-------------------------------------------------------------------------------
DO JP = 1,SIZE(PCOVER,1)
  !
  DO JPATCH=1,IPATCH
    !
    ZCOUNT(:) = 0.
    !
    DO JVEGTYPE=1,NVEGTYPE
      !
      IF(JPATCH==VEGTYPE_TO_PATCH(JVEGTYPE,IPATCH)) THEN
        !
        DO JCOVER = 1,SIZE(PCOVER,2)
          !
          IF (IDATA_DOY(JCOVER,JVEGTYPE) /= NUNDEF .AND. PCOVER(JP,JCOVER)/=0.) THEN
            !
            ZCOUNT(IDATA_DOY(JCOVER,JVEGTYPE)) = ZCOUNT(IDATA_DOY(JCOVER,JVEGTYPE)) + PCOVER(JP,JCOVER)
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
    TFIELD(JP,JPATCH)%TDATE%MONTH = IMONTH
    TFIELD(JP,JPATCH)%TDATE%DAY   = IDAY
    IF (IMONTH/=NUNDEF) TFIELD(JP,JPATCH)%TIME   = 0.
    !
  END DO
  !
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAJOR_PATCH_PGD_1D
!-------------------------------------------------------------------------------
!
