!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE AV_PGD_PARAM (PLAI_IN, PVEG_IN, &
                               PFIELD,PVEGTYPE,PDATA,HSFTYPE,HATYPE,KMASK,KNPATCH,KPATCH,PDZ,KDECADE)
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
!!    R. Alkama   04/2012  add 6 new tree vegtype (9 instead 3)
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE,  &
                                NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, NVEGTYPE,  &
                                XCDREF

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
REAL, DIMENSION(:,:,:), INTENT(IN) :: PLAI_IN
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVEG_IN
!
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN) :: HATYPE  ! Type of averaging
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JV! loop on vegtype
INTEGER :: JJ, JI, JP, IMASK
!
REAL, DIMENSION(SIZE(PFIELD,1),NVEGTYPE)  :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZSUM_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZWORK
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZDZ
!
REAL, DIMENSION(31) :: ZCOUNT
INTEGER, DIMENSION(SIZE(PFIELD,1))  :: NMASK
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
!
ZWORK(:)=0.
ZWEIGHT(:,:)=0.
ZSUM_WEIGHT_PATCH(:)=0.
!
DO JV=1,NVEGTYPE
  PATCH_LIST(JV) = VEGTYPE_TO_PATCH (JV, KNPATCH)
ENDDO

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
DO JV=1,NVEGTYPE
  JP= PATCH_LIST(JV)
  IF (JP/=KPATCH) CYCLE
  DO JI=1,SIZE(PFIELD)
    IMASK = KMASK(JI)
    
    IF (HSFTYPE=='NAT'.OR.HSFTYPE=='GRD') THEN
      ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)
    ELSEIF (HSFTYPE=='VEG'.OR.HSFTYPE=='GRV') THEN
      ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)*PVEG_IN(IMASK,KDECADE,JV)
    ELSEIF (HSFTYPE=='BAR'.OR.HSFTYPE=='GRB') THEN
      ZWEIGHT(JI,JV)=PVEGTYPE(IMASK,JV)*(1.-PVEG_IN(IMASK,KDECADE,JV))
    ELSEIF (HSFTYPE=='DVG'.OR.HSFTYPE=='GDV') THEN
      IF (SUM(PLAI_IN(JI,:,JV)).GT.0.) ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)
    ELSEIF (HSFTYPE=='LAI'.OR.HSFTYPE=='GRL') THEN
      IF (JV>=4) ZWEIGHT(JI,JV)=PVEGTYPE(IMASK,JV)*PLAI_IN(IMASK,KDECADE,JV)
    ELSEIF (HSFTYPE=='TRE'.OR.HSFTYPE=='GRT') THEN
      IF (JV==NVT_TEBD.OR.JV==NVT_BONE.OR.JV==NVT_TRBE.OR.JV==NVT_TRBD.OR.&
          JV==NVT_TEBE.OR.JV==NVT_TENE.OR.JV==NVT_BOBD.OR.JV==NVT_BOND.OR.&
          JV==NVT_SHRB) ZWEIGHT(JI,JV) = PVEGTYPE(JI,JV)
    ELSE
      CALL ABOR1_SFX('AV_PGD_PARAM_1D: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
    ENDIF

  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
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
    DO JV=1,NVEGTYPE
      JP= PATCH_LIST(JV)
      IF (JP/=KPATCH) CYCLE
      DO JJ=1,SIZE(PFIELD)
        IMASK = KMASK(JJ)
        ZSUM_WEIGHT_PATCH(JJ) = ZSUM_WEIGHT_PATCH(JJ) + ZWEIGHT(JJ,JV)
        ZWORK(JJ) =  ZWORK(JJ) + PDATA(IMASK,JV)  * ZWEIGHT(JJ,JV)
      ENDDO
    END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
   DO JV=1,NVEGTYPE 
     JP=PATCH_LIST(JV) 
     IF (JP/=KPATCH) CYCLE
     DO JJ=1,SIZE(PFIELD)
       IMASK = KMASK(JJ)     
       ZSUM_WEIGHT_PATCH(JJ) = ZSUM_WEIGHT_PATCH(JJ)+ZWEIGHT(JJ,JV)
       IF (PDATA(IMASK,JV).NE.0.) THEN
         ZWORK(JJ)= ZWORK(JJ) + 1./ PDATA(IMASK,JV) * ZWEIGHT(JJ,JV)
       ENDIF
     ENDDO
   END DO
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JV=1,NVEGTYPE
      JP=PATCH_LIST(JV)
      IF (JP/=KPATCH) CYCLE
      DO JJ=1,SIZE(PFIELD)
        IMASK = KMASK(JJ)        
        ZSUM_WEIGHT_PATCH(JJ) =  ZSUM_WEIGHT_PATCH(JJ)+ ZWEIGHT(JJ,JV)
        IF (PDATA(JJ,JV).NE.0.) THEN
          ZWORK(JJ)= ZWORK(JJ) + 1./(LOG(ZDZ(JJ)/ PDATA(IMASK,JV)))**2    &
                            * ZWEIGHT(JJ,JV)
        ENDIF
      ENDDO
    END DO   
!
  CASE ('MAJ')
!
    ZWORK(:) = 0.
    DO JJ=1,SIZE(PFIELD)
      ZCOUNT(:) = 0.
      DO JV=1,NVEGTYPE
        JP= PATCH_LIST(JV)
        IF (JP/=KPATCH) CYCLE
        IMASK = KMASK(JJ)
        ZCOUNT(NINT(PDATA(IMASK,JV))) = ZCOUNT(NINT(PDATA(IMASK,JV))) + ZWEIGHT(JJ,JV)
      ENDDO
      ZWORK(JJ) = FLOAT(MAXLOC(ZCOUNT,1))
    END DO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM_1D: (1) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
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
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZWORK(JI) / ZSUM_WEIGHT_PATCH(JI)
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
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZSUM_WEIGHT_PATCH(JI) / ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) THEN
        PFIELD(JI) = ZDZ(JI) * EXP( - SQRT(ZSUM_WEIGHT_PATCH(JI)/ZWORK(JI)) )
      ENDIF
    ENDDO
!
  CASE ('MAJ')
!   
    DO JI=1,SIZE(PFIELD)
      PFIELD(JI) = ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PGD_PARAM
