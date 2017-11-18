!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_WRITESURF_GR_SNOW
CONTAINS
      SUBROUTINE WRITESURF_GR_SNOW (OSNOWDIMNC, HSELECT, HPROGRAM, HSURFTYPE, &
                                    HPREFIX, KI, KMASK_P, KPATCH, TPSNOW, &
                                    PWSN_WR, PRHO_WR, PHEA_WR, PAGE_WR, PSG1_WR, &
                                    PSG2_WR, PHIS_WR, PALB_WR)
!     ##########################################################
!
!!****  *WRITESURF_GR_SNOW* - routine to write snow surface fields
!!
!!    PURPOSE
!!    -------
!       Writes snow surface fields
!
!!**  METHOD
!!    ------
!!
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      02/2003
!!     A. Bogatchev 09/2005 EBA snow option
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_SNOW
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT
!
USE MODD_WRITE_SURF_ATM, ONLY : LSPLIT_PATCH
!
USE MODI_WRITE_FIELD_2D_PATCH
USE MODI_WRITE_FIELD_1D_PATCH
USE MODI_DETECT_FIELD
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
LOGICAL, INTENT(IN) :: OSNOWDIMNC
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
 CHARACTER (LEN=6),  INTENT(IN) :: HPROGRAM   ! program
 CHARACTER (LEN=*),  INTENT(IN) :: HSURFTYPE  ! generic name used for
                                             ! snow characteristics
                                             ! storage in file
 CHARACTER (LEN=3),  INTENT(IN) :: HPREFIX    ! generic name of prefix for
                                             ! patch identification
INTEGER,            INTENT(IN)    :: KI      ! horizontal size of snow var.
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK_P
INTEGER,            INTENT(IN) :: KPATCH    ! number of tiles
TYPE(SURF_SNOW),    INTENT(IN) :: TPSNOW     ! snow characteristics
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PWSN_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRHO_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PHEA_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PAGE_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PSG1_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PSG2_WR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PHIS_WR
REAL, DIMENSION(:,:), INTENT(INOUT) :: PALB_WR
!
!*       0.2   declarations of local variables
!
 CHARACTER (LEN=100) :: YFMT           ! format for writing
 CHARACTER(LEN=12)   :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100)  :: YCOMMENT       ! Comment string
 CHARACTER(LEN=4)    :: YNLAYER        ! String depending on the number of layer : less
                                       !than 10 or more
 CHARACTER(LEN=3) :: YPAT
!
INTEGER             :: ISURFTYPE_LEN, IPAT_LEN, IFACT
INTEGER             :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER             :: JL, JP         ! loop counter
!
LOGICAL             :: GSNOW          ! T --> snow exists somewhere
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',0,ZHOOK_HANDLE)
!
!*       1.    Initialisation
!              --------------
!
ISURFTYPE_LEN = LEN_TRIM(HSURFTYPE)
!
JP = MAX(1,KPATCH)
!
IF (KPATCH<=1) THEN
  !
  !*       2.    Type of snow scheme
  !              -------------------
  !
  WRITE(YFMT,'(A5,I1,A4)') '(A3,A',ISURFTYPE_LEN,',A4)'
  WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE,'_TYP'
  YRECFM=ADJUSTL(HPREFIX//YRECFM)
  YCOMMENT = ' '
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,TPSNOW%SCHEME,IRESP,HCOMMENT=YCOMMENT)
  !
  !
  !*       3.    Number of layers
  !              ----------------
  !
  WRITE(YFMT,'(A5,I1,A4)') '(A3,A',ISURFTYPE_LEN,',A2)'
  WRITE(YRECFM,YFMT) 'SN_',HSURFTYPE,'_N'
  YRECFM=ADJUSTL(HPREFIX//YRECFM)
  YCOMMENT    = '(INTEGER)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,TPSNOW%NLAYER,IRESP,HCOMMENT=YCOMMENT)
  !
  !
  !*       4.    Tests to find if there is snow
  !              ------------------------------
  !
ENDIF
!
IF (KPATCH>0) THEN
  WRITE(YPAT,'(I2)') KPATCH
  YPAT = "P"//ADJUSTL(YPAT)
  IPAT_LEN = LEN_TRIM(ADJUSTL(YPAT))
ELSE
  YPAT = " "
  IPAT_LEN=1
ENDIF
!

IF (TPSNOW%NLAYER>0) THEN
  CALL DETECT_FIELD(HPROGRAM,TPSNOW%WSNOW(:,1:1),GSNOW)
ELSE
  GSNOW = .FALSE.
END IF
!
WRITE(YFMT,'(A5,I1,A2,I1,A1)') '(A3,A',ISURFTYPE_LEN,',A',IPAT_LEN,')'
WRITE(YRECFM,YFMT) 'SN_',ADJUSTL(HSURFTYPE(:LEN_TRIM(HSURFTYPE))),ADJUSTL(YPAT(:LEN_TRIM(YPAT)))
YRECFM=ADJUSTL(HPREFIX//YRECFM)
YCOMMENT    = '(LOGICAL)'
CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,GSNOW,IRESP,HCOMMENT=YCOMMENT)
!
!*       5.    Additional key
!              ---------------
!
IF (KPATCH==1) THEN
  YCOMMENT    = '(LOGICAL)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,'LSNOW_FRAC_T',LSNOW_FRAC_TOT,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF ( OSNOWDIMNC .AND. HPROGRAM=='OFFLIN' ) THEN
  !
  IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. &
      TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'WSN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A9,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_WSNOW',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%WSNOW(:,:),&
            KI,'snow_layer',PWSN_WR)
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'RSN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A9,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_RSNOW',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%RHO(:,:),&
            KI,'snow_layer',PRHO_WR)
    !
  ENDIF
  !
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'HSN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A9,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_HSNOW',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%HEAT(:,:),&
            KI,'snow_layer',PHEA_WR)
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'ASN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A8,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_SAGE',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%AGE(:,:),&
            KI,'snow_layer',PAGE_WR)
    !
  ENDIF
  !
  IF (TPSNOW%SCHEME=='CRO') THEN
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'SG1_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A7,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_SG1',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%GRAN1(:,:),&
            KI,'snow_layer',PSG1_WR)
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'SG2_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A7,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_SG2',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%GRAN2(:,:),&
            KI,'snow_layer',PSG2_WR)
    !
    WRITE(YFMT,'(A5,I1,A1)') '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'SHI_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A5,I1,A4)') '(A8,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_HIST',HSURFTYPE,' (kg/m2)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%HIST(:,:),&
            KI,'snow_layer',PHIS_WR)
    !
  ENDIF
  !
  IF (TPSNOW%SCHEME=='1-L') THEN
    !
    WRITE(YFMT,'(A5,I1,A1)')     '(A4,A',ISURFTYPE_LEN,')'
    WRITE(YRECFM,YFMT) 'TSN_',HSURFTYPE
    YRECFM=ADJUSTL(HPREFIX//YRECFM)
    WRITE(YFMT,'(A6,I1,A4)')     '(A10,A',ISURFTYPE_LEN,',A8)'
    WRITE(YCOMMENT,YFMT) 'X_Y_TSNOW',HSURFTYPE,' (K)'
    CALL  WRITE_FIELD_2D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%T(:,:),KI,'snow_layer')
    !
  END IF
  !
ELSE
  !
  DO JL = 1,TPSNOW%NLAYER
    !
    YNLAYER='I1.1'
    IF (JL>9) YNLAYER='I2.2'
    !
    IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. &
        TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
      !
      !*       6.    Snow reservoir
      !              --------------
      !
      WRITE(YFMT,'(A5,I1,A6)') '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'WSN_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)') '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_WSNOW_',HSURFTYPE,JL,' (kg/m2)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%WSNOW(:,JL),&
              KI,PWSN_WR(:,JL,:))
      !
      !*       7.    Snow density
      !              ------------
      !
      WRITE(YFMT,'(A5,I1,A6)') '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'RSN_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)') '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_RSNOW_',HSURFTYPE,JL,' (kg/m3)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%RHO(:,JL),&
              KI,PRHO_WR(:,JL,:))
      !
    END IF
    !
    IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
      !
      !*       9.    Heat content
      !              ------------
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'HSN_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_HSNOW_',HSURFTYPE,JL,' (J/m3)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%HEAT(:,JL),&
              KI,PHEA_WR(:,JL,:))
      !
      !*       10.    Age parameter
      !              ---------------
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SAG_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A9,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_SAGE_',HSURFTYPE,JL,' (-)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%AGE(:,JL),&
              KI,PAGE_WR(:,JL,:))
      !
    END IF
    !
    IF (TPSNOW%SCHEME=='CRO') THEN
      !
      !*       11.    Snow Gran1
      !              ----------
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SG1_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A11,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_SGRAN1_',HSURFTYPE,JL,' (-)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%GRAN1(:,JL),&
              KI,PSG1_WR(:,JL,:))
      !
      !*       11.    Snow Gran2
      !              ----------
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SG2_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A11,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_SGRAN2_',HSURFTYPE,JL,' (-)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%GRAN2(:,JL),&
              KI,PSG2_WR(:,JL,:))
      !
      !*       13.   Historical parameter
      !              -------------------
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'SHI_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_SHIST_',HSURFTYPE,JL,' (-)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%HIST(:,JL),&
              KI,PHIS_WR(:,JL,:))
      !
    ENDIF
    !
    !*       8.    Snow temperature
    !              ----------------
    !
    IF (TPSNOW%SCHEME=='1-L') THEN
      !
      WRITE(YFMT,'(A5,I1,A6)')     '(A4,A',ISURFTYPE_LEN,','//YNLAYER//')'
      WRITE(YRECFM,YFMT) 'TSN_',HSURFTYPE,JL
      YRECFM=ADJUSTL(HPREFIX//YRECFM)
      WRITE(YFMT,'(A6,I1,A9)')     '(A10,A',ISURFTYPE_LEN,','//YNLAYER//',A8))'
      WRITE(YCOMMENT,YFMT) 'X_Y_TSNOW_',HSURFTYPE,JL,' (K)'
      CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%T(:,JL),KI)
      !
    END IF
    !
  ENDDO
  !
ENDIF
!
!
!*       14.    Albedo
!              ------
!
IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' .OR. TPSNOW%SCHEME=='1-L' .OR. &
    TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
  !
  WRITE(YFMT,'(A5,I1,A1)')     '(A4,A',ISURFTYPE_LEN,')'
  WRITE(YRECFM,YFMT) 'ASN_',HSURFTYPE
  YRECFM=ADJUSTL(HPREFIX//YRECFM)
  WRITE(YFMT,'(A6,I1,A5)')     '(A10,A',ISURFTYPE_LEN,',A10)'
  WRITE(YCOMMENT,YFMT) 'X_Y_ASNOW_',HSURFTYPE,' (no unit)'
  CALL  WRITE_FIELD_1D_PATCH(HSELECT,HPROGRAM,YRECFM,YCOMMENT,KPATCH,KMASK_P,TPSNOW%ALB(:),&
          KI,PALB_WR)
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_GR_SNOW',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITESURF_GR_SNOW
END MODULE MODI_WRITESURF_GR_SNOW
