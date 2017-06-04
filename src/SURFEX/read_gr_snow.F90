!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_GR_SNOW(HPROGRAM, HSURFTYPE, HPREFIX,     &
                              KLU, KSIZE_P, KMASK_P, KPATCH, TPSNOW, HDIR, KVERSION, KBUGFIX, KNPATCH)  
!     ##########################################################
!
!!****  *READ_GR_SNOW* - routine to read snow surface fields
!!
!!    PURPOSE
!!    -------
!       Initialize snow surface fields.
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
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/01/99
!       F.solmon       06/00 adaptation for patch
!       V.Masson       01/03 new version of ISBA
!       B. Decharme    2008  If no WSNOW, WSNOW = XUNDEF
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_TYPE_SNOW
!
USE MODI_READ_SURF
!
USE MODI_ALLOCATE_GR_SNOW
USE MODI_PACK_SAME_RANK
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)           :: HPROGRAM  ! calling program
 CHARACTER (LEN=*),  INTENT(IN)           :: HSURFTYPE ! generic name used for
                                                      ! snow characteristics
                                                      ! storage in file
 CHARACTER (LEN=3),  INTENT(IN)           :: HPREFIX   ! generic name for patch
!                                                     ! identification                      
INTEGER,            INTENT(IN)           :: KLU       ! horizontal size of snow var.
INTEGER,            INTENT(IN)           :: KSIZE_P
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK_P
INTEGER,            INTENT(IN)           :: KPATCH    ! number of tiles
TYPE(SURF_SNOW), INTENT(INOUT)           :: TPSNOW    ! snow characteristics
 CHARACTER (LEN=1),  INTENT(IN), OPTIONAL :: HDIR      ! type of reading
!                                                     ! HDIR = 'A' : entire field on All processors
!                                                     ! HDIR = 'H' : distribution on each processor
!
INTEGER,            INTENT(IN), OPTIONAL :: KVERSION
INTEGER,            INTENT(IN), OPTIONAL :: KBUGFIX
INTEGER, INTENT(IN), OPTIONAL :: KNPATCH
!
!*       0.2   declarations of local variables
!
 CHARACTER (LEN=7) :: YFMT0               ! format for writing
 CHARACTER (LEN=100) :: YFMT                ! format for writing
 CHARACTER(LEN=16)   :: YRECFM2 
 CHARACTER(LEN=12)   :: YRECFM              ! Name of the article to be read
 CHARACTER(LEN=4)    :: YNLAYER     !Format depending on the number of layers
 CHARACTER(LEN=1)    :: YDIR                ! type of reading
 CHARACTER(LEN=3) :: YPAT
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK
!
INTEGER             :: IRESP, JI, JP              ! Error code after redding
INTEGER             :: ISURFTYPE_LEN, IPAT_LEN     ! 
INTEGER             :: JL              ! loop counter
INTEGER             :: IVERSION, IBUGFIX
INTEGER :: INPATCH
!
LOGICAL :: GVERSION, GDIM, GDIM2
LOGICAL             :: GSNOW               ! snow written in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW_1',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
INPATCH = 1
IF (PRESENT(KNPATCH)) INPATCH = KNPATCH
!
!-------------------------------------------------------------------------------
IF(PRESENT(KVERSION))THEN
  IVERSION=KVERSION
ELSE
  CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
ENDIF
IF(PRESENT(KBUGFIX))THEN
  IBUGFIX=KBUGFIX
ELSE
  CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
ENDIF
!
GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
GDIM2 = GDIM
IF (GDIM) CALL READ_SURF(HPROGRAM,'SPLIT_PATCH',GDIM2,IRESP)
!
!-------------------------------------------------------------------------------
!
GVERSION = (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3)
!
!*       1.    Type of snow scheme
!              -------------------
!
ISURFTYPE_LEN=LEN_TRIM(HSURFTYPE)
!
IF (KPATCH<=1) THEN

  IF (IVERSION <=2 .OR. (IVERSION==3 .AND. IBUGFIX<=4)) THEN
    WRITE(YFMT,'(A5,I1,A4)')     '(A5,A',ISURFTYPE_LEN,',A5)'
    WRITE(YRECFM2,YFMT) 'SNOW_',HSURFTYPE,'_TYPE'
  ELSE
    IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3) THEN
      WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A5)'
      WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_TYPE'
    ELSE
      WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A4)'
      WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_TYP'
      YRECFM2=ADJUSTL(HPREFIX//YRECFM2)
    ENDIF
  END IF
  !
  CALL READ_SURF(HPROGRAM,YRECFM2,TPSNOW%SCHEME,IRESP)
  !
  !*       2.    Snow levels
  !              -----------
  !
  !
  IF (IVERSION <=2 .OR. (IVERSION==3 .AND. IBUGFIX<=4)) THEN
    WRITE(YFMT,'(A5,I1,A4)')     '(A5,A',ISURFTYPE_LEN,',A6)'
    WRITE(YRECFM2,YFMT) 'SNOW_',HSURFTYPE,'_LAYER'
  ELSE
    WRITE(YFMT,'(A5,I1,A4)')     '(A3,A',ISURFTYPE_LEN,',A2)'
    WRITE(YRECFM2,YFMT) 'SN_',HSURFTYPE,'_N'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2=ADJUSTL(HPREFIX//YRECFM2)
  END IF
  !
  CALL READ_SURF(HPROGRAM,YRECFM2,TPSNOW%NLAYER,IRESP)
  !
ENDIF
!
!*       2.    Presence of snow fields in the file
!              -----------------------------------
!
IF (KPATCH>0.AND.GDIM) THEN
  WRITE(YPAT,'(I2)') KPATCH
  YPAT = "P"//ADJUSTL(YPAT)
  IPAT_LEN = LEN_TRIM(ADJUSTL(YPAT))        
ELSE
  YPAT = " "
  IPAT_LEN=1
ENDIF
!
IF (IVERSION >6 .OR. (IVERSION==6 .AND. IBUGFIX>=1)) THEN
  WRITE(YFMT,'(A5,I1,A2,I1,A1)') '(A3,A',ISURFTYPE_LEN,',A1,A',IPAT_LEN,')'
  WRITE(YRECFM,YFMT) 'SN_',ADJUSTL(HSURFTYPE(:LEN_TRIM(HSURFTYPE))),ADJUSTL(YPAT(:LEN_TRIM(YPAT)))
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM=ADJUSTL(HPREFIX//YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,GSNOW,IRESP)
ELSE
  IF (TPSNOW%NLAYER==0) THEN
    GSNOW = .FALSE.
    IF (TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='EBA') TPSNOW%NLAYER=1
    IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO'                          ) TPSNOW%NLAYER=3
  ELSE
    GSNOW = .TRUE.
  END IF
END IF
!
IF (.NOT.GDIM2) THEN
  YPAT = " "
  IPAT_LEN=1
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.    Allocations
!              -----------
!
 CALL ALLOCATE_GR_SNOW(TPSNOW,KSIZE_P)
!
IF (.NOT. GSNOW) THEN
  IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW_1',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
!*       4.    Additional key
!              ---------------
!
IF (IVERSION >= 7 .AND. HSURFTYPE=='VEG'.AND.KPATCH==1)  &
  CALL READ_SURF(HPROGRAM,'LSNOW_FRAC_T',LSNOW_FRAC_TOT,IRESP)
!
!-------------------------------------------------------------------------------
!
!
!*       5.    Snow reservoir
!              --------------
!
ALLOCATE(ZWORK(KLU,INPATCH))
!
IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' &
                         .OR. TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN 
  !
  WRITE(YFMT0,'(A5,I1,A1)') ',A1,A',ISURFTYPE_LEN
  !
  IF (GVERSION) THEN
    YFMT = '(A3'//YFMT0//')'
  ELSE
    YFMT = '(A5'//YFMT0//')'
  ENDIF
  CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"WSNOW",HSURFTYPE,TPSNOW%WSNOW)
  CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"RSNOW",HSURFTYPE,TPSNOW%RHO)
  !
  !*       7.    Snow temperature
  !              ----------------
  !
  IF (TPSNOW%SCHEME=='1-L') THEN
    !
    CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"TSNOW",HSURFTYPE,TPSNOW%T)
    !
  ENDIF
  !
  !*       8.    Heat content
  !              ------------
  !
  IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    !
    CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"HSNOW",HSURFTYPE,TPSNOW%HEAT)
    !
    IF (TPSNOW%SCHEME=='CRO') THEN
      !
      CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"SHIST",HSURFTYPE,TPSNOW%HIST)
      !
      !*       9.    Snow Gran1
      !              ------------
      !
      IF (GVERSION) THEN
        YFMT = "(A2,A1"//YFMT0//')'       
      ELSE
        YFMT = "(A5"//YFMT0//')'
      ENDIF
      YFMT = YFMT//YNLAYER//')'
      CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"SGRAN",HSURFTYPE,TPSNOW%GRAN1,HREC2="1")
      CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"SGRAN",HSURFTYPE,TPSNOW%GRAN2,HREC2="2")
      !
    ENDIF
    !
    IF ((TPSNOW%SCHEME=='3-L'.AND.IVERSION>=8) .OR. TPSNOW%SCHEME=='CRO') THEN
      !*       12.    Age parameter
      !              -------------------
      !
      IF (GVERSION) THEN
        YFMT = "(A3"//YFMT0//')'         
      ELSE
        YFMT = "(A4"//YFMT0//')'
      ENDIF
      YFMT = YFMT//YNLAYER//')'
      CALL READ_LAYERS(GVERSION,TPSNOW%NLAYER,YDIR,HPREFIX,YFMT,"SAGE",HSURFTYPE,TPSNOW%AGE)
      !
    ELSE
      !
      DO JL = 1,TPSNOW%NLAYER
        WHERE (TPSNOW%WSNOW(:,1) >= 0.0) 
          TPSNOW%AGE(:,JL) = 0.0
        ELSEWHERE
          TPSNOW%AGE(:,JL) = XUNDEF
        ENDWHERE
      ENDDO
      !
    END IF    
    !
  ENDIF
  !
  WRITE(YFMT,'(A5,I1,A2,I1,A1)') '(A4,A',ISURFTYPE_LEN,',A',IPAT_LEN,')'
  WRITE(YRECFM,YFMT) 'ASN_',ADJUSTL(HSURFTYPE(:LEN_TRIM(HSURFTYPE))),ADJUSTL(YPAT)
  IF (GVERSION) YRECFM=ADJUSTL(HPREFIX//YRECFM)
  IF (GDIM2) THEN
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:,1),IRESP,HDIR=YDIR)
    CALL PACK_SAME_RANK(KMASK_P,ZWORK(:,1),TPSNOW%ALB(:))
  ELSE
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK,IRESP,HDIR=YDIR)
    CALL PACK_SAME_RANK(KMASK_P,ZWORK(:,MAX(1,KPATCH)),TPSNOW%ALB(:))
  ENDIF
  !
ENDIF
!
DEALLOCATE(ZWORK)
!
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW_1',1,ZHOOK_HANDLE)
!
IF (TPSNOW%SCHEME=='1-L' .OR. TPSNOW%SCHEME=='D95' .OR. TPSNOW%SCHEME=='EBA' &
                         .OR. TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN 
  !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JI,JL)
  DO JI = 1,SIZE(TPSNOW%WSNOW,1)
    !
    IF (TPSNOW%WSNOW(JI,1) == 0.0 ) THEN
      !
      TPSNOW%ALB(JI) = XUNDEF
      !
      DO JL = 1,TPSNOW%NLAYER
        !
        TPSNOW%RHO(JI,JL)=XUNDEF
        IF (TPSNOW%SCHEME=='1-L') THEN
          TPSNOW%T(JI,JL) = XUNDEF
        ELSEIF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
          TPSNOW%HEAT(JI,JL) = XUNDEF
          IF (TPSNOW%SCHEME=='CRO') THEN
            TPSNOW%HIST (JI,JL) = XUNDEF
            TPSNOW%GRAN1(JI,JL) = XUNDEF
            TPSNOW%GRAN2(JI,JL) = XUNDEF
            TPSNOW%AGE  (JI,JL) = XUNDEF
          ENDIF
        ENDIF
        !
      ENDDO
    ENDIF
  ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('READ_GR_SNOW_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE READ_LAYERS(OVERSION,KNL,HDIRIN,HPREF,HFMT,HREC,HSURF,PTAB,HREC2)
!
USE MODE_READ_SURF_LAYERS
!
IMPLICIT NONE
!
LOGICAL, INTENT(IN) :: OVERSION
INTEGER, INTENT(IN) :: KNL
 CHARACTER(LEN=*), INTENT(IN) :: HDIRIN
 CHARACTER(LEN=*), INTENT(IN) :: HPREF
 CHARACTER(LEN=*), INTENT(IN) :: HFMT
 CHARACTER(LEN=*), INTENT(IN) :: HREC
 CHARACTER(LEN=*), INTENT(IN) :: HSURF
REAL, DIMENSION(:,:), INTENT(OUT) :: PTAB
 CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: HREC2
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK3D
 CHARACTER(LEN=1) :: YREC2
 CHARACTER(LEN=12)   :: YRECFM   ! Name of the article to be read
INTEGER :: JL, IRESP
!
IF (PRESENT(HREC2)) THEN
  YREC2=TRIM(HREC2)
ELSE
  YREC2=""
ENDIF
!
IF (YREC2/="") THEN
  WRITE(YRECFM,HFMT) TRIM(HREC),TRIM(YREC2),'_',TRIM(HSURF)
ELSE
  WRITE(YRECFM,HFMT) TRIM(HREC),'_',TRIM(HSURF)
ENDIF
IF (OVERSION) YRECFM=ADJUSTL(TRIM(HPREF)//YRECFM)
!
IF (GDIM2) THEN
  ALLOCATE(ZWORK3D(KLU,SIZE(PTAB,2),1))
ELSE
  ALLOCATE(ZWORK3D(KLU,SIZE(PTAB,2),INPATCH))
ENDIF
!
 CALL READ_SURF_LAYERS(HPROGRAM,YRECFM,GDIM2,ZWORK3D,IRESP,KPATCH=KPATCH,HDIR=YDIR)
!
DO JL = 1,KNL
  IF (GDIM2) THEN
    CALL PACK_SAME_RANK(KMASK_P,ZWORK3D(:,JL,1),PTAB(:,JL))
  ELSE
    CALL PACK_SAME_RANK(KMASK_P,ZWORK3D(:,JL,MAX(1,KPATCH)),PTAB(:,JL))
  ENDIF
ENDDO
!
DEALLOCATE(ZWORK3D)
!
END SUBROUTINE READ_LAYERS
!
END SUBROUTINE READ_GR_SNOW
