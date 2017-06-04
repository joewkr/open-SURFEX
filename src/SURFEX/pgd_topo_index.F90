!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TOPO_INDEX (DTCO, UG, U, USS, S, OCTI, &
                                 HPROGRAM,KLU,HCTI,HCTIFILETYPE,OIMP_CTI)
!     ##################################################################
!
!!**** *PGD_TOPO_INDEX* monitor for computing topographic index statistics used by TOIPMODEL
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
!!    Original    06/2009
!!    B. Decharme 05/2013 New topographic index linear regression for Topmodel
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_ISBA_n, ONLY : ISBA_S_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_PGD_GRID,       ONLY : NL
!
!
USE MODD_PGDWORK,        ONLY : XALL, XEXT_ALL, NSIZE_ALL, &
                                XMIN_WORK, XMAX_WORK,     &
                                XMEAN_WORK, XSTD_WORK,    &
                                XSKEW_WORK, NSIZE, XSUMVAL 
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE MODD_SGH_PAR,        ONLY : XREGP, XREGA
!
USE MODI_GET_LUOUT
USE MODI_GET_GRID_COORD
USE MODI_READ_SURF
USE MODI_TREAT_FIELD
USE MODI_PACK_SAME_RANK
USE MODI_INTERPOL_FIELD
!
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
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
TYPE(ISBA_S_t), INTENT(INOUT) :: S
!
LOGICAL, INTENT(INOUT) :: OCTI
!
CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM     ! program calling
INTEGER,              INTENT(IN)  :: KLU          ! number of nature points
CHARACTER(LEN=28),    INTENT(IN)  :: HCTI         ! topographic index file name
CHARACTER(LEN=6),     INTENT(IN)  :: HCTIFILETYPE ! topographic index file type
LOGICAL,              INTENT(IN)  :: OIMP_CTI     ! .true. if topographic index statistics is imposed
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZLAT, ZDELTA, ZMEAN_INI, &
                                   ZTI_MEAN, ZTI_STD, ZTI_SKEW
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
!
LOGICAL :: LREG,LREG10,LREG2
!
INTEGER :: IFULL     ! total number of points
INTEGER :: I_DIM
INTEGER :: IRET      ! error code
INTEGER :: ILUOUT    ! output listing logical unit
!
CHARACTER(LEN=6  ) :: YFILETYPE, YSCHEME, YSUBROUTINE
CHARACTER(LEN=20)  :: YFIELD        ! Name of the field.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX',0,ZHOOK_HANDLE)
IF(LEN_TRIM(HCTI)==0)THEN
!
  ALLOCATE(S%XTI_MIN (0))
  ALLOCATE(S%XTI_MAX (0))
  ALLOCATE(S%XTI_MEAN(0))
  ALLOCATE(S%XTI_STD (0))
  ALLOCATE(S%XTI_SKEW(0))
!        
!-------------------------------------------------------------------------------
ELSE
!-------------------------------------------------------------------------------
!
  OCTI = .TRUE.
!
!*    2.      Find LUOUT
!             ----------
!
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) 'Comput Topographic indexes for TOPMODEL  '
  WRITE(ILUOUT,*) '*****************************************'
!
!*    3.      Allocations of statistics arrays
!             --------------------------------
!
  ALLOCATE(S%XTI_MIN (KLU))
  ALLOCATE(S%XTI_MAX (KLU))
  ALLOCATE(S%XTI_MEAN(KLU))
  ALLOCATE(S%XTI_STD (KLU))
  ALLOCATE(S%XTI_SKEW(KLU))
!
  S%XTI_MIN (:) = XUNDEF
  S%XTI_MAX (:) = XUNDEF
  S%XTI_MEAN(:) = XUNDEF
  S%XTI_STD (:) = XUNDEF
  S%XTI_SKEW(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    4.      Allocations of work arrays
!             --------------------------
!
  CALL GET_TYPE_DIM_n(DTCO, U, 'NATURE',I_DIM)
  IF (I_DIM/=KLU) THEN
     WRITE(ILUOUT,*)'PGD_TOPO_INDEX: Wrong dimension of MASK: ',I_DIM,KLU
     CALL ABOR1_SFX('PGD_TOPO_INDEX: WRONG DIMENSION OF MASK')
  ENDIF
!
  ALLOCATE(IMASK(KLU))
  IFULL=0
  CALL GET_SURF_MASK_n(DTCO, U, &
                       'NATURE',KLU,IMASK,IFULL,ILUOUT)
  IF (IFULL/=NL) THEN
     WRITE(ILUOUT,*)'PGD_TOPO_INDEX: Wrong dimension of FULL: ',IFULL,NL
     CALL ABOR1_SFX('PGD_TOPO_INDEX: WRONG DIMENSION OF FULL')
  ENDIF
!
  ALLOCATE(XMIN_WORK  (IFULL))
  ALLOCATE(XMAX_WORK  (IFULL))
  ALLOCATE(XMEAN_WORK (IFULL))
  ALLOCATE(XSTD_WORK  (IFULL))
  ALLOCATE(XSKEW_WORK (IFULL))
!
  XMIN_WORK (:)=XUNDEF
  XMAX_WORK (:)=XUNDEF
  XMEAN_WORK(:)=XUNDEF
  XSTD_WORK (:)=XUNDEF
  XSKEW_WORK(:)=XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    5.      Use of imposed field
!             --------------------
!
  IF (OIMP_CTI) THEN
!
     YFILETYPE=HCTIFILETYPE
     IF(HCTIFILETYPE=='NETCDF')THEN
        CALL ABOR1_SFX('Use another format than netcdf for cti input file with LIMP_CTI')
     ELSE
#ifdef SFX_ASC
       CFILEIN     = ADJUSTL(ADJUSTR(HCTI)//'.txt')
#endif
#ifdef SFX_FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(HCTI)//'.fa')
#endif
#ifdef SFX_LFI
       CFILEIN_LFI = ADJUSTL(HCTI)
#endif
CALL INIT_IO_SURF_n(DTCO, U, &
                           YFILETYPE,'FULL  ','SURF  ','READ ')
     ENDIF     
!   
     CALL READ_SURF(YFILETYPE,'TI_MIN' ,XMIN_WORK ,IRET) 
     CALL READ_SURF(YFILETYPE,'TI_MAX' ,XMAX_WORK ,IRET)
     CALL READ_SURF(YFILETYPE,'TI_MEAN',XMEAN_WORK,IRET)
     CALL READ_SURF(YFILETYPE,'TI_STD' ,XSTD_WORK ,IRET) 
     CALL READ_SURF(YFILETYPE,'TI_SKEW',XSKEW_WORK,IRET) 
!
     CALL END_IO_SURF_n(YFILETYPE)
!
  ELSE
!
!-------------------------------------------------------------------------------
!
!*    6.      Use of cti file
!             ---------------
!
     ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
     ALLOCATE(XEXT_ALL (U%NDIM_FULL,2))
     ALLOCATE(XALL     (U%NDIM_FULL,3,1))     
!
     NSIZE_ALL(:,1) = 0.
     XEXT_ALL (:,1) = -99999.
     XEXT_ALL (:,2) = 99999.
     XALL   (:,:,1) = 0.     
!
     XMAX_WORK(:) =-99999.
!
     YFIELD      = 'CTI'
     YSCHEME     = 'SURF  '
     YSUBROUTINE = 'A_CTI '
     CALL TREAT_FIELD(UG, U, USS, &
                      HPROGRAM,YSCHEME,HCTIFILETYPE,YSUBROUTINE,HCTI,YFIELD)
!
!-------------------------------------------------------------------------------
!
!*    7.      Coherence
!             ---------
!
     WHERE(NSIZE(:,1)<36.OR.XSTD_WORK(:)==0.0)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE   (:,1) = 0
     ENDWHERE 
!
     WHERE(U%XNATURE(:)>0.0.AND.XSKEW_WORK(:)<=-8.0)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE   (:,1) = 0
     ENDWHERE             
!
     WHERE(U%XNATURE(:)==0.)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE   (:,1) = 0
     ENDWHERE   
!
!-------------------------------------------------------------------------------
!
!*    8.      Regression 1km to 100m or 2m
!             ----------------------------
!
     IF(HCTIFILETYPE=='DIRECT')THEN
!
!      Topographic index linear regression for Topmodel if topo ref at 1km
!      pan and king (2012) 1km to 2m
!    
       CALL CTIREG(LREG,LREG10,LREG2)
!
       IF(ALL(XMEAN_WORK(:)==XUNDEF))LREG=.FALSE.
!
       IF(LREG)THEN
!          
         WRITE(ILUOUT,*)'WITH DIF, TOPO INDEX USED REGRESSIONS OF ' 
!
         ALLOCATE(ZDELTA     (IFULL))
         ALLOCATE(ZMEAN_INI  (IFULL))
         ALLOCATE(ZTI_MEAN   (IFULL))
         ALLOCATE(ZTI_STD    (IFULL))
         ALLOCATE(ZTI_SKEW   (IFULL))
!
         ZMEAN_INI(:)=XMEAN_WORK(:)
         ZTI_MEAN (:)=XUNDEF
         ZTI_STD  (:)=XUNDEF
         ZTI_SKEW (:)=XUNDEF
         ZDELTA   (:)= 0.0
!
!        1km to 10m
         IF(LREG10)THEN
           WRITE(ILUOUT,*)' PAN AND KING (2012) 1km to 10m '
           WHERE(XMEAN_WORK(:)/=XUNDEF.AND.(XMAX_WORK(:)-XMIN_WORK(:))>0.2)
               ZTI_MEAN(:)= 1.136+0.657*XMEAN_WORK(:)-0.640*XSTD_WORK(:)+0.053*XSKEW_WORK(:)
               ZTI_STD (:)=-0.128+0.187*XMEAN_WORK(:)+0.168*XSTD_WORK(:)-0.261*XSKEW_WORK(:)
               ZTI_SKEW(:)= 3.768-0.246*XMEAN_WORK(:)+0.317*XSTD_WORK(:)+0.222*XSKEW_WORK(:)
           ENDWHERE
             XMEAN_WORK(:)=ZTI_MEAN(:)
             XSTD_WORK (:)=ZTI_STD (:)
             XSKEW_WORK(:)=ZTI_SKEW(:)
         ELSE
             ZTI_MEAN(:)=XMEAN_WORK(:)
             ZTI_STD (:)=XSTD_WORK (:)
             ZTI_SKEW(:)=XSKEW_WORK(:)
         ENDIF
!
!          10m to 2m
         IF(LREG2)THEN
           WRITE(ILUOUT,*)' PAN AND KING (2012) 10m to 2m '
           WHERE(XMEAN_WORK(:)/=XUNDEF.AND.(XMAX_WORK(:)-XMIN_WORK(:))>0.2)
                   XMEAN_WORK(:)=-3.826+1.402*ZTI_MEAN(:)-0.434*ZTI_STD(:)+0.328*ZTI_SKEW(:)
                   XSTD_WORK (:)= 3.655-0.209*ZTI_MEAN(:)+0.440*ZTI_STD(:)-0.091*ZTI_SKEW(:)
                   XSKEW_WORK(:)= 2.266-0.023*ZTI_MEAN(:)-0.245*ZTI_STD(:)-0.240*ZTI_SKEW(:)
           ENDWHERE
         ENDIF
!
         WHERE(XMEAN_WORK(:)/=XUNDEF.AND.(XMAX_WORK(:)-XMIN_WORK(:))>0.2)
               XSTD_WORK (:)=MAX(0.2,XSTD_WORK (:))
               XSKEW_WORK(:)=MAX(0.2,XSKEW_WORK(:))
         ENDWHERE         
!           
         WHERE(XMEAN_WORK(:)>0.0.AND.XMEAN_WORK(:)/=XUNDEF)
               ZDELTA   (:)= (XMEAN_WORK(:)-ZMEAN_INI(:))
               XMIN_WORK(:)= MAX(             0.0,XMIN_WORK(:)+ZDELTA(:))
               XMAX_WORK(:)= MAX(XMIN_WORK(:)+0.2,XMAX_WORK(:)+ZDELTA(:))
         ELSEWHERE
              XMIN_WORK (:) = XUNDEF
              XMAX_WORK (:) = XUNDEF
              XMEAN_WORK(:) = XUNDEF
              XSTD_WORK (:) = XUNDEF
              XSKEW_WORK(:) = XUNDEF
              NSIZE   (:,1) = 0
         ENDWHERE
!
         DEALLOCATE(ZDELTA   )
         DEALLOCATE(ZMEAN_INI)
         DEALLOCATE(ZTI_MEAN )
         DEALLOCATE(ZTI_STD  )
         DEALLOCATE(ZTI_SKEW )
!     
       ENDIF
!  
     ENDIF
!
!-------------------------------------------------------------------------------
!
!*    10.     Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
    WRITE(ILUOUT,*) '*********************************************'
    WRITE(ILUOUT,*) 'Interpolation if some index not initialized  '
    WRITE(ILUOUT,*) '*********************************************'
!
    ALLOCATE(ZLAT(NL))
    CALL GET_GRID_COORD(UG%G%CGRID, UG%G%NGRID_PAR, UG%G%XGRID_PAR, U%NSIZE_FULL, &
                        ILUOUT,PY=ZLAT)
!
    WHERE (U%XNATURE(:)==0..AND.NSIZE(:,1)==0) NSIZE(:,1) = -1
!
!   No Antarctic
    WHERE(U%XNATURE(:)>0..AND.ZLAT(:)<-60.)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE   (:,1) = -1
    ENDWHERE   
!
    IF(ALL(NSIZE(:,1)==0.0))NSIZE(:,1)=-1
!
    CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,1),XMIN_WORK (:),'TI_MIN ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,1),XMAX_WORK (:),'TI_MAX ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,1),XMEAN_WORK(:),'TI_MEAN',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,1),XSTD_WORK (:),'TI_STD ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(UG, U, HPROGRAM,ILUOUT,NSIZE(:,1),XSKEW_WORK(:),'TI_SKEW',PDEF=XUNDEF,KNPTS=1)
!
    DEALLOCATE(NSIZE     )
    DEALLOCATE(XSUMVAL   )
    DEALLOCATE(ZLAT      )
!
  ENDIF
!-------------------------------------------------------------------------------
!
!*    11.     Asign parameters
!             ----------------
!
  CALL PACK_SAME_RANK(IMASK,XMIN_WORK ,S%XTI_MIN)
  CALL PACK_SAME_RANK(IMASK,XMAX_WORK ,S%XTI_MAX)
  CALL PACK_SAME_RANK(IMASK,XMEAN_WORK,S%XTI_MEAN)
  CALL PACK_SAME_RANK(IMASK,XSTD_WORK ,S%XTI_STD)
  CALL PACK_SAME_RANK(IMASK,XSKEW_WORK,S%XTI_SKEW)
!  
!-------------------------------------------------------------------------------
!
  DEALLOCATE(XMIN_WORK )
  DEALLOCATE(XMAX_WORK )
  DEALLOCATE(XMEAN_WORK)
  DEALLOCATE(XSTD_WORK )
  DEALLOCATE(XSKEW_WORK)
!
  WRITE(ILUOUT,*) '******************************'
  WRITE(ILUOUT,*) 'End Comput Topographic indexes'
  WRITE(ILUOUT,*) '******************************'
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE CTIREG(OREG,OREG10,OREG2)  
!      
!*    0.     DECLARATION
!            -----------
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
LOGICAL,           INTENT(OUT) :: OREG,OREG10,OREG2        ! regression key
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL    :: ZDLAT                      ! latitude mesh in the data file
REAL    :: ZDLON                      ! longitude mesh in the data file
REAL    :: ZGLBLATMIN                 ! minimum latitude of data box in the file
REAL    :: ZGLBLONMIN                 ! minimum longitude of data box in the file
REAL    :: ZGLBLATMAX                 ! maximum latitude of data box in the file
REAL    :: ZGLBLONMAX                 ! maximum longitude of data box in the file
!
CHARACTER(LEN=28)  :: YFILEHDR        ! Name of the field file header
!
INTEGER                    :: INBLAT
INTEGER                    :: INBLON
INTEGER                    :: IGLB
INTEGER                    :: JHEAD      ! loop control
INTEGER                    :: ININDEX    ! index of character 'N' in YSTRING1
INTEGER                    :: ISINDEX    ! index of character 'S' in YSTRING1
INTEGER                    :: IEINDEX    ! index of character 'E' in YSTRING1
INTEGER                    :: IWINDEX    ! index of character 'W' in YSTRING1
REAL, DIMENSION(7)         :: ZVAL       ! values of the head data
INTEGER                    :: IHEAD      ! index of the data in the array ZVAL
CHARACTER(LEN=100)         :: YSTRING    ! total string in the head
CHARACTER(LEN=100)         :: YSTRING1   ! string less the begining line descriptor
CHARACTER(LEN=100)         :: YVAL       ! absolute value of the data of the line
INTEGER                    :: IPOINT     ! index of '.' in the string YVAL
INTEGER                    :: ILENGTH    ! length of the string YVAL
INTEGER                    :: IFRACLENGTH! length of the fractional part in string YVAL
CHARACTER(LEN=2)           :: YLENGTH    ! length of the string YVAL
CHARACTER(LEN=2)           :: YFRACLENGTH! length of the fractional part in string YVAL
CHARACTER(LEN=10)          :: YINTERNALFORMAT ! format to read YVAL in real ZVAL
!
REAL    :: Z1000M, Z100M, Z10M
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX:CTIREG',0,ZHOOK_HANDLE)
!
OREG  =.FALSE.
OREG10=.FALSE.
OREG2 =.FALSE.
!
IGLB=11
YFILEHDR =ADJUSTL(ADJUSTR(HCTI)//'.hdr')
CALL OPEN_NAMELIST(HPROGRAM,IGLB,YFILEHDR)
!
!*         1.    Line of comments
!                ----------------
!
READ (IGLB,'(A100)',END=99) YSTRING
!
!-------------------------------------------------------------------------------
!
!*         2.    Other lines
!                -----------
!
DO JHEAD=1,7
  READ (IGLB,'(A100)',END=99) YSTRING
  YSTRING=ADJUSTL(YSTRING)
!
!*         2.1   Selection of the line
!                ---------------------
!
  SELECT CASE (YSTRING(1:5))
         CASE('cutva')
           IHEAD=1
           YSTRING1=YSTRING(10:100)
         CASE('nodat')
           IHEAD=1
           YSTRING1=YSTRING(8:100)
         CASE('north')
           IHEAD=2
           YSTRING1=YSTRING(7:100)           
         CASE('south')
           IHEAD=3
           YSTRING1=YSTRING(7:100) 
         CASE('east:')
           IHEAD=4
           YSTRING1=YSTRING(6:100)  
         CASE('west:')
           IHEAD=5
           YSTRING1=YSTRING(6:100)  
         CASE('rows:')
           IHEAD=6
           YSTRING1=YSTRING(6:100) 
         CASE('cols:')
           IHEAD=7
           YSTRING1=YSTRING(6:100) 
  END SELECT
!
!*         2.2   Test on presence of geographical descritor (N, E, S or W)
!                ---------------------------------------------------------
!
  ININDEX=INDEX(YSTRING1,'N')
  ISINDEX=INDEX(YSTRING1,'S')
  IEINDEX=INDEX(YSTRING1,'E')
  IWINDEX=INDEX(YSTRING1,'W')
  YVAL=ADJUSTL(YSTRING1)
  IF (ININDEX/=0) YVAL=ADJUSTL(YSTRING1(1:ININDEX-1))
  IF (ISINDEX/=0) YVAL='-'//ADJUSTL(YSTRING1(1:ISINDEX-1))
  IF (IEINDEX/=0) YVAL=ADJUSTL(YSTRING1(1:IEINDEX-1))
  IF (IWINDEX/=0) YVAL='-'//ADJUSTL(YSTRING1(1:IWINDEX-1))
!
!*         2.3   Transformation of the data in real
!                ----------------------------------
!
  IPOINT=INDEX(YVAL,'.')
  IF (IPOINT==0) YVAL=ADJUSTL(ADJUSTR(YVAL)//'.')
!
!*         2.4   Definition of the format of the data
!                ------------------------------------
!
  ILENGTH=LEN_TRIM(ADJUSTL(ADJUSTR(YVAL)))
  IFRACLENGTH=ILENGTH-INDEX(YVAL,'.')
  WRITE(YLENGTH,'(I2)') ILENGTH
  WRITE(YFRACLENGTH,'(I2)') IFRACLENGTH
  YINTERNALFORMAT='(F'//YLENGTH//'.'//YFRACLENGTH//')'
!
!*         2.5   Data definition
!                ---------------
!
  READ(YVAL,ADJUSTL(YINTERNALFORMAT)) ZVAL(IHEAD)
!
ENDDO
!
!-------------------------------------------------------------------------------
!
!*         3.    Initialization of arguments, longitudes and latitudes
!                -----------------------------------------------------
!
ZGLBLATMAX=ZVAL(2)
ZGLBLATMIN=ZVAL(3)
ZGLBLONMIN=ZVAL(5)
ZGLBLONMAX=ZVAL(4)+NINT((ZVAL(5)-ZVAL(4)+180.+1.E-10)/360.)*360.
INBLAT=NINT(ZVAL(6))
INBLON=NINT(ZVAL(7))
!
ZDLAT=(ZGLBLATMAX-ZGLBLATMIN)/INBLAT
ZDLON=(ZGLBLONMAX-ZGLBLONMIN)/INBLON
!
Z1000M = 30./3600. !(minute arc to m)
Z100M  = Z1000M/10.
Z10M   = Z1000M/100.
!
IF(SQRT(ZDLAT*ZDLON)>Z100M)THEN
   OREG  =.TRUE.
   OREG10=.TRUE.
   OREG2 =.TRUE.
ENDIF
!
IF(SQRT(ZDLAT*ZDLON)>=Z10M.AND.SQRT(ZDLAT*ZDLON)<=Z100M)THEN
   OREG  =.TRUE.
   OREG10=.FALSE.
   OREG2 =.TRUE.
ENDIF
!
CALL CLOSE_NAMELIST(HPROGRAM,IGLB)
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX:CTIREG',1,ZHOOK_HANDLE)
RETURN
99 CONTINUE
CALL ABOR1_SFX('CTIREG: PB READING TOPO INDEX FILE HEADER')
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX:CTIREG',1,ZHOOK_HANDLE)
!
END SUBROUTINE CTIREG
!
!-------------------------------------------------------------------------------
END SUBROUTINE PGD_TOPO_INDEX
