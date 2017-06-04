!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TREAT_FIELD (UG, U, USS, &
                              HPROGRAM,HSCHEME,HFILETYPE,    &
                              HSUBROUTINE,HFILENAME,HFIELD,   &
                              PPGDARRAY            )  
!     ##############################################################
!
!!**** *TREAT_FIELD* chooses which treatment subroutine to use
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/09/95
!!
!!    Modification
!!    25/05/96    (V. Masson) remove useless case for HSUBROUTINE   
!!    29/11/2002  (D. Gazen)  add HSFTYPE argument + call to read_binllvfast routine
!!    03/2004     (V. MAsson) externalization
!!    04/2009     (B. Decharme) Special treatement for gaussian grid
!!    06/2009     (B. Decharme)  call Topographic index statistics calculation
!!    09/2010     (E. Kourzeneva) call reading of the lake database
!!    03/2012     (M. Lafaysse) NETCDF
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PGDWORK, ONLY : NSIZE, NSIZE_ALL, XALL, NSSO_ALL, XSSO_ALL, &
                         XSUMVAL, XEXT_ALL, CATYPE, &
                         XSSQO, LSSQO, NSSO, XMIN_WORK, XMAX_WORK, & 
                         NVALNBR, NVALCOUNT, XVALLIST, JPVALMAX
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC, NCOMM, NREQ, NINDEX, IDX_R, &
                                NSIZE_TASK,NREQ, NSIZE_max=>NSIZE
!
USE MODD_DATA_LAKE,      ONLY : NGRADDEPTH_LDB, NGRADSTATUS_LDB 
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
!
USE MODI_INI_SSOWORK
USE MODI_GET_LUOUT
USE MODI_READ_DIRECT
USE MODI_READ_DIRECT_GAUSS
USE MODI_READ_LATLON
USE MODI_READ_BINLLV
USE MODI_READ_BINLLVFAST
USE MODI_READ_ASCLLV
USE MODI_READ_AND_SEND_MPI
USE MODI_READ_PGD_NETCDF
USE MODI_AVERAGE2_MESH
USE MODI_MAKE_LCOVER
USE MODI_ABOR1_SFX
USE MODI_AVERAGE2_COVER
USE MODI_AVERAGE2_CTI
USE MODI_AVERAGE2_LDB
USE MODI_AVERAGE2_OROGRAPHY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HFILETYPE     ! Type of the data file
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=20), INTENT(IN) :: HFIELD        ! Name of the field.
REAL, DIMENSION(:,:), INTENT(INOUT), OPTIONAL :: PPGDARRAY ! field on MESONH grid
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: I3D_ALL
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZVALLIST, ZVAL
REAL, DIMENSION(:,:), ALLOCATABLE ::  ZEXTVAL
INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISIZE0
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IVALNBR
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IVALCOUNT
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ISIZE
!
INTEGER :: IMAX  ! Maximum of times a value has been encountered in the grid mesh
INTEGER :: IVAL  ! Index of this value
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
INTEGER, DIMENSION(MPI_STATUS_SIZE,NPROC-1) :: ISTATUS2
#endif
INTEGER, DIMENSION(0:NPROC-1) :: ITCOV
INTEGER :: ILUOUT, IS2, INFOMPI, JP, ICPT, JCOV, JI, JL, IREQ, IDX,&
                        IDX_SAVE, JT
LOGICAL :: GMULTITYPE
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_1',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.     Selection of type of reading (and point by point treatment)
!            -----------------------------------------------------------
!
GMULTITYPE = .FALSE.
IF (HFILETYPE=='DIRTYP') GMULTITYPE = .TRUE.
!
SELECT CASE (HFILETYPE)

  CASE ('DIRECT','DIRTYP')
    IF(UG%G%CGRID=="GAUSS     " .OR. UG%G%CGRID=="IGN       " .OR. UG%G%CGRID=="LONLAT REG")THEN
      CALL READ_DIRECT_GAUSS(UG, U, USS, &
                             HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD,GMULTITYPE)
    ELSE
      CALL READ_DIRECT(UG, U, USS, &
                       HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD,GMULTITYPE)
    ENDIF

  CASE ('BINLLV')
    IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_BINLLV(UG, U, USS, &
                        HPROGRAM,HSUBROUTINE,HFILENAME)

  CASE ('BINLLF')
    IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_BINLLVFAST(UG, U, USS, &
                            HPROGRAM,HSUBROUTINE,HFILENAME)

  CASE ('ASCLLV')
    IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_ASCLLV(UG, U, USS, &
                        HPROGRAM,HSUBROUTINE,HFILENAME)

  CASE ('LATLON')
    IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_LATLON(UG, U, USS, &
                        HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME)

  CASE ('NETCDF')
    IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK
    IF (NRANK==NPIO) CALL READ_PGD_NETCDF(UG, U, USS, &
                            HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)

  CASE DEFAULT
    CALL ABOR1_SFX('TREAT_FIELD: FILE TYPE NOT SUPPORTED: '//HFILETYPE)

END SELECT
!
!-------------------------------------------------------------------------------
!
!nsize contains the number of points found for each of the domain, for each task
ALLOCATE(NSIZE(U%NSIZE_FULL,SIZE(NSIZE_ALL,2)))
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_1',1,ZHOOK_HANDLE)
!
IF (NPROC>1) THEN
  !
  IF (HFILETYPE=='DIRECT'.OR.HFILETYPE=='DIRTYP') THEN
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_21',0,ZHOOK_HANDLE)
    !
    ALLOCATE(ISIZE (NSIZE_max,SIZE(NSIZE_ALL,2),NPROC))
    ALLOCATE(ISIZE0(NSIZE_max,SIZE(NSIZE_ALL,2)))
    !
    IDX_SAVE = IDX_R
    IDX = IDX_SAVE + NRANK
    !
    IDX = IDX + 1
    ISIZE(:,:,:) = 0
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_21',1,ZHOOK_HANDLE)
    !
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP) 
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_22',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JP,ICPT,JI,IREQ,INFOMPI)
    DO JP=0,NPROC-1
      IF (JP/=NRANK) THEN
        ICPT = 0
        DO JI = 1,SIZE(NINDEX)
          IF (NINDEX(JI)==JP) THEN
            ICPT = ICPT + 1
            ISIZE(ICPT,:,JP+1) = NSIZE_ALL(JI,:)
          ENDIF
        ENDDO
        IF (JP<NRANK) THEN
          IREQ = JP+1
        ELSE
          IREQ = JP
        ENDIF
#ifdef SFX_MPI        
        CALL MPI_ISEND(ISIZE(:,:,JP+1),SIZE(ISIZE,1)*SIZE(ISIZE,2)*KIND(ISIZE)/4,&
                             MPI_INTEGER,JP,IDX,NCOMM,NREQ(IREQ),INFOMPI)
#endif
      ENDIF
    ENDDO
!$OMP END DO
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_22',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_23',0,ZHOOK_HANDLE)
    !
    NSIZE(:,:) = 0
    !for each task
    DO JP=0,NPROC-1
      !
      ISIZE0(:,:) = 0
      !
      IF (JP/=NRANK) THEN
        !each task receives each ISIZE from each task
#ifdef SFX_MPI        
        CALL MPI_RECV(ISIZE0,NSIZE_max*SIZE(ISIZE0,2)*KIND(ISIZE0)/4,MPI_INTEGER,&
                      JP,IDX_SAVE+1+JP,NCOMM,ISTATUS,INFOMPI)
#endif
      ELSE
        !
        ICPT = 0
        DO JI = 1,SIZE(NINDEX)
          IF (NINDEX(JI)==JP) THEN
            ICPT = ICPT + 1
            ISIZE0(ICPT,:) = NSIZE_ALL(JI,:)
          ENDIF
        ENDDO
        !
      ENDIF
      !
      !nsize is the sum of all parts isize
      NSIZE(:,:) = NSIZE(:,:) + ISIZE0(1:NSIZE_TASK(NRANK),:)
      !
    ENDDO
#ifdef SFX_MPI    
    CALL MPI_WAITALL(NPROC-1,NREQ(1:NPROC-1),ISTATUS2,INFOMPI)
#endif
    !
    DEALLOCATE(ISIZE,ISIZE0)
    !
    IDX_R = IDX_R + NPROC
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_23',1,ZHOOK_HANDLE)
    !
  ELSE
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_3',0,ZHOOK_HANDLE)
    !
    CALL READ_AND_SEND_MPI(NSIZE_ALL,NSIZE)
    !
    IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_3',1,ZHOOK_HANDLE)
    !
  ENDIF
  !
ELSE
  IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_4',0,ZHOOK_HANDLE)
  NSIZE(:,:) = NSIZE_ALL(:,:)
  IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_4',1,ZHOOK_HANDLE)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_5',0,ZHOOK_HANDLE)
!
DEALLOCATE(NSIZE_ALL)
!
!
IF (HSUBROUTINE=='A_COVR') THEN
  IS2 = SIZE(XALL,2)
ELSEIF (HSUBROUTINE=='A_LDBS') THEN
  IS2 = NGRADSTATUS_LDB
ELSEIF (HSUBROUTINE=='A_LDBD') THEN
  IS2 = NGRADDEPTH_LDB
ELSEIF (HSUBROUTINE=='A_OROG') THEN
  IS2 = 2
ELSEIF (HSUBROUTINE=='A_CTI ') THEN
  IS2 = 3 
ENDIF
!
!
SELECT CASE (HSUBROUTINE)

  CASE ('A_COVR')
    !
    !to gather the parts of LCOVER sparsed among the tasks
    CALL MAKE_LCOVER(U%LCOVER)
    !
    !contains the indexes of the covers in XCOVER, associated to their effective
    !num
    ALLOCATE(IMASK(SIZE(U%LCOVER)))
    IMASK(:) = 0
    ICPT = 0
    DO JCOV = 1,SIZE(U%LCOVER)
      IF (U%LCOVER(JCOV)) THEN
        ICPT = ICPT + 1
        IMASK(JCOV) = ICPT
      ENDIF
    ENDDO
    !
    !because each task did not necessarily meet the same number of coves, itcov 
    !contains the numbers of covers met for all tasks 
    IF (NPROC>1) THEN
#ifdef SFX_MPI
    CALL MPI_ALLGATHER(IS2,KIND(IS2)/4,MPI_INTEGER,&
                     ITCOV,KIND(ITCOV)/4,MPI_INTEGER,NCOMM,INFOMPI)
#endif
    ELSE
      ITCOV(:) = IS2
    ENDIF
    !
    !XSUMVAL needs to contain the numbers of times each cover is encountered
    !for the current task 
    ALLOCATE(XSUMVAL(U%NSIZE_FULL,COUNT(U%LCOVER)))
    XSUMVAL(:,:) = 0.
    !
    IF (NPROC>1) THEN
      ALLOCATE(ZVAL(U%NSIZE_FULL,MAXVAL(ITCOV),2))
      DO JP = 0,NPROC-1
        !the part of XALL concerning the current task is sent 
        !from each other task
        CALL READ_AND_SEND_MPI(XALL,ZVAL(:,1:ITCOV(JP),:),KPIO=JP)
        DO JL = 1,ITCOV(JP)
          DO JI=1,U%NSIZE_FULL
            JCOV = NINT(ZVAL(JI,JL,1))
            IF (JCOV/=0) THEN
              !xsumval2 is the sum of contributions zsumval3 coming from all tasks
              XSUMVAL(JI,IMASK(JCOV)) = XSUMVAL(JI,IMASK(JCOV)) + ZVAL(JI,JL,2)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(ZVAL)
    ELSE
      DO JL = 1,SIZE(XALL,2)
        DO JI=1,SIZE(XALL,1)
          JCOV = NINT(XALL(JI,JL,1))
          IF (JCOV/=0) THEN
            XSUMVAL(JI,IMASK(JCOV)) = XSUMVAL(JI,IMASK(JCOV)) + XALL(JI,JL,2)
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    DEALLOCATE(XALL,IMASK)
    !
    !
  CASE ('A_LDBD','A_LDBS','A_OROG','A_CTI ')
    !
    !XSUMVAL needs to contain the numbers of times each quantity is encountered
    !for the current task    
    ALLOCATE(XSUMVAL(U%NSIZE_FULL,IS2))
    XSUMVAL(:,:) = 0.
    IF (NPROC>1) THEN
      ALLOCATE(ZVAL(U%NSIZE_FULL,IS2,1))
      DO JP = 0,NPROC-1
        CALL READ_AND_SEND_MPI(XALL(:,:,1),ZVAL(:,:,1),KPIO=JP)
        !sum of contributions coming from all tasks
        XSUMVAL(:,:) = XSUMVAL(:,:) + ZVAL(:,:,1)
      ENDDO
      DEALLOCATE(ZVAL)
    ELSE
      XSUMVAL(:,:) = XALL(:,:,1)
    ENDIF
    DEALLOCATE(XALL)
    !
    !
    IF (HSUBROUTINE=='A_OROG' .OR. HSUBROUTINE=='A_CTI ') THEN
      !
      !special fields
      !
      IF (HSUBROUTINE=='A_CTI ') THEN
        !max and min
        IF (NPROC>1) THEN
          ALLOCATE(ZEXTVAL(U%NSIZE_FULL,1))
          DO JP = 0,NPROC-1
            CALL READ_AND_SEND_MPI(XEXT_ALL(:,1),ZEXTVAL(:,1),KPIO=JP)
            XMAX_WORK(:) = MAX(XMAX_WORK,ZEXTVAL(:,1))
          ENDDO
          DO JP = 0,NPROC-1
            CALL READ_AND_SEND_MPI(XEXT_ALL(:,2),ZEXTVAL(:,1),KPIO=JP)
            XMIN_WORK(:) = MIN(XMIN_WORK,ZEXTVAL(:,1))
          ENDDO   
          DEALLOCATE(ZEXTVAL)
        ELSE
          XMAX_WORK(:) = XEXT_ALL(:,1)
          XMIN_WORK(:) = XEXT_ALL(:,2)
        ENDIF
        !
      ELSEIF (HSUBROUTINE=='A_OROG') THEN
        !max and min
        IF (NPROC>1) THEN
          ALLOCATE(ZEXTVAL(U%NSIZE_FULL,1))
          DO JP = 0,NPROC-1
            CALL READ_AND_SEND_MPI(XEXT_ALL(:,1),ZEXTVAL(:,1),KPIO=JP)
            USS%XMAX_ZS(:) = MAX(USS%XMAX_ZS,ZEXTVAL(:,1))
          ENDDO 
          DO JP = 0,NPROC-1
            CALL READ_AND_SEND_MPI(XEXT_ALL(:,2),ZEXTVAL(:,1),KPIO=JP)
            USS%XMIN_ZS(:) = MIN(USS%XMIN_ZS,ZEXTVAL(:,1))
          ENDDO   
          DEALLOCATE(ZEXTVAL)
        ELSE
          USS%XMAX_ZS(:) = XEXT_ALL(:,1)
          USS%XMIN_ZS(:) = XEXT_ALL(:,2)
        ENDIF
        !
        !sso fields   
        ALLOCATE(XSSQO(U%NSIZE_FULL,NSSO,NSSO))
        XSSQO(:,:,:) = -XUNDEF
        IF (NPROC>1) THEN
          ALLOCATE(ZVAL(U%NSIZE_FULL,NSSO,NSSO))
          DO JP = 0,NPROC-1
            CALL READ_AND_SEND_MPI(XSSO_ALL,ZVAL,KPIO=JP)
            XSSQO(:,:,:) = MAX(XSSQO(:,:,:),ZVAL)
          ENDDO
          DEALLOCATE(ZVAL)
        ELSE
          XSSQO(:,:,:) = XSSO_ALL(:,:,:)
        ENDIF
        DEALLOCATE(XSSO_ALL)
        !
        ALLOCATE(LSSQO(U%NSIZE_FULL,NSSO,NSSO))
        LSSQO(:,:,:) = .FALSE.
        IF (NPROC>1) THEN
          ALLOCATE(I3D_ALL(U%NSIZE_FULL,NSSO,NSSO)) 
          DO JP = 0,NPROC-1       
            CALL READ_AND_SEND_MPI(NSSO_ALL,I3D_ALL,KPIO=JP)
            WHERE (I3D_ALL(:,:,:)==1) LSSQO(:,:,:) = .TRUE.
          ENDDO
          DEALLOCATE(I3D_ALL)
        ELSE
          WHERE(NSSO_ALL(:,:,:)==1) LSSQO(:,:,:) = .TRUE.
        ENDIF
        DEALLOCATE(NSSO_ALL)
      ENDIF
      DEALLOCATE(XEXT_ALL)
      !
    ENDIF
    !
    !
  CASE ('A_MESH')
   IF (CATYPE/='MAJ') THEN

    ALLOCATE(XSUMVAL(U%NSIZE_FULL,SIZE(XALL,2)))
    !most simple case
    IF (NPROC>1) THEN
      XSUMVAL(:,:) = 0.
      ALLOCATE(ZVAL(U%NSIZE_FULL,SIZE(XALL,2),1))
      DO JP = 0,NPROC-1
        CALL READ_AND_SEND_MPI(XALL(:,:,1),ZVAL(:,:,1),KPIO=JP)
        XSUMVAL(:,:) = XSUMVAL(:,:) + ZVAL(:,:,1)
      ENDDO
      DEALLOCATE(ZVAL)
    ELSE
      XSUMVAL(:,:) = XALL(:,:,1)
    ENDIF
    DEALLOCATE(XALL)

   ELSE

     ALLOCATE(XSUMVAL(U%NSIZE_FULL,1))
     IF (HFILETYPE=='DIRECT' .AND. NPROC>1) THEN
       CALL ABOR1_SFX("TREAT_FIELD: MAJ is not possible with DIRECT filetype and NPROC>1")
     ELSE
       ALLOCATE(IVALNBR(U%NSIZE_FULL,SIZE(NVALNBR,2)),IVALCOUNT(U%NSIZE_FULL,JPVALMAX,SIZE(NVALNBR,2)),&
                ZVALLIST(U%NSIZE_FULL,JPVALMAX,SIZE(NVALNBR,2)))
       IF (NPROC>1) THEN
         CALL READ_AND_SEND_MPI(NVALNBR,IVALNBR)
         CALL READ_AND_SEND_MPI(NVALCOUNT,IVALCOUNT)
         CALL READ_AND_SEND_MPI(XVALLIST,ZVALLIST)
       ELSE
         IVALNBR = NVALNBR
         IVALCOUNT = NVALCOUNT
         ZVALLIST = XVALLIST
       ENDIF
       DEALLOCATE(NVALNBR,NVALCOUNT,XVALLIST)
       DO JT=1,SIZE(NSIZE,2)
         DO JI=1,SIZE(NSIZE,1)
           IF(NSIZE(JI,JT)==0) CYCLE
             !* determines the index of the value which has been the most encountered
             !  in the grid mesh
             IMAX=0
             DO JL=1,IVALNBR(JI,JT)
               IF (IVALCOUNT(JI,JL,JT)>IMAX) THEN
                 IMAX = IVALCOUNT(JI,JL,JT)
                 IVAL = JL
               END IF
             END DO
             !* sets this value to the PGD field
             XSUMVAL(JI,JT)=ZVALLIST(JI,IVAL,JT)
         END DO
       ENDDO
       DEALLOCATE(IVALNBR,IVALCOUNT,ZVALLIST)
     ENDIF
   ENDIF
    !
END SELECT
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_5',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    2.     Call to the adequate subroutine (global treatment)
!            --------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_6',0,ZHOOK_HANDLE)
!
SELECT CASE (HSUBROUTINE)

  CASE ('A_COVR')
    CALL AVERAGE2_COVER(U, HPROGRAM)

  CASE ('A_OROG')
    CALL AVERAGE2_OROGRAPHY(USS)

  CASE ('A_CTI ')
    CALL AVERAGE2_CTI

  CASE ('A_LDBD')
    CALL AVERAGE2_LDB(PPGDARRAY(:,1),'D',1)

  CASE ('A_LDBS')
    CALL AVERAGE2_LDB(PPGDARRAY(:,1),'S',1)
    
  CASE ('A_MESH')
    IF (.NOT. PRESENT(PPGDARRAY)) THEN
      WRITE(ILUOUT,*) 'You asked to average a PGD field with A_MESH option,'
      WRITE(ILUOUT,*) 'but you did not give the array to store this field'
      CALL ABOR1_SFX('TREAT_FIELD: ARRAY IS MISSING')
    END IF
    CALL AVERAGE2_MESH(PPGDARRAY)

END SELECT
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD_6',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_FIELD
