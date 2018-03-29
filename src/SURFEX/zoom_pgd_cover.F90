!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_ZOOM_PGD_COVER
CONTAINS
      SUBROUTINE ZOOM_PGD_COVER (DTCO, UG, U, GCP, &
                                 HPROGRAM,HINIFILE,HINIFILETYPE,OECOCLIMAP)
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!     Modification 17/04/12 M.Tomasini All COVER physiographic fields are now
!!                                     interpolated for spawning =>
!!                                     ABOR1_SFX if (.NOT.OECOCLIMAP) in comment
!     Modification 05/02/15 M.Moge : use NSIZE_FULL instead of SIZE(XLAT) (for clarity)
!!      J.Escobar 18/12/2015 : missing interface
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_DATA_COVER_PAR,   ONLY : JPCOVER
USE MODD_PREP,             ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_CONVERT_COVER_FRAC
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_PREP_GRID_EXTERN
USE MODI_HOR_INTERPOL
USE MODI_PREP_OUTPUT_GRID
USE MODI_OLD_NAME
USE MODI_SUM_ON_ALL_PROCS
USE MODI_GET_LUOUT
USE MODI_CLEAN_PREP_OUTPUT_GRID
USE MODI_GET_1D_MASK
USE MODI_READ_LCOVER
#ifdef SFX_MNH
USE MODI_READ_SURFX2COV_1COV_MNH
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
LOGICAL,              INTENT(OUT) :: OECOCLIMAP  ! flag to use ecoclimap
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ICPT1
INTEGER :: IRESP
INTEGER :: ILUOUT
INTEGER :: INI     ! total 1D dimension (input grid)
INTEGER :: IL      ! total 1D dimension (output grid)
INTEGER :: JCOVER  ! loop counter
INTEGER :: IVERSION       ! surface version
#ifdef MNH_PARALLEL
REAL, DIMENSION(:), POINTER  :: ZCOVER1D
#endif
REAL, DIMENSION(:,:), POINTER     :: ZCOVER
REAL, DIMENSION(:,:), POINTER :: ZSEA1, ZWATER1, ZNATURE1, ZTOWN1
REAL, DIMENSION(:,:), POINTER :: ZSEA2, ZWATER2, ZNATURE2, ZTOWN2
REAL, DIMENSION(:),   ALLOCATABLE :: ZSUM
 CHARACTER(LEN=12)  :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100) :: YCOMMENT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_COVER',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
 CALL OPEN_AUX_IO_SURF(HINIFILE,HINIFILETYPE,'FULL  ')
!
 CALL READ_SURF(HPROGRAM,'ECOCLIMAP',OECOCLIMAP,IRESP)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL PREP_OUTPUT_GRID(UG%G, UG%G, U%NSIZE_FULL, ILUOUT)
!
 CALL PREP_GRID_EXTERN(GCP,HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)

!
!------------------------------------------------------------------------------
!
!*      3.     Reading of cover
!              ----------------
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
ALLOCATE(U%LCOVER(JPCOVER))
!
 CALL OLD_NAME(HPROGRAM,'COVER_LIST      ',YRECFM)
 CALL READ_LCOVER(HPROGRAM,U%LCOVER)
!
#ifndef MNH_PARALLEL
ALLOCATE(ZCOVER(INI,COUNT(U%LCOVER)))
 CALL READ_SURF_COV(HPROGRAM,YRECFM,ZCOVER(:,:),U%LCOVER,IRESP,HDIR='A')
#endif
!
ALLOCATE(ZSEA1   (INI,1))
ALLOCATE(ZNATURE1(INI,1))
ALLOCATE(ZWATER1 (INI,1))
ALLOCATE(ZTOWN1  (INI,1))
!
IF (IVERSION>=7) THEN
  CALL READ_SURF(HPROGRAM,'FRAC_SEA   ',ZSEA1(:,1),   IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_NATURE',ZNATURE1(:,1),IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_WATER ',ZWATER1(:,1), IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_TOWN  ',ZTOWN1(:,1),  IRESP,HDIR='A')
  !
ELSE
#ifndef MNH_PARALLEL
  CALL CONVERT_COVER_FRAC(DTCO,ZCOVER,U%LCOVER,ZSEA1(:,1),ZNATURE1(:,1),ZTOWN1(:,1),ZWATER1(:,1))
#endif
ENDIF
!
!------------------------------------------------------------------------------
!
!*      4.     Interpolations
!              --------------
!
IL = U%NSIZE_FULL
ALLOCATE(U%XCOVER(IL,COUNT(U%LCOVER)))
!
! on lit les cover une apres l'autre, et on appelle hor_interpol sur chaque cover separement
!
#ifdef MNH_PARALLEL
IF ( HPROGRAM == 'MESONH' ) THEN
  ALLOCATE(ZCOVER1D(INI))
  ICPT1 = 0
  DO JCOVER=1,JPCOVER
    IF ( U%LCOVER( JCOVER ) ) THEN
      ICPT1 = ICPT1 + 1
      CALL READ_SURFX2COV_1COV_MNH(YRECFM,INI,JCOVER,ZCOVER1D(:),IRESP,YCOMMENT,'A')
      CALL HOR_INTERPOL(DTCO, U,GCP,ILUOUT,SPREAD(ZCOVER1D,2,1),U%XCOVER(:,ICPT1:ICPT1))
    ENDIF
    !
  ENDDO
  DEALLOCATE(ZCOVER1D)
ENDIF
#else
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZCOVER,U%XCOVER)
DEALLOCATE(ZCOVER)
#endif
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
ALLOCATE(ZCOVER(IL,COUNT(U%LCOVER)))
!
ALLOCATE(ZSEA2  (IL,1))
ALLOCATE(ZNATURE2(IL,1))
ALLOCATE(ZWATER2 (IL,1))
ALLOCATE(ZTOWN2  (IL,1))
!
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZSEA1,ZSEA2)
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZNATURE1,ZNATURE2)
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZWATER1,ZWATER2)
 CALL HOR_INTERPOL(DTCO, U, GCP, ILUOUT,ZTOWN1,ZTOWN2)
!
DEALLOCATE(ZSEA1)
DEALLOCATE(ZNATURE1)
DEALLOCATE(ZWATER1)
DEALLOCATE(ZTOWN1)
!
ALLOCATE(U%XSEA   (IL))
ALLOCATE(U%XNATURE(IL))
ALLOCATE(U%XWATER (IL))
ALLOCATE(U%XTOWN  (IL))
!
U%XSEA(:)   = ZSEA2   (:,1)
U%XNATURE(:)= ZNATURE2(:,1)
U%XWATER(:) = ZWATER2 (:,1)
U%XTOWN(:)  = ZTOWN2  (:,1)
!
DEALLOCATE(ZSEA2)
DEALLOCATE(ZNATURE2)
DEALLOCATE(ZWATER2)
DEALLOCATE(ZTOWN2)
!
 CALL CLEAN_PREP_OUTPUT_GRID
!------------------------------------------------------------------------------
!
!*      5.     Coherence check
!              ---------------
!
ALLOCATE(ZSUM(IL))
ZSUM = 0.
DO JCOVER=1,SIZE(U%XCOVER,2)
  ZSUM(:) = ZSUM(:) + U%XCOVER(:,JCOVER)
END DO
!
DO JCOVER=1,SIZE(U%XCOVER,2)
  WHERE(ZSUM(:)/=0.)  U%XCOVER(:,JCOVER) = U%XCOVER(:,JCOVER)/ZSUM(:)
END DO
!
!------------------------------------------------------------------------------
!
!*      6.     Fractions
!              ---------
!
! When the model runs in multiproc, NSIZE* represents the number of points
! on a proc, and NDIM* the total number of points on all procs.
! The following definition of NDIM* won't be correct any more when the PGD
! runs in multiproc.
!
U%NSIZE_NATURE    = COUNT(U%XNATURE(:) > 0.0)
U%NSIZE_WATER     = COUNT(U%XWATER (:) > 0.0)
U%NSIZE_SEA       = COUNT(U%XSEA   (:) > 0.0)
U%NSIZE_TOWN      = COUNT(U%XTOWN  (:) > 0.0)
U%NSIZE_FULL      = IL
!
U%NDIM_NATURE    = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,U%XNATURE(:) > 0., 'DIM')
U%NDIM_WATER     = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,U%XWATER (:) > 0., 'DIM')
U%NDIM_SEA       = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,U%XSEA   (:) > 0., 'DIM')
U%NDIM_TOWN      = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,U%XTOWN  (:) > 0., 'DIM')
ZSUM=1.
U%NDIM_FULL      = SUM_ON_ALL_PROCS(HPROGRAM,UG%G%CGRID,ZSUM   (:) ==1., 'DIM')
DEALLOCATE(ZSUM)
!
ALLOCATE(U%NR_NATURE (U%NSIZE_NATURE))
ALLOCATE(U%NR_TOWN   (U%NSIZE_TOWN  ))
ALLOCATE(U%NR_WATER  (U%NSIZE_WATER ))
ALLOCATE(U%NR_SEA    (U%NSIZE_SEA   ))
!
IF (U%NSIZE_SEA   >0)CALL GET_1D_MASK( U%NSIZE_SEA,    U%NSIZE_FULL, U%XSEA   , U%NR_SEA   )
IF (U%NSIZE_WATER >0)CALL GET_1D_MASK( U%NSIZE_WATER,  U%NSIZE_FULL, U%XWATER , U%NR_WATER )
IF (U%NSIZE_TOWN  >0)CALL GET_1D_MASK( U%NSIZE_TOWN,   U%NSIZE_FULL, U%XTOWN  , U%NR_TOWN  )
IF (U%NSIZE_NATURE>0)CALL GET_1D_MASK( U%NSIZE_NATURE, U%NSIZE_FULL, U%XNATURE, U%NR_NATURE)
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_COVER',1,ZHOOK_HANDLE)

!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_COVER
END MODULE MODI_ZOOM_PGD_COVER
