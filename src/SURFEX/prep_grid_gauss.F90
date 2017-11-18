!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_PREP_GRID_GAUSS
CONTAINS
      SUBROUTINE PREP_GRID_GAUSS (&
                                  HFILETYPE,HINTERP_TYPE,KNI)
!     ##########################################################################
!
!!****  *PREP_GRID_GAUSS* - reads EXTERNALIZED Surface grid.
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!!      M. Jidane    Nov 2013 : correct allocation of NINLO and reading of INLOPA
!!      F. Taillefer Dec 2013 : debug estimation of XILO2
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
!
!
!
USE MODI_READ_SURF
!
USE MODD_GRID_GAUSS, ONLY : XILA1, XILO1, XILA2, XILO2, NINLA, NINLO, NILEN, LROTPOLE, &
                            XCOEF, XLAP, XLOP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
!
!
 CHARACTER(LEN=6),  INTENT(IN)    :: HFILETYPE    ! file type
 CHARACTER(LEN=6),  INTENT(OUT)   :: HINTERP_TYPE ! Grid type
INTEGER,           INTENT(OUT)   :: KNI          ! number of points
!
!* 0.2 Declaration of local variables
!      ------------------------------
!
 CHARACTER(LEN=12) :: YRECFM    ! Name of the article to be read
INTEGER           :: IRESP
!
!
INTEGER           :: JL        ! loop counter
REAL, DIMENSION(:), ALLOCATABLE :: ZW ! work array
!
INTEGER :: INLATI  ! number of pseudo-latitudes
INTEGER :: INLATI2 ! number of half pseudo-latitudes
REAL    :: ZLAPO   ! latitude of the rotated pole  (deg)
REAL    :: ZLOPO   ! longitude of the rotated pole (deg)
REAL    :: ZCODIL  ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_GRID_GAUSS',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------------
!
!*   1 Projection
!      ----------
!
YRECFM = 'LAPO'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,ZLAPO,IRESP)
YRECFM = 'LOPO'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,ZLOPO,IRESP)
YRECFM = 'CODIL'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,ZCODIL,IRESP)
!
!-----------------------------------------------------------------------
!
!*   2 Grid
!      ----
!
YRECFM = 'NLATI'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,INLATI,IRESP)
!
IF (ALLOCATED(INLOPA)) DEALLOCATE(INLOPA)
ALLOCATE(INLOPA(INLATI))
IF (ALLOCATED(NINLO)) DEALLOCATE(NINLO)
ALLOCATE(NINLO(INLATI))
YRECFM = 'NLOPA'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,INLOPA,IRESP,HDIR='-')
!
KNI = SUM(INLOPA)
!
!-----------------------------------------------------------------------
!
!*   3 Computes additional quantities used in interpolation
!      ----------------------------------------------------
!
INLATI2  = NINT(REAL(INLATI)/2.0)
NINLA    = INLATI
NILEN    = KNI
XLOP     = ZLOPO
XLAP     = ZLAPO
XCOEF    = ZCODIL
!
NINLO(:) = INLOPA(:)
!
!* type of transform
IF (ZLAPO>89.99 .AND. ABS(ZLOPO)<0.00001) THEN
  LROTPOLE = .FALSE.
ELSE
  LROTPOLE = .TRUE.
ENDIF
!
XILA1=90.0*(1.0-0.5/REAL(INLATI))
XILO1=0.0
XILA2=-90.0*(1.0-0.5/REAL(INLATI))
XILO2=360.0*(REAL(INLOPA(INLATI2))-1.0)/REAL(INLOPA(INLATI2))
!
HINTERP_TYPE = 'HORIBL'
!-----------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PREP_GRID_GAUSS',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------
!
END SUBROUTINE PREP_GRID_GAUSS
END MODULE MODI_PREP_GRID_GAUSS
