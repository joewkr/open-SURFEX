!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_GRID_CARTESIAN (HFILETYPE,HINTERP_TYPE,KNI)
!     ##########################################################################
!
!!****  *PREP_GRID_CARTESIAN* - reads EXTERNALIZED Surface grid.
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
USE MODD_GRID_CARTESIAN, ONLY : XX, XY, NX, NY
!
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
REAL, DIMENSION(:), ALLOCATABLE :: ZW ! work array
 CHARACTER(LEN=12) :: YRECFM    ! Name of the article to be read
 CHARACTER(LEN=1) :: YDIR
INTEGER           :: IRESP
INTEGER           :: JL        ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
!*     Grid
!      ----
!
IF (LHOOK) CALL DR_HOOK('PREP_GRID_CARTESIAN',0,ZHOOK_HANDLE)
YRECFM = 'IMAX'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,NX,IRESP)
YRECFM = 'JMAX'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,NY,IRESP)
!
KNI = NX * NY
!
YDIR = '-'
IF (HFILETYPE=='MESONH') YDIR = 'A'
!
ALLOCATE(ZW(KNI))
!
IF (ALLOCATED(XX)) DEALLOCATE(XX)
ALLOCATE(XX(NX))
YRECFM = 'XX'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,ZW,IRESP,HDIR=YDIR)
XX = ZW(1:NX)


IF (ALLOCATED(XY)) DEALLOCATE(XY)
ALLOCATE(XY(NY))
YRECFM = 'YY'
 CALL READ_SURF(&
                HFILETYPE,YRECFM,ZW,IRESP,HDIR=YDIR)
DO JL=1,KNI
  IF (MOD(JL,NX)==0) XY(JL/NX) = ZW(JL)
END DO
DEALLOCATE(ZW)
!
!-----------------------------------------------------------------------
IF(KNI==1)THEN
  HINTERP_TYPE = 'UNIF  '
ELSE
  HINTERP_TYPE = 'BILIN '
ENDIF
IF (LHOOK) CALL DR_HOOK('PREP_GRID_CARTESIAN',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------
!
END SUBROUTINE PREP_GRID_CARTESIAN
