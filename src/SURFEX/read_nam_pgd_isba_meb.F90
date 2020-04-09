!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
MODULE MODI_READ_NAM_PGD_ISBA_MEB
CONTAINS
      SUBROUTINE READ_NAM_PGD_ISBA_MEB(HPROGRAM, KLUOUT, OMEB_PATCH, OFORC_MEASURE, &
                      OMEB_LITTER, OMEB_GNDRES)
!     #############################################################################
!
!!**** *READ_NAM_PGD_ISBA_MEB* reads namelist for ISBA
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
!!    Original    12/2014
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODE_POS_SURF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM      ! Type of program
INTEGER,             INTENT(IN)    :: KLUOUT
!
LOGICAL, DIMENSION(:), INTENT(OUT) :: OMEB_PATCH
LOGICAL              , INTENT(OUT) :: OFORC_MEASURE
LOGICAL              , INTENT(OUT) :: OMEB_LITTER
LOGICAL              , INTENT(OUT) :: OMEB_GNDRES
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
LOGICAL, DIMENSION(NVEGTYPE) :: LMEB_PATCH
LOGICAL                :: LFORC_MEASURE
LOGICAL                :: LMEB_LITTER
LOGICAL                :: LMEB_GNDRES
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_MEB_ISBA/LMEB_PATCH,LFORC_MEASURE,LMEB_LITTER,LMEB_GNDRES
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_ISBA_MEB',0,ZHOOK_HANDLE)
!
LMEB_PATCH(:) =.FALSE.
LFORC_MEASURE =.FALSE.
LMEB_LITTER   =.FALSE.
LMEB_GNDRES   =.FALSE.
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
CALL POSNAM(ILUNAM,'NAM_MEB_ISBA',GFOUND,KLUOUT)
IF (GFOUND) THEN
   READ(UNIT=ILUNAM,NML=NAM_MEB_ISBA)
ELSE
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* LMEB is activated in NAM_ISBA         *'
  WRITE(ILUOUT,*) '* But NAM_MEB_ISBA is not defined       *'
  WRITE(ILUOUT,*) '* Check your namelist                   *'
  WRITE(ILUOUT,*) '*****************************************'
  CALL ABOR1_SFX('PGD_ISBA: NAM_MEB_ISBA and LMEB_PATCH not defined')
ENDIF
!
CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
OMEB_PATCH(:) = LMEB_PATCH(:)
!
OFORC_MEASURE = LFORC_MEASURE
OMEB_LITTER   = LMEB_LITTER
OMEB_GNDRES   = LMEB_GNDRES
!
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_ISBA_MEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_ISBA_MEB
END MODULE MODI_READ_NAM_PGD_ISBA_MEB
