!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      FUNCTION SUM_ON_ALL_PROCS(HPROGRAM,HGRID,OIN,HNAME) RESULT(KOUT)
!     #######################################################
!
!
!!****  *SUM_ON_ALL_PROCS* - sums the values of the integers provided on each processor
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!!      S.Senesi    08/2015 : Adapt to XIOS output mode
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_CONF, ONLY : CSOFTWARE
!
#ifdef SFX_OL
USE MODI_SUM_ON_ALL_PROCS_OL
#endif
#ifdef SFX_MNH
USE MODI_SUM_ON_ALL_PROCS_MNH
USE MODI_SUM_ON_ALL_PROCS_MNH_HAL
#endif
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),      INTENT(IN) :: HPROGRAM ! program calling SURFEX
 CHARACTER(LEN=10),     INTENT(IN) :: HGRID    ! grid type
LOGICAL, DIMENSION(:), INTENT(IN) :: OIN
 CHARACTER(LEN=3),      INTENT(IN), OPTIONAL :: HNAME ! pour la maquette offline
INTEGER                           :: KOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER                       :: ISIZE
INTEGER, DIMENSION(SIZE(OIN)) :: IIN ! 1 if OIN true, 0 otherwise
!
 CHARACTER(LEN=3) :: YNAME
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUM_ON_ALL_PROCS',0,ZHOOK_HANDLE)
!
ISIZE = SIZE(OIN)
IIN   = 0
WHERE(OIN) IIN = 1
!
IF (HPROGRAM=='MESONH') THEN
#ifdef SFX_MNH
  YNAME = ' '
  IF (PRESENT(HNAME)) YNAME = HNAME
  IF (YNAME=="HAL") THEN
    CALL SUM_ON_ALL_PROCS_MNH_HAL(ISIZE,IIN,KOUT)
  ELSE
    CALL SUM_ON_ALL_PROCS_MNH(ISIZE,IIN,KOUT)
  ENDIF
#endif
ELSE IF (HPROGRAM=='AROME ' .OR. (HPROGRAM=='XIOS  '.AND.TRIM(CSOFTWARE)/="OFFLINE")) THEN
#ifdef SFX_ARO
  KOUT = MAX(COUNT(OIN),1)   ! to be coded properly in AROME
#endif
ELSE
#ifdef SFX_OL
  ! to be coded properly once Offline version is parallelized
  YNAME = ' '
  IF (PRESENT(HNAME)) YNAME = HNAME
  CALL SUM_ON_ALL_PROCS_OL(HGRID,ISIZE,IIN,KOUT,YNAME)
#endif
END IF
!
IF (LHOOK) CALL DR_HOOK('SUM_ON_ALL_PROCS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION SUM_ON_ALL_PROCS
