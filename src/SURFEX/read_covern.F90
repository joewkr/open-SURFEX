!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_COVER_n (DTCO, U, &
                               HPROGRAM)
!     ################################
!
!!****  *READ_COVER_n* - routine to read a file for
!!                         physiographic data file of model _n 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise the 
!!       physiographic data file.
!!
!!
!!**  METHOD
!!    ------
!!      The data are read in the initial surface file :
!!        - 2D physiographic data fields
!!          
!!      It does not read the grid definition. This should have been
!!      read already.
!!
!!    EXTERNAL
!!    --------
!!      
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ASSIM, ONLY : LASSIM, LREAD_ALL
!
USE MODD_DATA_COVER_PAR, ONLY : NBARE_SOIL, JPCOVER
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_LCOVER
USE MODI_READ_SURF
USE MODI_CONVERT_COVER_FRAC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

INTEGER           :: IRESP          ! Error code after redding
! 
INTEGER           :: IVERSION       ! surface version
!
LOGICAL :: GREAD_ALL
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!*       2.1    Cover classes :
!               -------------
!
IF (LHOOK) CALL DR_HOOK('READ_COVER_N',0,ZHOOK_HANDLE)
!
IF (LASSIM) THEN
  GREAD_ALL = LREAD_ALL
  LREAD_ALL = .TRUE.
ENDIF
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
ALLOCATE(U%LCOVER(JPCOVER))
 CALL READ_LCOVER(HPROGRAM,U%LCOVER)
!
!
ALLOCATE(U%XCOVER(U%NSIZE_FULL,COUNT(U%LCOVER)))
 CALL READ_SURF_COV(HPROGRAM,'COVER',U%XCOVER(:,:),U%LCOVER,IRESP)
!
!*       2.1    Fractions :
!               ---------
!
ALLOCATE(U%XSEA   (U%NSIZE_FULL))
ALLOCATE(U%XNATURE(U%NSIZE_FULL))
ALLOCATE(U%XWATER (U%NSIZE_FULL))
ALLOCATE(U%XTOWN  (U%NSIZE_FULL))
!
IF (IVERSION>=7) THEN
  !
  CALL READ_SURF(HPROGRAM,'FRAC_SEA   ',U%XSEA,   IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_NATURE',U%XNATURE,IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_WATER ',U%XWATER, IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_TOWN  ',U%XTOWN,  IRESP)
  !
ELSE
  CALL CONVERT_COVER_FRAC(DTCO,U%XCOVER,U%LCOVER,U%XSEA,U%XNATURE,U%XTOWN,U%XWATER)
ENDIF
!
!*       2.2    Orography :
!               ---------
!
!
ALLOCATE(U%XZS(U%NSIZE_FULL))
YRECFM='ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,U%XZS(:),IRESP)
!
IF (LASSIM) LREAD_ALL = GREAD_ALL
!
IF (LHOOK) CALL DR_HOOK('READ_COVER_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_COVER_n
