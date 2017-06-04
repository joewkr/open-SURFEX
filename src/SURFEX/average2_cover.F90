!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE AVERAGE2_COVER (U, HPROGRAM)
!     #########################
!
!!**** *AVERAGE2_COVER* computes the cover fractions
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_PGDWORK,   ONLY : NSIZE, XSUMVAL, XPREC
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_PGD_GRID,       ONLY : CGRID
!
USE MODI_SUM_ON_ALL_PROCS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZUNITY
!
REAL :: ZINT
INTEGER :: JI, JJ
INTEGER :: JCOV ! loop counter on cover classes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Average values
!            --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_COVER',0,ZHOOK_HANDLE)
ALLOCATE(ZUNITY(SIZE(NSIZE)))
ZUNITY (:) = 0.
!
ALLOCATE(U%XCOVER(SIZE(NSIZE,1),SIZE(XSUMVAL,2)))
!
DO JCOV=1,SIZE(XSUMVAL,2)
  WHERE (NSIZE(:,1)/=0)
    U%XCOVER(:,JCOV) = XSUMVAL(:,JCOV) /NSIZE(:,1)
    ZUNITY(:)=ZUNITY(:) + U%XCOVER(:,JCOV)
  ELSEWHERE
    U%XCOVER(:,JCOV) = 0.
  END WHERE
END DO
!
DO JCOV=1,SIZE(U%XCOVER,2)
  WHERE (NSIZE(:,1) /=0 )
    U%XCOVER(:,JCOV)=U%XCOVER(:,JCOV) / ZUNITY(:)
  END WHERE
END DO
!
DO JJ=1,SIZE(U%XCOVER,2)
  DO JI = 1,SIZE(U%XCOVER,1)

    ZINT = AINT(U%XCOVER(JI,JJ))
    IF (U%XCOVER(JI,JJ)/=ZINT) THEN
      U%XCOVER(JI,JJ) = ZINT + ANINT((U%XCOVER(JI,JJ)-ZINT)*XPREC)/XPREC
    ENDIF

  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZUNITY)
IF (LHOOK) CALL DR_HOOK('AVERAGE2_COVER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_COVER
