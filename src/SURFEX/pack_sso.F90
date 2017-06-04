!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PACK_SSO(USS,HPROGRAM,KMASK,ISS)
!     #########################################
!
!!****  *PACK_SSO* - routine to initialise the horizontal grid of a scheme
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_PACK_SAME_RANK
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
TYPE(SSO_t), INTENT(INOUT) :: USS
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM   ! calling program
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
TYPE(SSO_t), INTENT(INOUT) :: ISS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*       1.    Reading of type of grid
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_SSO',0,ZHOOK_HANDLE)
!
!*    1.      Number of points and packing
!             ----------------------------
!
CALL PACK_SAME_RANK(KMASK,USS%XAOSIP(:),ISS%XAOSIP(:))
CALL PACK_SAME_RANK(KMASK,USS%XAOSIM(:),ISS%XAOSIM(:))
CALL PACK_SAME_RANK(KMASK,USS%XAOSJP(:),ISS%XAOSJP(:))
CALL PACK_SAME_RANK(KMASK,USS%XAOSJM(:),ISS%XAOSJM(:))
!
CALL PACK_SAME_RANK(KMASK,USS%XHO2IP(:),ISS%XHO2IP(:))
CALL PACK_SAME_RANK(KMASK,USS%XHO2IM(:),ISS%XHO2IM(:))
CALL PACK_SAME_RANK(KMASK,USS%XHO2JP(:),ISS%XHO2JP(:))
CALL PACK_SAME_RANK(KMASK,USS%XHO2JM(:),ISS%XHO2JM(:))
!
CALL PACK_SAME_RANK(KMASK,USS%XSSO_STDEV(:),ISS%XSSO_STDEV(:))
CALL PACK_SAME_RANK(KMASK,USS%XSSO_SLOPE(:),ISS%XSSO_SLOPE(:))

!
IF (LHOOK) CALL DR_HOOK('PACK_SSO',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE PACK_SSO
