!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SSO_n (KSIZE_FULL, PSEA, USS, HPROGRAM)
!     ################################
!
!!****  *READ_SSO_n* - routine to read a file for
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
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_READ_SURF
!
USE MODD_SURF_PAR, ONLY : XUNDEF
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
INTEGER, INTENT(IN) :: KSIZE_FULL
REAL, DIMENSION(:), INTENT(IN) :: PSEA
!
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

!
INTEGER           :: IRESP          ! Error code after redding
! 
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       2.     Orography :
!               ---------
!
!
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',0,ZHOOK_HANDLE)
IF(.NOT.ASSOCIATED(USS%XAVG_ZS)) ALLOCATE(USS%XAVG_ZS(KSIZE_FULL))
YRECFM='AVG_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAVG_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XSIL_ZS)) ALLOCATE(USS%XSIL_ZS(KSIZE_FULL))
YRECFM='SIL_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSIL_ZS(:),IRESP)
!
!
!*       3.     Subgrid Orography :
!               -----------------
!
!
IF(.NOT.ASSOCIATED(USS%XSSO_STDEV)) ALLOCATE(USS%XSSO_STDEV(KSIZE_FULL))
YRECFM='SSO_STDEV'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_STDEV(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_STDEV(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XMIN_ZS)) ALLOCATE(USS%XMIN_ZS(KSIZE_FULL))
YRECFM='MIN_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XMIN_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XMAX_ZS)) ALLOCATE(USS%XMAX_ZS(KSIZE_FULL))
YRECFM='MAX_ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XMAX_ZS(:),IRESP)
!
IF(.NOT.ASSOCIATED(USS%XSSO_ANIS)) ALLOCATE(USS%XSSO_ANIS(KSIZE_FULL))
YRECFM='SSO_ANIS'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_ANIS(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_ANIS(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XSSO_DIR)) ALLOCATE(USS%XSSO_DIR(KSIZE_FULL))
YRECFM='SSO_DIR'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_DIR(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_DIR(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XSSO_SLOPE)) ALLOCATE(USS%XSSO_SLOPE(KSIZE_FULL))
YRECFM='SSO_SLOPE'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XSSO_SLOPE(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XSSO_SLOPE(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       3.     Subgrid Orography roughness:
!               ---------------------------
!
!
IF(.NOT.ASSOCIATED(USS%XHO2IP)) ALLOCATE(USS%XHO2IP(KSIZE_FULL))
YRECFM='HO2IP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2IP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2IP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2JP)) ALLOCATE(USS%XHO2JP(KSIZE_FULL))
YRECFM='HO2JP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2JP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2JP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2IM)) ALLOCATE(USS%XHO2IM(KSIZE_FULL))
YRECFM='HO2IM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2IM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2IM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XHO2JM)) ALLOCATE(USS%XHO2JM(KSIZE_FULL))
YRECFM='HO2JM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XHO2JM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XHO2JM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSIP)) ALLOCATE(USS%XAOSIP(KSIZE_FULL))
YRECFM='AOSIP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSIP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSIP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSJP)) ALLOCATE(USS%XAOSJP(KSIZE_FULL))
YRECFM='AOSJP'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSJP(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSJP(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSIM)) ALLOCATE(USS%XAOSIM(KSIZE_FULL))
YRECFM='AOSIM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSIM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSIM(:) = XUNDEF
!
IF(.NOT.ASSOCIATED(USS%XAOSJM)) ALLOCATE(USS%XAOSJM(KSIZE_FULL))
YRECFM='AOSJM'
 CALL READ_SURF(HPROGRAM,YRECFM,USS%XAOSJM(:),IRESP)
WHERE (PSEA(:) == 1.) USS%XAOSJM(:) = XUNDEF
IF (LHOOK) CALL DR_HOOK('READ_SSO_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_SSO_n
