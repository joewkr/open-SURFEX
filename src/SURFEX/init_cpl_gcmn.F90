!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_CPL_GCM_n (U, HPROGRAM,HINIT)
!     ########################################
!
!!****  *INIT_CPL_GCM_n* - routine to read  physical fields into  
!!                         the restart file for ARPEGE/ALADIN run
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise some 
!!       physical fields. Indeed, when ARPEGE/ALADIN is used, 
!!       these field is not initialize at the begin of a run.
!!
!!
!!**  METHOD
!!    ------
!!      The data are read in the initial surface file :
!!        - 2D data fields
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_READ_SURF
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
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=3),  INTENT(IN)  :: HINIT    ! choice of fields to initialize
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP      ! Error code after redding
! 
CHARACTER(LEN=12) :: YRECFM     ! Name of the article to be read
!
INTEGER           :: IVERSION   ! surface version
!
LOGICAL           :: LREAD      ! work key
LOGICAL           :: GCPL_GCM   ! work key
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_CPL_GCM_N',0,ZHOOK_HANDLE)
!
GCPL_GCM = .FALSE.
!
IF (HINIT=='PGD') THEN
!     
   ALLOCATE(U%XRAIN (0))
   ALLOCATE(U%XSNOW (0))           
   ALLOCATE(U%XZ0   (0))           
   ALLOCATE(U%XZ0H  (0))           
   ALLOCATE(U%XQSURF(0))           
!     
ELSE
!
   ALLOCATE(U%XRAIN (U%NSIZE_FULL))
   ALLOCATE(U%XSNOW (U%NSIZE_FULL))
   ALLOCATE(U%XZ0   (U%NSIZE_FULL))
   ALLOCATE(U%XZ0H  (U%NSIZE_FULL))
   ALLOCATE(U%XQSURF(U%NSIZE_FULL))
!
   U%XRAIN (:) = 0.0
   U%XSNOW (:) = 0.0
   U%XZ0   (:) = 0.001
   U%XZ0H  (:) = 0.001
   U%XQSURF(:) = 0.0
!
ENDIF
!
YRECFM='VERSION'
CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
LREAD=(HINIT/='PGD'.AND.HINIT/='PRE'.AND.IVERSION>=8)
!
IF (LREAD) THEN
   YRECFM='LCPL_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,GCPL_GCM,IRESP)
ENDIF  
!
IF (LREAD.AND.GCPL_GCM) THEN
!
   YRECFM='RAIN_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,U%XRAIN(:),IRESP)
!
   YRECFM='SNOW_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,U%XSNOW(:),IRESP)
!
   YRECFM='Z0_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,U%XZ0(:),IRESP)
!
   YRECFM='Z0H_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,U%XZ0H(:),IRESP)
!
   YRECFM='QS_GCM'
   CALL READ_SURF(HPROGRAM,YRECFM,U%XQSURF(:),IRESP)
!
ENDIF        
!
IF (LHOOK) CALL DR_HOOK('INIT_CPL_GCM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_CPL_GCM_n
