!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_CPL_GCM_n (HSELECT, U, HPROGRAM)
!     #######################################
!
!!****  *WRITESURF_CPL_GCM_n* - routine to write physical fields into
!!                              the restart file for ARPEGE/ALADIN run
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to store the 
!!       physical fields into the restart file . Indeed, 
!!       when ARPEGE/ALADIN is used, theses fields 
!!       are not initialized at the begin of a run.
!!
!!
!!**  METHOD
!!    ------
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_ATM,      ONLY : LCPL_GCM
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER           :: IRESP          ! Error code after redding
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_PRECIP_N',0,ZHOOK_HANDLE)
!
YRECFM='LCPL_GCM'
YCOMMENT='flag to store physical fields in restart file'
CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,LCPL_GCM,IRESP,HCOMMENT=YCOMMENT)
!
IF(LCPL_GCM)THEN
!
   YRECFM='RAIN_GCM'
   YCOMMENT='RAINFALL FOR RESTART (kg/m2/s)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,U%XRAIN(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='SNOW_GCM'
   YCOMMENT='SNOWFALL FOR RESTART (kg/m2/s)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,U%XSNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='Z0_GCM'
   YCOMMENT='Z0 FOR RESTART (m)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,U%XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='Z0H_GCM'
   YCOMMENT='Z0H FOR RESTART (m)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,U%XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='QS_GCM'
   YCOMMENT='QS FOR RESTART (kg/kg)'
   CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,U%XQSURF(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PRECIP_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_CPL_GCM_n
