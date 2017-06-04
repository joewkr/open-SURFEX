!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_DUMMY_n (HSELECT, DUU, HPROGRAM)
!     ##########################################
!
!!****  *WRITESURF_DUMMY_n* - routine to write dummy surface fields
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2004
!!      P.Tulet     2015  Bug depassement de tableau YRECFM
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : DUMMY_SURF_FIELDS_t
!
USE MODI_WRITE_SURF
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
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(DUMMY_SURF_FIELDS_t), INTENT(INOUT) :: DUU
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JDUMMY         ! loop counter
!
 CHARACTER(LEN=20 ):: YSTRING20      ! string
 CHARACTER(LEN=3  ):: YSTRING03      ! string
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Number of dummy fields :
!               ----------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_DUMMY_N',0,ZHOOK_HANDLE)
YRECFM='DUMMY_GR_NBR'
YCOMMENT=' '
!
 CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,DUU%NDUMMY_NBR,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Dummy fields :
!               ------------
!
DO JDUMMY=1,DUU%NDUMMY_NBR
  !
  WRITE(YRECFM,'(A8,I3.3,A1)') 'DUMMY_GR',JDUMMY,' '
  YSTRING20=DUU%CDUMMY_NAME(JDUMMY)
  YSTRING03=DUU%CDUMMY_AREA(JDUMMY)
  YCOMMENT='X_Y_'//YRECFM//YSTRING20//YSTRING03//  &
             '                                                             '  
  CALL WRITE_SURF(HSELECT, &
                 HPROGRAM,YRECFM,DUU%XDUMMY_FIELDS(:,JDUMMY),IRESP,HCOMMENT=YCOMMENT)
END DO
IF (LHOOK) CALL DR_HOOK('WRITESURF_DUMMY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_DUMMY_n
