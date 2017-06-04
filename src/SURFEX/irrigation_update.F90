!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE IRRIGATION_UPDATE (NAG, NPE, KPATCH, PTSTEP, KMONTH, KDAY, PTIME) 
!     ####################################################################
!
!!****  *IRRIGATION_UPDATE* - routine to update irrigation fields
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
!!      P. Le Moigne  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_n, ONLY : ISBA_NPE_t
USE MODD_AGRI_n, ONLY : AGRI_NP_t
!
USE MODD_AGRI,   ONLY   : JPSTAGE, XTHRESHOLD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
TYPE(AGRI_NP_t), INTENT(INOUT) :: NAG
!
INTEGER, INTENT(IN) :: KPATCH
REAL,    INTENT(IN)  :: PTSTEP, PTIME
INTEGER, INTENT(IN)  :: KMONTH, KDAY
!
INTEGER              :: JI, JP
LOGICAL              :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! Mask to realize update only once a day
!
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',0,ZHOOK_HANDLE)
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
DO JP = 1,KPATCH
  !
  IF (GMASK) THEN
    !
    WHERE( (NPE%AL(JP)%XIRRIG(:).GT.0.).AND.(NAG%AL(JP)%LIRRIDAY(:)) .AND.(NAG%AL(JP)%NIRRINUM(:).LT.JPSTAGE))
      NAG%AL(JP)%NIRRINUM (:) = NAG%AL(JP)%NIRRINUM(:) + 1
      NAG%AL(JP)%LIRRIDAY (:) = .FALSE.
    ENDWHERE
    !
    DO JI = 1,SIZE(NPE%AL(JP)%XIRRIG,1)
      NAG%AL(JP)%XTHRESHOLDSPT(JI)= XTHRESHOLD(NAG%AL(JP)%NIRRINUM(JI))
    ENDDO
    !
  END IF
  !
  ! Reinitialization of irrigation stage (necessary for runs from August to August)
  !
  IF((KMONTH==1).AND.(KDAY==1)) NAG%AL(JP)%NIRRINUM(:) = 1
  !
  NAG%AL(JP)%LIRRIGATE(:) = .FALSE.
  !
  DO JI = 1,SIZE(NPE%AL(JP)%XIRRIG,1)
    !
    ! Activate irrigation after seeding date
    !
    IF (KMONTH == NPE%AL(JP)%TSEED(JI)%TDATE%MONTH .AND. KDAY .GE. NPE%AL(JP)%TSEED(JI)%TDATE%DAY) THEN
      NAG%AL(JP)%LIRRIGATE(JI) = .TRUE.
    END IF
    IF (KMONTH > NPE%AL(JP)%TSEED(JI)%TDATE%MONTH) THEN
      NAG%AL(JP)%LIRRIGATE(JI) = .TRUE.
    END IF
    !
    ! Stop irrigation after reaping date
    !
    IF (KMONTH == NPE%AL(JP)%TREAP(JI)%TDATE%MONTH .AND. KDAY .GT. NPE%AL(JP)%TREAP(JI)%TDATE%DAY) THEN
      NAG%AL(JP)%LIRRIGATE(JI) = .FALSE.
    END IF
    IF (KMONTH > NPE%AL(JP)%TREAP(JI)%TDATE%MONTH) THEN
      NAG%AL(JP)%LIRRIGATE(JI) = .FALSE.
    END IF
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE IRRIGATION_UPDATE
