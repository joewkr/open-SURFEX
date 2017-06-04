!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PRE_WATF_DAT_CONF(HPROGRAM,KLUOUT,TPTIME)
!     #######################################################
!
!!****  *READ_PRE_WATF_DAT_CONF* - routine to read the date in namelist
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODN_PREP_WATFLUX
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM    ! program calling WATFLUX
INTEGER,           INTENT(IN)  :: KLUOUT      ! logical unit of output listing
TYPE (DATE_TIME),  INTENT(OUT) :: TPTIME      ! current date and time
REAL(KIND=JPRB) :: ZHOOK_HANDLE


!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!
TPTIME%TDATE%YEAR  = NYEAR
TPTIME%TDATE%MONTH = NMONTH
TPTIME%TDATE%DAY   = NDAY
IF (LHOOK) CALL DR_HOOK('READ_PRE_WATF_DAT_CONF',0,ZHOOK_HANDLE)
TPTIME%TIME       = XTIME
IF (LHOOK) CALL DR_HOOK('READ_PRE_WATF_DAT_CONF',1,ZHOOK_HANDLE)
!
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_PRE_WATF_DAT_CONF
