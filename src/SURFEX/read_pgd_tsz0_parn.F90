!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TSZ0_PAR_n (DTZ, HPROGRAM)
!     ################################################
!
!!****  *READ_PGD_TSZ0_PAR_n* - reads SEAFLUX sst
!!                        
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     09/2007 
!!      P. Le Moigne 03/2015 tsz0 time management
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
USE MODD_DATA_TSZ0_n, ONLY : DATA_TSZ0_t
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
TYPE(DATA_TSZ0_t), INTENT(INOUT) :: DTZ
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IVERSION  ! surface version
INTEGER :: IBUGFIX   ! bugfix  of the old file
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TSZ0_PAR_N',0,ZHOOK_HANDLE)
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG', IBUGFIX ,IRESP)
!
IF (IVERSION.GT.7 .OR. (IVERSION==7 .AND. IBUGFIX.GT.1)) THEN
  YRECFM='ND_TSZ0_TIME'
  YCOMMENT = '(-)'
  CALL READ_SURF(HPROGRAM,YRECFM,DTZ%NTIME,IRESP,HCOMMENT=YCOMMENT)
ELSE 
  DTZ%NTIME=37
ENDIF
!
ALLOCATE(DTZ%XDATA_DTS   (DTZ%NTIME))
ALLOCATE(DTZ%XDATA_DHUGRD(DTZ%NTIME))
!
IF (IVERSION.GT.7 .OR. (IVERSION==7 .AND. IBUGFIX.GT.1)) THEN
  !
  YRECFM = 'D_DTS'
  YCOMMENT = 'X_Y_DATA_DTS'
  CALL READ_SURF(HPROGRAM,YRECFM,DTZ%XDATA_DTS(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
  !
  YRECFM='D_DHUGRD'
  YCOMMENT = 'X_Y_DATA_DHUGRD'
  CALL READ_SURF(HPROGRAM,YRECFM,DTZ%XDATA_DHUGRD(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
  !
ELSE
  !
  DTZ%XDATA_DTS   (:) = 0.0
  DTZ%XDATA_DHUGRD(:) = 0.0
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TSZ0_PAR_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_PGD_TSZ0_PAR_n
