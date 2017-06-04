!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE COMPARE_OROGRAPHY (DTCO, U, HPROGRAM, OSURFZS, PDELT_ZSMAX  )
!**************************************************************************
!
!!    PURPOSE
!!    -------
!!        Check consistency orographies read from forcing file and from initial file
!!
!!**  METHOD
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
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_INIT_IO_SURF_n
USE MODI_READ_SURF
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_FORC_ATM,   ONLY : XZS
USE MODD_SURF_CONF,  ONLY : CPROGNAME
!         
USE MODI_SET_SURFEX_FILEIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
! global variables
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6)    ,INTENT(IN)  :: HPROGRAM
REAL                ,INTENT(IN)  :: PDELT_ZSMAX
LOGICAL             ,INTENT(IN)  :: OSURFZS

! local variables
INTEGER                          :: ILUOUT
INTEGER                          :: IRET
REAL, DIMENSION(SIZE(XZS,1))     :: ZS1       ! orography read from FORCING.nc
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COMPARE_OROGRAPHY',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
! read orography
! 
!  orography from initial file
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ') 
 CALL READ_SURF(HPROGRAM,'ZS', ZS1, IRET)
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
IF (OSURFZS) THEN
  CALL GET_LUOUT('ASCII ',ILUOUT)
  WRITE(ILUOUT,*)' OROGRAPHY READ FROM INITIAL FILE'
  XZS(:) = ZS1(:)
ELSEIF (MAXVAL(ABS(XZS(:)-ZS1(:))) > PDELT_ZSMAX) THEN
  CALL GET_LUOUT('ASCII ',ILUOUT)
  WRITE(ILUOUT,*)' DIFFERENCE OF OROGRAPHY TOO BIG BETWEEN FORCING AND INITIAL FILE'
  WRITE(ILUOUT,*)' Maximum orography difference allowed (m) : ', PDELT_ZSMAX
  WRITE(ILUOUT,*)' Maximum orography difference         (m) : ', MAXVAL(ABS(XZS(:)-ZS1(:)))
  CALL ABOR1_SFX('COMPARE_OROGRAPHY: DIFFERENCE OF OROGRAPHY TOO BIG BETWEEN FORCING AND INITIAL FILE')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COMPARE_OROGRAPHY',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPARE_OROGRAPHY
