!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CH_INIT_NAMES (KLUOUT,HSV, SV, OVARSIGI, OVARSIGJ)  
!!    ###########################################
!!
!!*** *CH_INIT_NAMES*
!!
!!    PURPOSE
!!    -------
!!      Read and filter all chemical species into the CSV array
!!     initialize NSV_CHSBEG and  NSV_CHSEND index for the begin and the ending chemical index
!!     
!!
!!    REFERENCE
!!    ---------
!!    
!!    AUTHOR
!!    ------
!!    P. Tulet    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 16/10/01
!!    01/12/03    (D.Gazen) change emissions handling for surf. externalization
!!    01/06/05    (P.Tulet) add aerosols list
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_CHS_AEROSOL
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!

INTEGER,                         INTENT(IN)  :: KLUOUT ! output listing channel
 CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN)  :: HSV    ! name of chemical species
                                                       ! with character # (gas chemistry )
                                                       ! and  character @ (aerosols)
TYPE(SV_t), INTENT(INOUT) :: SV
!
LOGICAL,                         INTENT(OUT) :: OVARSIGI, OVARSIGJ ! type of standard deviation
!
!*      0.2    declarations of local variables
INTEGER :: JSV  !! loop  NBEQ
 CHARACTER        :: YRC1
 CHARACTER(LEN=5) :: YRC2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CH_INIT_NAMES',0,ZHOOK_HANDLE)
SV%NBEQ = 0
SV%NAEREQ = 0
SV%NSV_CHSBEG = 0
SV%NSV_AERBEG = 0
SV%NSV_CHSEND = 0
SV%NSV_AEREND = 0
OVARSIGI = .FALSE.
OVARSIGJ = .FALSE.
NSOA = 0


DO JSV=1, SIZE(HSV)

  SV%CSV(JSV) = HSV(JSV)
  YRC1= HSV(JSV)(1:1)
  YRC2 = HSV(JSV)(2:)

  IF (YRC1 == '#') THEN
    SV%CSV(JSV) = TRIM(YRC2)
    SV%NBEQ = SV%NBEQ + 1
    IF (SV%NBEQ == 1) SV%NSV_CHSBEG=JSV
  ELSE IF (YRC1 == '@') THEN
    SV%CSV(JSV) = TRIM(YRC2)
    SV%NAEREQ = SV%NAEREQ + 1
    IF (SV%NAEREQ == 1) SV%NSV_AERBEG=JSV
    IF (SV%CSV(JSV) == "M6I") OVARSIGI = .TRUE.
    IF (SV%CSV(JSV) == "M6J") OVARSIGJ = .TRUE.
    IF (SV%CSV(JSV) == "SOA1I") NSOA = 10
  ENDIF

ENDDO

SV%NSV_CHSEND = SV%NSV_CHSBEG + SV%NBEQ -1
SV%NSV_AEREND = SV%NSV_AERBEG + SV%NAEREQ -1

IF (SV%NAEREQ .GT. 0) THEN
DO JSV=1, size(SV%CSV)
   IF (TRIM(SV%CSV(JSV)) == "M0I") JP_CH_M0i=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "M0J") JP_CH_M0j=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "M6I") JP_CH_M6i=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "M6J") JP_CH_M6j=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "H2OI") JP_CH_H2Oi=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "H2OJ") JP_CH_H2Oj=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "SO4I") JP_CH_SO4i=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "SO4J") JP_CH_SO4j=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "NO3I") JP_CH_NO3i=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "NO3J") JP_CH_NO3j=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "NH3I") JP_CH_NH3i=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "NH3J") JP_CH_NH3j=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "OCI") JP_CH_OCi=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "OCJ") JP_CH_OCj=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "BCI") JP_CH_BCi=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "BCJ") JP_CH_BCj=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "DSTI") JP_CH_DSTi=JSV-SV%NSV_CHSEND
   IF (TRIM(SV%CSV(JSV)) == "DSTJ") JP_CH_DSTj=JSV-SV%NSV_CHSEND
END DO

END IF
IF (LHOOK) CALL DR_HOOK('CH_INIT_NAMES',1,ZHOOK_HANDLE)

!
END SUBROUTINE CH_INIT_NAMES
