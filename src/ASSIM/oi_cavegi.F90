!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_CAVEGI(PVGAT1,PVGAT2,PVGAT3,PVGBT1,PVGBT2,PVGBT3,PVGCT1,PVGCT2,       &
                     PVGAH1,PVGAH2,PVGAH3,PVGBH1,PVGBH2,PVGBH3,PVGCH1,PVGCH2,       &
                     PSIGT2MP,PSIGHP2,OSGOBS)   
!****------------------------------------------------------------------------
USE MODD_ASSIM, ONLY : XSIGHP1, XSIGT2MR, XSIGH2MR, XSIGT2MO, XSIGH2MO, XREPS3
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER :: J
! 
REAL ,INTENT(OUT) :: PVGAT1(24),PVGAT2(24),PVGAT3(24)
REAL ,INTENT(OUT) :: PVGBT1(24),PVGBT2(24),PVGBT3(24)
REAL ,INTENT(OUT) :: PVGCT1(24),PVGCT2(24)
REAL ,INTENT(OUT) :: PVGAH1(24),PVGAH2(24),PVGAH3(24)
REAL ,INTENT(OUT) :: PVGBH1(24),PVGBH2(24),PVGBH3(24)
REAL ,INTENT(OUT) :: PVGCH1(24),PVGCH2(24)
REAL ,INTENT(OUT) :: PSIGT2MP(24),PSIGHP2(24)
!
LOGICAL :: OSGOBS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!**---------------------------------------------------------------------
!**  1. Initialisation des polynomes bruts et des champs de reference.
!**     -------------------------------------------------------------
!
! lecture des coefficients polynomiaux
!
IF (LHOOK) CALL DR_HOOK('OI_CAVEGI',0,ZHOOK_HANDLE)
!
DO J=1,24

  READ(61,'(6X,8F10.4)') PVGAT1(J),PVGAT2(J),PVGAT3(J),&
                         PVGBT1(J),PVGBT2(J),PVGBT3(J),&
                         PVGCT1(J),PVGCT2(J)    

ENDDO
!
DO J=1,24

  READ(61,'(6X,8F10.4)') PVGAH1(J),PVGAH2(J),PVGAH3(J),&
                         PVGBH1(J),PVGBH2(J),PVGBH3(J),&
                         PVGCH1(J),PVGCH2(J)    

ENDDO
!
DO J = 1,24 

  PSIGT2MP(J) = MAX(.3 , 2.7*(1.0 - ((REAL(J)-15.)/9.)**2))   
  PSIGHP2 (J) = (REAL(J)*2.0/3. - 15.)*1.E-2

ENDDO
!
!**---------------------------------------------------------------------
!**  3. Initialisation des variables internes.
!**     -------------------------------------
!
OSGOBS = XSIGT2MO > 0.0 .AND. XSIGH2MO > 0.0 .AND. &
         (ABS(XSIGH2MO-XSIGH2MR) > XREPS3 .OR. ABS(XSIGT2MO-XSIGT2MR) > XREPS3)  

IF (LHOOK) CALL DR_HOOK('OI_CAVEGI',1,ZHOOK_HANDLE)
!
!**---------------------------------------------------------------------
END SUBROUTINE OI_CAVEGI


