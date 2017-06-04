!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_IDEAL(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODN_IDEAL_FLUX, ONLY : NFORCF, NFORCT, XTIMEF, XTIMET, XSFTH, XSFTQ, XSFCO2, &
                            CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD
!
USE MODI_DEFAULT_IDEAL_FLUX
USE MODI_READ_IDEAL_FLUX_CONF
!
!--------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------
!                  
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL',0,ZHOOK_HANDLE)
 CALL DEFAULT_IDEAL_FLUX(NFORCF, NFORCT, XTIMEF, XTIMET, XSFTH, XSFTQ, XSFCO2, &
                        CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD)
!
 CALL READ_IDEAL_FLUX_CONF(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------
!
END SUBROUTINE READ_NAMELISTS_IDEAL
