!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_SURF(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
!
USE MODD_SURF_ATM,       ONLY : XCISMIN, XVMODMIN, LALDTHRES,               &
                                   LDRAG_COEF_ARP, LALDZ0H, LNOSOF,         &
                                   LCPL_GCM, XEDB, XEDC, XEDD, XEDK,        &
                                   XUSURIC, XUSURID, XUSURICL,              &
                                   XVCHRNK, XVZ0CM, XRIMAX, XDELTA_MAX,     &
                                   XWINDMIN, LVZIUSTAR0_ARP,                &
                                   XRZHZ0M, XVZIUSTAR0, LRRGUST_ARP,        &
                                   XRRSCALE, XRRGAMMA, XUTILGUST, LCPL_ARP, &
                                   LQVNPLUS, LVERTSHIFT, LVSHIFT_LW,        &
                                   LVSHIFT_PRCP,                            &
                                   XCO2UNCPL   
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE, LSPLIT_PATCH                                    
!
USE MODI_DEFAULT_SURF_ATM
USE MODI_DEFAULT_WRITE_SURF_ATM
USE MODI_READ_DEFAULT_SURF_ATM
USE MODI_READ_SURF_ATM_CONF
!
USE MODI_INI_CSTS
USE MODI_READ_NAM_WRITE_COVER_TEX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
REAL    :: ZOUT_TSTEP
INTEGER :: ILUNAM         ! logical unit of namelist file
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SURF',0,ZHOOK_HANDLE)
 CALL DEFAULT_SURF_ATM(ZOUT_TSTEP,XCISMIN,XVMODMIN,LALDTHRES,     &
                         LDRAG_COEF_ARP, LALDZ0H, LNOSOF,         &
                         LCPL_GCM, XEDB, XEDC, XEDD, XEDK,        &
                         XUSURIC, XUSURID, XUSURICL,              &
                         XVCHRNK, XVZ0CM, XRIMAX, XDELTA_MAX,     &
                         XWINDMIN,                                &
                         LVZIUSTAR0_ARP,                          &
                         XRZHZ0M, XVZIUSTAR0, LRRGUST_ARP,        &
                         XRRSCALE, XRRGAMMA,XUTILGUST, LCPL_ARP,  &
                         LQVNPLUS, LVERTSHIFT, LVSHIFT_LW,        &
                         LVSHIFT_PRCP, XCO2UNCPL                  )
!                       
 CALL DEFAULT_WRITE_SURF_ATM(LNOWRITE_CANOPY, LNOWRITE_TEXFILE, LSPLIT_PATCH)
!
 CALL READ_DEFAULT_SURF_ATM(HPROGRAM)
!
 CALL READ_SURF_ATM_CONF(HPROGRAM)
!
!
CPROGNAME=HPROGRAM
 CALL INI_CSTS
!
 CALL READ_NAM_WRITE_COVER_TEX(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SURF',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE READ_NAMELISTS_SURF
