!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    #####################
      MODULE MODN_SURF_ATM
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Namelist for wind threshold
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/2007
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_SURF_ATM, ONLY : XCISMIN, XVMODMIN, LALDTHRES, &
                            LDRAG_COEF_ARP, LALDZ0H,      &
                            LNOSOF, LCPL_GCM,             &
                            XEDB, XEDC, XEDD, XEDK,       &
                            XUSURIC, XUSURID, XUSURICL,   &
                            XVCHRNK, XVZ0CM, XDELTA_MAX,  &
                            XRIMAX, LVERTSHIFT,           &
                            LVZIUSTAR0_ARP, LRRGUST_ARP,  &
                            XVZIUSTAR0,XRZHZ0M,           &
                            XRRSCALE, XRRGAMMA,           &
                            XUTILGUST, LCPL_ARP, LQVNPLUS,&
                            LVSHIFT_LW, LVSHIFT_PRCP,     &
                            XCO2UNCPL         
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_SURF_ATM/XCISMIN, XVMODMIN, LALDTHRES, &
                         LDRAG_COEF_ARP, LALDZ0H,      &
                         LNOSOF, LCPL_GCM,             &
                         XEDB, XEDC, XEDD, XEDK,       &
                         XUSURIC, XUSURID, XUSURICL,   &
                         XVCHRNK, XVZ0CM, XDELTA_MAX,  &
                         XRIMAX, LVERTSHIFT,           &
                         LVZIUSTAR0_ARP, LRRGUST_ARP,  &
                         XVZIUSTAR0,XRZHZ0M,           &
                         XRRSCALE, XRRGAMMA,           &
                         XUTILGUST, LCPL_ARP, LQVNPLUS,&
                         LVSHIFT_LW, LVSHIFT_PRCP,     &
                         XCO2UNCPL         
!
END MODULE MODN_SURF_ATM
