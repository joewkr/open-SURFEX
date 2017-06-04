!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_TEB_GREENROOF
!     ##################
!
!!****  *MODN_PREP_TEB_GREENROOF* - declaration of namelist NAM_PREP_TEB_GREENROOF
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify the namelist
!       NAM_PREP_TEB_GREENROOF
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!    A. Lemonsu & C. de Munck    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2011                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_TEB_GREENROOF,  ONLY : CFILE_GR, CTYPE, CFILEPGD_GR, CTYPEPGD,     &
                                     CFILE_HUG_GR, CTYPE_HUG,        &
                                     CFILE_HUG_SURF_GR, CFILE_HUG_ROOT_GR, CFILE_HUG_DEEP_GR, &
                                     XHUG_SURF_GR, XHUG_ROOT_GR, XHUG_DEEP_GR,                 &
                                     XHUGI_SURF_GR, XHUGI_ROOT_GR, XHUGI_DEEP_GR,              &
                                     CFILE_TG_GR, CTYPE_TG,                             &
                                     CFILE_TG_SURF_GR, CFILE_TG_ROOT_GR, CFILE_TG_DEEP_GR,    &
                                     XTG_SURF_GR, XTG_ROOT_GR, XTG_DEEP_GR   

!
IMPLICIT NONE
!
NAMELIST/NAM_PREP_TEB_GREENROOF/CFILE_GR, CTYPE, CFILEPGD_GR, CTYPEPGD,     &
                                CFILE_HUG_GR, CTYPE_HUG,        &
                                CFILE_HUG_SURF_GR, CFILE_HUG_ROOT_GR, CFILE_HUG_DEEP_GR, &
                                XHUG_SURF_GR, XHUG_ROOT_GR, XHUG_DEEP_GR,                &
                                XHUGI_SURF_GR, XHUGI_ROOT_GR, XHUGI_DEEP_GR,             &
                                CFILE_TG_GR, CTYPE_TG,                              &
                                CFILE_TG_SURF_GR, CFILE_TG_ROOT_GR, CFILE_TG_DEEP_GR,    &
                                XTG_SURF_GR, XTG_ROOT_GR, XTG_DEEP_GR   
!
END MODULE MODN_PREP_TEB_GREENROOF
