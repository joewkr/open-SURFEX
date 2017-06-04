!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_TEB_GARDEN
!     ##################
!
!!****  *MODN_PREP_TEB_GARDEN* - declaration of namelist NAM_PREP_TEB_GARDEN
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist
!       NAM_PREP_TEB_GARDEN
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_TEB_GARDEN,  ONLY : CFILE_GD, CTYPE, CFILEPGD_GD, CTYPEPGD,        &
                                     CFILE_HUG_GD, CTYPE_HUG,                           &
                                     CFILE_HUG_SURF_GD, CFILE_HUG_ROOT_GD, CFILE_HUG_DEEP_GD, &
                                     XHUG_SURF_GD, XHUG_ROOT_GD, XHUG_DEEP_GD,                &
                                     XHUGI_SURF_GD, XHUGI_ROOT_GD, XHUGI_DEEP_GD,             &
                                     CFILE_TG_GD, CTYPE_TG,                             &
                                     CFILE_TG_SURF_GD, CFILE_TG_ROOT_GD, CFILE_TG_DEEP_GD,    &
                                     XTG_SURF_GD, XTG_ROOT_GD, XTG_DEEP_GD   

!
IMPLICIT NONE
!
NAMELIST/NAM_PREP_TEB_GARDEN/CFILE_GD, CTYPE, CFILEPGD_GD, CTYPEPGD,        &
                                CFILE_HUG_GD, CTYPE_HUG,                           &
                                CFILE_HUG_SURF_GD, CFILE_HUG_ROOT_GD, CFILE_HUG_DEEP_GD, &
                                XHUG_SURF_GD, XHUG_ROOT_GD, XHUG_DEEP_GD,                &
                                XHUGI_SURF_GD, XHUGI_ROOT_GD, XHUGI_DEEP_GD,             &
                                CFILE_TG_GD, CTYPE_TG,                              &
                                CFILE_TG_SURF_GD, CFILE_TG_ROOT_GD, CFILE_TG_DEEP_GD,    &
                                XTG_SURF_GD, XTG_ROOT_GD, XTG_DEEP_GD   
!
END MODULE MODN_PREP_TEB_GARDEN
