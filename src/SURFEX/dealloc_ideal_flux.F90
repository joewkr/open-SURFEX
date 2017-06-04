!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_IDEAL_FLUX
!     #################################################################################
!
!!****  *DEALLOC_IDEAL_FLUX * - Deallocate all arrays
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_IDEAL_FLUX, ONLY : XTIMEF, XTIMET, &
                            XSFTH, XSFTQ, XSFTS, XSFCO2, XUSTAR, XTSRAD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_IDEAL_FLUX',0,ZHOOK_HANDLE)
IF (ALLOCATED(XTIMEF)) DEALLOCATE(XTIMEF)
IF (ALLOCATED(XTIMET)) DEALLOCATE(XTIMET)
IF (ALLOCATED(XSFTS))  DEALLOCATE(XSFTS)
IF (ALLOCATED(XSFTH))  DEALLOCATE(XSFTH)
IF (ALLOCATED(XSFTQ))  DEALLOCATE(XSFTQ)
IF (ALLOCATED(XSFCO2)) DEALLOCATE(XSFCO2)
IF (ALLOCATED(XUSTAR)) DEALLOCATE(XUSTAR)
IF (ALLOCATED(XTSRAD)) DEALLOCATE(XTSRAD)
IF (LHOOK) CALL DR_HOOK('DEALLOC_IDEAL_FLUX',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_IDEAL_FLUX


