!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEALLOC_GR_SNOW(TPSNOW)
!     ##############################################
!
!!****  *DEALLOC_GR_SNOW* - 
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!       TPSNOW%SCHEME must yet be initialized
!!    
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      S.Faroux  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    14/01/15
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_SNOW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
TYPE(SURF_SNOW), INTENT(INOUT)             :: TPSNOW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_GR_SNOW',0,ZHOOK_HANDLE)
!
DEALLOCATE(TPSNOW%WSNOW)
DEALLOCATE(TPSNOW%RHO)
DEALLOCATE(TPSNOW%ALB)  
DEALLOCATE(TPSNOW%EMIS)
DEALLOCATE(TPSNOW%TS  )
DEALLOCATE(TPSNOW%TEMP)
DEALLOCATE(TPSNOW%HEAT)
DEALLOCATE(TPSNOW%GRAN1)
DEALLOCATE(TPSNOW%GRAN2)  
DEALLOCATE(TPSNOW%HIST )  
DEALLOCATE(TPSNOW%AGE  ) 
DEALLOCATE(TPSNOW%T)
!
IF (LHOOK) CALL DR_HOOK('DEALLOC_GR_SNOW',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE DEALLOC_GR_SNOW
