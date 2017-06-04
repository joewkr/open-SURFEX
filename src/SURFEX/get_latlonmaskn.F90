!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_LATLONMASK_n (UG, &
                                   OLATLONMASK,HGRID,PGRID_PAR,KGRID_PAR)
!     #######################################################
!
!!**** *GET_LATLONMASK_n* get the grid dimensions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Le Moigne         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2007
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!      
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_LATLONMASK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=10), INTENT(OUT)             ::  HGRID      
REAL, DIMENSION(:), POINTER                ::  PGRID_PAR      
INTEGER, INTENT(OUT)                       ::  KGRID_PAR      
LOGICAL, DIMENSION(:,:), INTENT(OUT)       ::  OLATLONMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!----------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('GET_LATLONMASK_N',0,ZHOOK_HANDLE)
UG%NGRID_FULL_PAR=SIZE(UG%XGRID_FULL_PAR)
 CALL LATLONMASK(UG%G%CGRID,UG%NGRID_FULL_PAR,UG%XGRID_FULL_PAR,OLATLONMASK)
!
HGRID=UG%G%CGRID
!
KGRID_PAR=UG%NGRID_FULL_PAR
!
ALLOCATE(PGRID_PAR(KGRID_PAR))
!
PGRID_PAR(:)=UG%XGRID_FULL_PAR(:)
IF (LHOOK) CALL DR_HOOK('GET_LATLONMASK_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_LATLONMASK_n
