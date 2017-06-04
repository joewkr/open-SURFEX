!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_FROM_DATA_SEAFLUX_n (DTS, &
                                           PSST)
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary 
!!                     physiographic variables for SEAFLUX
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Le Moigne        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original   09/2007
!     
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
!
USE MODD_TYPE_DATE_SURF

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
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PSST
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ITIME
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      TIME INITIALIZATION
!             -------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_SEAFLUX_N',0,ZHOOK_HANDLE)
ITIME = DTS%NTIME
ITIME = 1
!
!*    2.       VARIABLES
!              ---------
!
! sea surface temperature
! -----------------------
!
IF (PRESENT(PSST)) PSST(:) =  DTS%XDATA_SST (:,ITIME)
IF (PRESENT(PSST)) PSST(:) =  DTS%XDATA_SST (:,1)
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_FROM_DATA_SEAFLUX_n
