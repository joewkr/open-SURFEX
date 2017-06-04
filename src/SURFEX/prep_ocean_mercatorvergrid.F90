!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE PREP_OCEAN_MERCATORVERGRID(HPROGRAM,OUNIF)
!   ######################################################################
!
!!****  *PREP_OCEAN_MERCATORVERGRID*  
!!
!!    PURPOSE
!!    -------
!
!     Define the vertical ocean grid
!         
!     
!!**  METHOD
!!    ------
!
!
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_OCEAN_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      C. Lebeaupin Brossier  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!       D.BARBARY 11/2014 : HPROGRAM,OUNIF in Calling OCEAN_MERCATORVERGRID
!                           Reading oceanic level and depth
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_OCEAN_GRID
USE MODI_READ_Z1D_NETCDF
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
LOGICAL, INTENT(IN) :: OUNIF
!
!*      0.2    declarations of local variables
!
INTEGER            :: IK1,IKK1
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!       1.     Allocations
!              -----------
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_MERCATORVERGRID',0,ZHOOK_HANDLE)

!       1.     Principal grid
!              --------------
!CLB
NOCKMIN=0
IK1=NOCKMIN+1

IF (.NOT.OUNIF) THEN
  !
  CALL READ_Z1D_NETCDF ! doit initialiser(NOCKMAX,XZHOC(:))
  !
ELSE
  !
  ! Initialize vertical grid for idealized case
  NOCKMAX=40
  IKK1=NOCKMAX-1
  ALLOCATE(XZHOC      (NOCKMIN:NOCKMAX))
  XZHOC(0)=0.
  XZHOC(IK1)    = -1.
  XZHOC(2)      = -5.
  XZHOC(3)      = -10.
  XZHOC(4)      = -15.
  XZHOC(5)      = -20.
  XZHOC(6)      = -25.
  XZHOC(7)      = -30.
  XZHOC(8)      = -40.
  XZHOC(9)      = -50.
  XZHOC(10)     = -60.
  XZHOC(11)     = -75.
  XZHOC(12)     = -100.
  XZHOC(13)     = -125.
  XZHOC(14)     = -150.
  XZHOC(15)     = -175.
  XZHOC(16)     = -200.
  XZHOC(17)     = -225.
  XZHOC(18)     = -250.
  XZHOC(19)     = -300.
  XZHOC(20)     = -400.
  XZHOC(21)     = -500.
  XZHOC(22)     = -600.
  XZHOC(23)     = -700.
  XZHOC(24)     = -800.
  XZHOC(25)     = -900.
  XZHOC(26)     = -1000.
  XZHOC(27)     = -1100.
  XZHOC(28)     = -1200.
  XZHOC(29)     = -1300.
  XZHOC(30)     = -1400.
  XZHOC(31)     = -1500.
  XZHOC(32)     = -1750.
  XZHOC(33)     = -2000.
  XZHOC(34)     = -2250.
  XZHOC(35)     = -2500.
  XZHOC(36)     = -2750.
  XZHOC(37)     = -3000.
  XZHOC(38)     = -3250.
  XZHOC(IKK1)   = -3500.
  XZHOC(NOCKMAX)  = -4000.
!  WRITE(0,*) 'Number of vertical levels ',NOCKMAX
!  WRITE(0,*) 'Depth of vertical level (m)',XZHOC(:)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_OCEAN_MERCATORVERGRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_OCEAN_MERCATORVERGRID
