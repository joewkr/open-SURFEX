!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_CTI(UG,KLUOUT,KNBLINES,PLAT,PLON,PVALUE,PNODATA)
!     ################################################
!
!!**** *AVERAGE1_CTI* computes the sum of cti, squared cti
!!                    and subgrid cti characteristics
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
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_PGDWORK,       ONLY : XALL, NSIZE_ALL, XEXT_ALL
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
!!
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
INTEGER,                 INTENT(IN)    :: KLUOUT
INTEGER,                 INTENT(IN)    :: KNBLINES
REAL, DIMENSION(:),      INTENT(IN)    :: PLAT    ! latitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PLON    ! longitude of the point to add
REAL, DIMENSION(:),      INTENT(IN)    :: PVALUE  ! value of the point to add
REAL, OPTIONAL, INTENT(IN) :: PNODATA
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain
!
REAL, DIMENSION(SIZE(PLAT)) :: ZVALUE
REAL :: ZNODATA
!
INTEGER :: JL, JOV        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
!*    1.     Get position
!            ------------
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',0,ZHOOK_HANDLE)
!
IF (PRESENT(PNODATA)) THEN
  ZVALUE(:) = PVALUE(:)
  ZNODATA = PNODATA
  CALL GET_MESH_INDEX(UG,KLUOUT,KNBLINES,PLAT,PLON,IINDEX,ZVALUE,ZNODATA)
ELSE
  ZVALUE(:) = 1.
  ZNODATA = 0.
  CALL GET_MESH_INDEX(UG,KLUOUT,KNBLINES,PLAT,PLON,IINDEX)
ENDIF
!
!*    2.     Loop on all input data points
!            -----------------------------
!     
bloop: &
DO JL = 1 , SIZE(PLAT)
!
  DO JOV = 1, NOVMX
!
!*    3.     Tests on position
!            -----------------
!
    IF (IINDEX(JOV,JL)==0) CYCLE bloop
!
!*    4.     Summation
!            ---------
!
    NSIZE_ALL(IINDEX(JOV,JL),1)=NSIZE_ALL(IINDEX(JOV,JL),1)+1
!
!*    5.     CTI
!            ---
!
    XALL(IINDEX(JOV,JL),1,1) = XALL(IINDEX(JOV,JL),1,1)+PVALUE(JL)
!
!*    6.     Square of CTI
!            -------------
!
    XALL(IINDEX(JOV,JL),2,1) = XALL(IINDEX(JOV,JL),2,1)+PVALUE(JL)**2
!
!
!*    7.     Cube of CTI
!            -------------
!
    XALL(IINDEX(JOV,JL),3,1) = XALL(IINDEX(JOV,JL),3,1)+PVALUE(JL)**3
!
!
!*    8.     Maximum CTI in the mesh
!            -----------------------
!
    XEXT_ALL(IINDEX(JOV,JL),1) = MAX(XEXT_ALL(IINDEX(JOV,JL),1),PVALUE(JL))
!
!
!*    9.     Minimum CTI in the mesh
!            -----------------------
!
    XEXT_ALL(IINDEX(JOV,JL),2) = MIN(XEXT_ALL(IINDEX(JOV,JL),2),PVALUE(JL))
!
!
  ENDDO
!  
ENDDO bloop
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_CTI',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_CTI
