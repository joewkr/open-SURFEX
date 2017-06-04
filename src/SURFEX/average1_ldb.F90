!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_LDB(UG,KLUOUT,KNBLINES,PLAT,PLON,PVALUE,HTYPE,PNODATA)
!     #######################################################
!
!!**** *AVERAGE1_LDB* 
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
!!    S. Faroux         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    17/02/11
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_PGDWORK, ONLY : XALL, NSIZE_ALL
USE MODD_DATA_LAKE, ONLY : XBOUNDGRADDEPTH_LDB, XBOUNDGRADSTATUS_LDB
!
USE MODD_POINT_OVERLAY
!
USE MODI_GET_MESH_INDEX
USE MODI_ABOR1_SFX
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
 CHARACTER(LEN=1),       INTENT(IN)    :: HTYPE
REAL, OPTIONAL, INTENT(IN) :: PNODATA
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZBOUND
!
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain                              
!
REAL, DIMENSION(SIZE(PLAT)) :: ZVALUE
REAL :: ZNODATA
!
REAL    :: ZCUT
INTEGER :: JL, JGR, JOV        ! loop index on input arrays
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
!     
IF (LHOOK) CALL DR_HOOK('AVERAGE1_LDB',0,ZHOOK_HANDLE)
!
SELECT CASE (HTYPE)
!
  CASE('D')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADDEPTH_LDB)))
    ZBOUND(:) = XBOUNDGRADDEPTH_LDB(:)
!
  CASE('S')
    ALLOCATE(ZBOUND(SIZE(XBOUNDGRADSTATUS_LDB)))
    ZBOUND(:) = XBOUNDGRADSTATUS_LDB(:)
!
  CASE DEFAULT
    CALL ABOR1_SFX("AVERAGE1_LDB: HTYPE NOT SUPPORTED")
!
END SELECT
!
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
!*    4.     Test on value meaning
!            ---------------------
!
    ZCUT = PVALUE(JL)
!
    DO JGR = 1, SIZE(ZBOUND)-1
      IF (ZCUT.GT.ZBOUND(JGR) .AND. ZCUT.LE.ZBOUND(JGR+1)) THEN
        XALL(IINDEX(1,JL),JGR,1) = XALL(IINDEX(1,JL),JGR,1) + 1
        EXIT
      ENDIF
    ENDDO
!
!*    5.     Summation
!            ---------
!
    NSIZE_ALL(IINDEX(1,JL),1)=NSIZE_ALL(IINDEX(1,JL),1)+1
!
  END DO
!
ENDDO bloop
!
DEALLOCATE(ZBOUND)
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_LDB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_LDB
