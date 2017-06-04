!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE1_MESH(UG,KLUOUT,KNBLINES,PLAT,PLON,PVALUE,OMULTITYPE,KFACT,PNODATA)
!     #######################################################
!
!!**** *AVERAGE1_MESH* computes the sum of orography, squared orography
!!                              and subgrid orography characteristics
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_PGDWORK,       ONLY : XALL, NSIZE_ALL, CATYPE, &
                               NVALNBR, NVALCOUNT, XVALLIST, JPVALMAX
USE MODD_DATA_COVER_PAR,ONLY : XCDREF
!
USE MODI_GET_MESH_INDEX
USE MODD_POINT_OVERLAY
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
LOGICAL, INTENT(IN) :: OMULTITYPE
INTEGER, INTENT(IN) :: KFACT
REAL, OPTIONAL, INTENT(IN) :: PNODATA
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(NOVMX,SIZE(PLAT)) :: IINDEX ! mesh index of all input points
                                         ! 0 indicates the point is out of the domain
INTEGER :: JVAL, JTY, IDX       ! loop counter on encoutered values
INTEGER :: JL, JOV        ! loop index on input arrays
REAL    :: ZEPS=1.E-10  ! a small value
LOGICAL :: GFOUND       ! T : Value already found in this grid point
!
LOGICAL, DIMENSION(SIZE(PLAT)) :: GFLAG
REAL, DIMENSION(SIZE(PLAT)) :: ZVALUE
REAL :: ZNODATA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Get position
!            ------------
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE1_MESH',0,ZHOOK_HANDLE)
!
! to calculate the mesh indexes only where pvalue /= pnodata
IF (PRESENT(PNODATA)) THEN
  ZNODATA = PNODATA
  ZVALUE(:) = PVALUE(:)
ELSE
  ZNODATA = 0.
  ZVALUE(:) = 1.
ENDIF
!
CALL GET_MESH_INDEX(UG,KLUOUT,KNBLINES,PLAT,PLON,IINDEX,ZVALUE,ZNODATA)
!
IF (.NOT.PRESENT(PNODATA)) ZVALUE(:) = PVALUE(:)
!
!*    2.     Loop on all input data points
!            -----------------------------
! 
DO JOV = 1, NOVMX
  !
  bloop: &
  DO JL = 1 , SIZE(PLAT)
    !
!*    3.     Tests on position
!            -----------------
!
    IDX = IINDEX(JOV,JL)
 
    IF (IDX==0) CYCLE bloop
!
!*    4.     Summation
!            ---------
!
    IF (PRESENT(PNODATA)) THEN
      IF (ZVALUE(JL)==ZNODATA) CYCLE
    ENDIF
!
! the type of the point and the true value
    IF (OMULTITYPE) THEN
      JTY = FLOOR(ZVALUE(JL)/100.)
      ZVALUE(JL) = (ZVALUE(JL) - JTY*100.) / FLOAT(KFACT)
    ELSE
      JTY = 1
    ENDIF

    NSIZE_ALL(IDX,JTY) = NSIZE_ALL(IDX,JTY)+1
!
!*    5.     Choice of type of summation
!            ---------------------------
!
    SELECT CASE (CATYPE)

      CASE ('ARI')
        XALL(IDX,JTY,1) = XALL(IDX,JTY,1) +   ZVALUE(JL)

      CASE ('INV')
        XALL(IDX,JTY,1) = XALL(IDX,JTY,1) + 1./ZVALUE(JL)

      CASE ('CDN')
        XALL(IDX,JTY,1) = XALL(IDX,JTY,1) + 1./(LOG(XCDREF/ZVALUE(JL)))**2

      CASE ('MAJ')

        GFOUND=.FALSE.
        DO JVAL=1,NVALNBR(IDX,JTY)
          IF (ABS( XVALLIST(IDX,JVAL,JTY) - ZVALUE(JL)) < ZEPS) THEN
            NVALCOUNT(IDX,JVAL,JTY) = NVALCOUNT(IDX,JVAL,JTY) + 1
            GFOUND=.TRUE.
            EXIT
          END IF
        END DO

        IF (.NOT. GFOUND) THEN
          IF (NVALNBR(IDX,JTY)==JPVALMAX) &
            CALL ABOR1_SFX('TOO MANY DIFFERENT VALUES TO AGGREGATE WITH THE MAJORITY RULE')
          NVALNBR(IDX,JTY) = NVALNBR(IDX,JTY) +1
          JVAL = NVALNBR(IDX,JTY)
          NVALCOUNT(IDX,JVAL,JTY) = 1
          XVALLIST (IDX,JVAL,JTY) = ZVALUE(JL)
        END IF

    END SELECT
    !
  ENDDO bloop
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE1_MESH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE1_MESH
