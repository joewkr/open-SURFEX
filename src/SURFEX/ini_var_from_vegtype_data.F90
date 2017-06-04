!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_VAR_FROM_VEGTYPE_DATA (DTCO, DTV, UG, U, &
                                            HPROGRAM,ILUOUT,HNAME,PFIELD,PDEF)
!     ##############################################################
!!
!!    PURPOSE
!!    -------
!!    interpol field with n pts
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
!!    S. FAROUX        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!    Original    12/2010
!!
!----------------------------------------------------------------------------
!!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_GET_SURF_MASK_n
USE MODI_INTERPOL_FIELD
USE MODI_UNPACK_SAME_RANK
USE MODI_PACK_SAME_RANK
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),             INTENT(IN)    :: HPROGRAM  ! host model
INTEGER,                      INTENT(IN   ) :: ILUOUT
 CHARACTER(LEN=*),             INTENT(IN   ) :: HNAME
REAL, DIMENSION(:,:),         INTENT(INOUT) :: PFIELD
REAL, DIMENSION(:), OPTIONAL, INTENT(IN   ) :: PDEF 
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,    DIMENSION(:), ALLOCATABLE :: ZFIELD_TOT
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK  ! mask for packing from complete field to nature field
INTEGER, DIMENSION(:), ALLOCATABLE :: NSIZE, NSIZE_TOT
INTEGER                            :: INI, IVEGTYPE
INTEGER                            :: JVEGTYPE  ! loop counter on vegtypes
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_VEGTYPE_DATA',0,ZHOOK_HANDLE)
!
INI=SIZE(PFIELD,1)
IVEGTYPE=SIZE(PFIELD,2)
!
ALLOCATE(IMASK(INI))
ALLOCATE(NSIZE(INI))
ALLOCATE(NSIZE_TOT(U%NSIZE_FULL))
ALLOCATE(ZFIELD_TOT(U%NSIZE_FULL))
!
 CALL GET_SURF_MASK_n(DTCO, U, &
                      'NATURE',INI,IMASK,U%NSIZE_FULL,ILUOUT)
!
DO JVEGTYPE=1,IVEGTYPE
  NSIZE(:)=0
  WHERE (PFIELD(:,JVEGTYPE).NE.XUNDEF) NSIZE(:)=1
  WHERE (DTV%XPAR_VEGTYPE(:,JVEGTYPE)==0.) NSIZE(:)=-1
  CALL UNPACK_SAME_RANK(IMASK,NSIZE,NSIZE_TOT,-1)
  CALL UNPACK_SAME_RANK(IMASK,PFIELD(:,JVEGTYPE),ZFIELD_TOT)
  IF(PRESENT(PDEF))THEN
    CALL INTERPOL_FIELD(UG, U, &
                        HPROGRAM,ILUOUT,NSIZE_TOT,ZFIELD_TOT,HNAME,PDEF=PDEF(JVEGTYPE))
  ELSE
    CALL INTERPOL_FIELD(UG, U, &
                        HPROGRAM,ILUOUT,NSIZE_TOT,ZFIELD_TOT,HNAME)
  ENDIF 
  CALL PACK_SAME_RANK(IMASK,ZFIELD_TOT,PFIELD(:,JVEGTYPE))  
ENDDO
!
DEALLOCATE(IMASK)
DEALLOCATE(NSIZE)
DEALLOCATE(NSIZE_TOT)
DEALLOCATE(ZFIELD_TOT)
!
IF (LHOOK) CALL DR_HOOK('INI_VAR_FROM_VEGTYPE_DATA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_VAR_FROM_VEGTYPE_DATA
