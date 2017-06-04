!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH (DTCO, UG, U, USS, &
                                          HPROGRAM,PDEPTH,KSTATUS)
!     ##############################################################
!
!!**** *TREAT_GLOBAL_LAKE_DEPTH* monitor for averaging and interpolations of ISBA physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    S. Faroux        Meteo-France
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
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, XSUMVAL, NSIZE
USE MODD_DATA_LAKE,      ONLY : CLAKELDB, CSTATUSLDB, NGRADDEPTH_LDB, NGRADSTATUS_LDB 
!
USE MODI_GET_LUOUT
USE MODI_TREAT_FIELD
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_GET_SURF_MASK_n
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
REAL, DIMENSION(:),INTENT(OUT):: PDEPTH    ! physiographic field
INTEGER, DIMENSION(:),INTENT(OUT):: KSTATUS   ! physiographic field
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
INTEGER                        :: IDIM   !
INTEGER                        :: JI
!
 CHARACTER(LEN=6)    :: YMASK
INTEGER, DIMENSION(NL) :: ISTATUS
REAL, DIMENSION(NL,1) :: ZDEPTH, ZSTATUS    ! physiographic field on full grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',0,ZHOOK_HANDLE)
ZDEPTH (:,:) = XUNDEF
ZSTATUS(:,:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    4.      Averages the field
!             ------------------
!
ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
ALLOCATE(XALL     (U%NDIM_FULL,NGRADDEPTH_LDB,1))
!
NSIZE_ALL(:,:) = 0.
XALL   (:,:,:) = 0.
!
 CALL TREAT_FIELD(UG, U, USS, &
                  HPROGRAM,'SURF  ','DIRECT','A_LDBD', CLAKELDB,   &
                 'water depth         ',ZDEPTH      ) 
!
DEALLOCATE(XSUMVAL)
DEALLOCATE(NSIZE)
!
ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
ALLOCATE(XALL    (U%NDIM_FULL,NGRADSTATUS_LDB,1))
!
NSIZE_ALL  (:,:) = 0.
XALL     (:,:,:) = 0.
!
 CALL TREAT_FIELD(UG, U, USS, &
                  HPROGRAM,'SURF  ','DIRECT','A_LDBS', CSTATUSLDB,  &
                 'water status        ',ZSTATUS             )
!
ISTATUS = NINT(ZSTATUS(:,1))
!
DEALLOCATE(XSUMVAL)
DEALLOCATE(NSIZE)
!
!-------------------------------------------------------------------------------
!
!*    5.      Consistancy check
!             ------------------
!
DO JI = 1, SIZE(ZDEPTH,1)
  IF (U%XWATER(JI).GT.0.) THEN
    IF (ISTATUS(JI).LE.2) ZDEPTH(JI,1) = 10.
    IF (ISTATUS(JI)==3.AND.ZDEPTH(JI,1)==0.) ZDEPTH(JI,1) = 10.
  ELSE
    ZDEPTH(JI,1) = 0.
  ENDIF
ENDDO
!
!*    6.      Mask for the field
!             ------------------
!
YMASK='WATER '
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     YMASK,IDIM)
IF (IDIM/=SIZE(PDEPTH) .OR. IDIM/=SIZE(KSTATUS)) THEN
   WRITE(ILUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PDEPTH),SIZE(KSTATUS)
   CALL ABOR1_SFX('TREAT_GLOBAL_LAKE_DEPTH: WRONG DIMENSION OF MASK')
ENDIF

ALLOCATE(IMASK(IDIM))
ILU=0
 CALL GET_SURF_MASK_n(DTCO, U, &
                      YMASK,IDIM,IMASK,ILU,ILUOUT)
 CALL PACK_SAME_RANK(IMASK,ZDEPTH(:,1),PDEPTH(:))
 CALL PACK_SAME_RANK(IMASK,ISTATUS(:),KSTATUS(:))
DEALLOCATE(IMASK)
!
IF (LHOOK) CALL DR_HOOK('TREAT_GLOBAL_LAKE_DEPTH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_GLOBAL_LAKE_DEPTH
