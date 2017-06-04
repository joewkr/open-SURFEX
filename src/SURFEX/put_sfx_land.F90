!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PUT_SFX_LAND (IO, S, K, NK, NP, U, &
                               KLUOUT,OCPL_WTD,OCPL_FLOOD, &
                              PWTD,PFWTD,PFFLOOD,PPIFLOOD )  
!     #####################################################
!
!!****  *PUT_SFX_LAND* - routine to put some land surface variables to surfex
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2009
!!      B. Decharme    01/16 : Bug with flood budget
!!    10/2016 B. Decharme : bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_K_t, ISBA_NK_t, ISBA_NP_t, ISBA_P_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODN_SFX_OASIS,  ONLY : XFLOOD_LIM
!
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_K_t), INTENT(INOUT) :: K
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER,           INTENT(IN)  :: KLUOUT
LOGICAL,           INTENT(IN)  :: OCPL_WTD
LOGICAL,           INTENT(IN)  :: OCPL_FLOOD
!
REAL, DIMENSION(:), INTENT(IN) :: PWTD     ! water table depth (negative below soil surface) (m)
REAL, DIMENSION(:), INTENT(IN) :: PFWTD    ! fraction of water table rise (-)
REAL, DIMENSION(:), INTENT(IN) :: PFFLOOD  ! fraction of flooded area (-)
REAL, DIMENSION(:), INTENT(IN) :: PPIFLOOD ! Potential floodplain infiltration (kg/m2)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t), POINTER :: PK
TYPE(ISBA_K_t), POINTER :: KK
!
INTEGER :: JP
CHARACTER(LEN=50)     :: YCOMMENT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PUT_SFX_LAND',0,ZHOOK_HANDLE)
!
!*       1.0   Initialization
!              --------------
!
IF(U%NSIZE_NATURE==0)THEN
  IF (LHOOK) CALL DR_HOOK('PUT_SFX_LAND',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!*       2.0   Put variable over nature
!              ------------------------
!
IF(OCPL_WTD)THEN
!    
  K%XWTD    (:) = XUNDEF
  K%XFWTD   (:) = XUNDEF
!
  YCOMMENT='water table depth'
  CALL PACK_SAME_RANK(U%NR_NATURE(:),PWTD(:),K%XWTD(:))
  CALL CHECK_LAND(YCOMMENT,K%XWTD)
!  
  YCOMMENT='fraction of water table rise'
  CALL PACK_SAME_RANK(U%NR_NATURE(:),PFWTD(:),K%XFWTD(:))
  CALL CHECK_LAND(YCOMMENT,K%XFWTD)
!
  WHERE(K%XFWTD(:)==0.0)
    K%XWTD    (:) = XUNDEF
  ENDWHERE
!
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    KK => NK%AL(JP)
    CALL PACK_SAME_RANK(PK%NR_P,K%XWTD,KK%XWTD)
    CALL PACK_SAME_RANK(PK%NR_P,K%XFWTD,KK%XFWTD)
  ENDDO
!   
ENDIF
!
IF(OCPL_FLOOD)THEN
!
  K%XFFLOOD (:) = XUNDEF
  K%XPIFLOOD(:) = XUNDEF
!
  YCOMMENT='Flood fraction'
  CALL PACK_SAME_RANK(U%NR_NATURE(:),PFFLOOD(:),K%XFFLOOD(:))
  CALL CHECK_LAND(YCOMMENT,K%XFFLOOD)
!  
  YCOMMENT='Potential flood infiltration'
  CALL PACK_SAME_RANK(U%NR_NATURE(:),PPIFLOOD(:),K%XPIFLOOD(:))
  CALL CHECK_LAND(YCOMMENT,K%XPIFLOOD)
!
! No flood for very smal flooded area (default 1%)
!
  WHERE(K%XFFLOOD (:)<XFLOOD_LIM)
    K%XFFLOOD (:)=0.0
    K%XPIFLOOD(:)=0.0
  ENDWHERE
!
  DO JP = 1,IO%NPATCH
    PK => NP%AL(JP)
    KK => NK%AL(JP)
    CALL PACK_SAME_RANK(PK%NR_P,K%XFFLOOD,KK%XFFLOOD)
    CALL PACK_SAME_RANK(PK%NR_P,K%XPIFLOOD,KK%XPIFLOOD)
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PUT_SFX_LAND',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_LAND(HCOMMENT,PFIELD)
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
CHARACTER(LEN=*),   INTENT(IN) :: HCOMMENT
REAL, DIMENSION(:), INTENT(IN) :: PFIELD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PUT_SFX_LAND:CHECK_LAND',0,ZHOOK_HANDLE)
!
IF(ANY(PFIELD(:)>=XUNDEF))THEN
  WRITE(KLUOUT,*)'PUT_SFX_LAND: problem after get '//TRIM(HCOMMENT)//' from OASIS'
  WRITE(KLUOUT,*)'PUT_SFX_LAND: some points not defined = ',COUNT(PFIELD(:)>=XUNDEF)
  CALL ABOR1_SFX('PUT_SFX_LAND: problem after get '//TRIM(HCOMMENT)//' from OASIS')          
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PUT_SFX_LAND:CHECK_LAND',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_LAND
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PUT_SFX_LAND
