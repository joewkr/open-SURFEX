!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_ISBA_LANDUSE (DTCO, UG, U, IO, NK, NP, NPE, PMESH_SIZE, HPROGRAM)  
!#############################################################
!
!!****  *INIT_ISBA_LANDUSE* - routine to initialize land use for ISBA field
!!
!!    PURPOSE
!!    -------
!     Extrapolation from existing surounding cells with same patch properties:
!!      (1) IPTS=n  interpol field with n pts
!!      (2) IPTS=0  conserve cells mass  
!!   Case 2 : simple extrapolation based on the inside cell informations.
!!             this is donne before conserving cell or global mass
!!
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,ONLY : XUNDEF                 
!
USE MODD_SURFEX_MPI, ONLY : NPROC, NCOMM
!
USE MODI_GET_LUOUT
USE MODI_INI_VAR_FROM_PATCH
USE MODI_CONSERV_GLOBAL_MASS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
REAL, DIMENSION(:), INTENT(IN) :: PMESH_SIZE
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(ISBA_P_t), POINTER :: PK
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZZDG     ! Actual layer thicknesses
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZZDG_OLD ! Old layer thicknesses
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWG_OLD  ! Old XWG
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWGI_OLD ! Old XWGI
!
INTEGER, DIMENSION(IO%NPATCH,0:NPROC-1) :: ICOUNT_ALL
INTEGER, DIMENSION(IO%NPATCH) :: ICOUNT
INTEGER :: ILUOUT, ISIZE, JP, ICPT
INTEGER :: JLAYER, JNBIOMASS, JNLITTER, JNLITTLEVS, JNSOILCARB
#ifdef SFX_MPI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: ISTATUS
#endif
INTEGER :: INFOMPI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
ICOUNT(:) = 0
DO JP=1,IO%NPATCH
  PK => NP%AL(JP)
  IF (PK%NSIZE_P==0) THEN
    ICOUNT(JP) = 1
  ELSEIF(ALL(PK%XDG(:,IO%NGROUND_LAYER)==PK%XDG_OLD(:,IO%NGROUND_LAYER))) THEN
    ICOUNT(JP) = 1
  ENDIF
ENDDO
!
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_ALLGATHER(ICOUNT,SIZE(ICOUNT)*KIND(ICOUNT)/4,MPI_INTEGER,&
                     ICOUNT_ALL,KIND(ICOUNT_ALL)/4,MPI_INTEGER,NCOMM,INFOMPI)
#endif
ELSE
  ICOUNT_ALL(:,0) = ICOUNT
ENDIF
!
ICPT = 0
DO JP = 1,IO%NPATCH
  ! all the values are the same
  IF (ALL(ICOUNT_ALL(JP,:)/=0)) ICPT = ICPT + 1
ENDDO
!
IF ( ICPT==IO%NPATCH ) THEN
  IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! Conserve mass in the cell
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'WR      ', 0)

IF (IO%LGLACIER) &
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH, HPROGRAM,ILUOUT,'ICE_STO ', 0)
!
DO JLAYER=1,SIZE(NPE%AL(1)%XTG,2)
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH, HPROGRAM,ILUOUT,'TEMP GRO', 0, JLAYER)
END DO
!
!
 CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH, HPROGRAM,ILUOUT,'ALBSNOW ', 0)
!
IF (NPE%AL(1)%TSNOW%SCHEME=='1-L'  .OR. NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'EMISSNOW', 0)    
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'TSSNOW  ', 0)
ENDIF
!
DO JLAYER=1,NPE%AL(1)%TSNOW%NLAYER
  !
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'WSNOW   ', 0, JLAYER)
  !
  IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN            
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'TEMPSNOW',0, JLAYER)
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'HEATSNOW', 0, JLAYER)     
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'AGESNOW ', 0, JLAYER)
  ENDIF
  !
  IF (NPE%AL(1)%TSNOW%SCHEME=='1-L') THEN
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'TSNOW   ', 0, JLAYER)
  ENDIF
  !
  IF(NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'GR1SNOW', 0, JLAYER)
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'GR2SNOW', 0, JLAYER)
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'HISTSNOW', 0, JLAYER)
  ENDIF
  !
ENDDO
!
!-------------------------------------------------------------------------------
! Conserve mass globaly because soil depth change
!-------------------------------------------------------------------------------
!
ALLOCATE(ZZDG    (SIZE(NP%AL(1)%XDG,1),SIZE(NP%AL(1)%XDG,2),IO%NPATCH))
ALLOCATE(ZZDG_OLD(SIZE(NP%AL(1)%XDG,1),SIZE(NP%AL(1)%XDG,2),IO%NPATCH))
ALLOCATE(ZWG_OLD (SIZE(NP%AL(1)%XDG,1),SIZE(NP%AL(1)%XDG,2),IO%NPATCH))
ALLOCATE(ZWGI_OLD(SIZE(NP%AL(1)%XDG,1),SIZE(NP%AL(1)%XDG,2),IO%NPATCH))
!
DO JP = 1,IO%NPATCH
  PEK => NPE%AL(JP)
  PK => NP%AL(JP)

  ISIZE = NP%AL(JP)%NSIZE_P
  ZWG_OLD (1:ISIZE,:,JP) =PEK%XWG   (:,:)
  ZWGI_OLD(1:ISIZE,:,JP) =PEK%XWGI  (:,:)
  ZZDG    (1:ISIZE,1,JP) =PK%XDG    (:,1)
  ZZDG_OLD(1:ISIZE,1,JP) =PK%XDG_OLD(:,1)
  IF(IO%CISBA=='DIF')THEN
    DO JLAYER=2,IO%NGROUND_LAYER
      ZZDG    (1:ISIZE,JLAYER,JP) = PK%XDG    (:,JLAYER)-PK%XDG    (:,JLAYER-1)
      ZZDG_OLD(1:ISIZE,JLAYER,JP) = PK%XDG_OLD(:,JLAYER)-PK%XDG_OLD(:,JLAYER-1)
    ENDDO
  ELSE     
    ZZDG    (:,2,JP) = PK%XDG    (:,2)
    ZZDG_OLD(:,2,JP) = PK%XDG_OLD(:,2)
    IF(IO%CISBA=='3-L' )THEN
      ZZDG    (:,3,JP) = PK%XDG    (:,3)-PK%XDG    (:,2)
      ZZDG_OLD(:,3,JP) = PK%XDG_OLD(:,3)-PK%XDG_OLD(:,2)
    ENDIF 
  ENDIF
ENDDO
!
WHERE(ZZDG(:,:,:)    >1.E+10)ZZDG    (:,:,:)=0.
WHERE(ZZDG_OLD(:,:,:)>1.E+10)ZZDG_OLD(:,:,:)=0.
!
DO JLAYER=1,IO%NGROUND_LAYER
   CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'WG      ', 0, JLAYER)
   CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'WGI     ', 0, JLAYER)
ENDDO
!
!
 CALL CONSERV_GLOBAL_MASS(DTCO, U, NP, NPE, PMESH_SIZE, IO%NPATCH,ILUOUT,ZZDG,ZZDG_OLD,'WG ',ZWG_OLD)
 CALL CONSERV_GLOBAL_MASS(DTCO, U, NP, NPE, PMESH_SIZE,IO%NPATCH,ILUOUT,ZZDG,ZZDG_OLD,'WGI',ZWGI_OLD)
!
DEALLOCATE(ZWG_OLD,ZZDG,ZZDG_OLD,ZWGI_OLD)
!
!-------------------------------------------------------------------------------
! Extrapolation with 3 pts 
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'RESA    ', 3, JLAYER)
!
DO JLAYER=1,NPE%AL(1)%TSNOW%NLAYER
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'RHOSNOW ', 3, JLAYER)
ENDDO
!
IF (IO%CPHOTO/='NON') THEN
  !
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'AN      ', 3)
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'ANDAY   ', 3)   
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'ANFM    ', 3)
  CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'LE      ', 3)
  !
  DO JNBIOMASS=1,IO%NNBIOMASS
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'RESPBIOM', 3,JNBIOMASS)
    CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'BIOMASS ', 3,JNBIOMASS)
 ENDDO
 !
 IF (IO%CRESPSL=='CNT') THEN
   !
   DO JNLITTLEVS=1,IO%NNLITTLEVS
     CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'LIGNINST',3,JNLITTLEVS)
     DO JNLITTER=1,IO%NNLITTER
       CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'LITTER  ',3,JNLITTER,JNLITTLEVS)
     ENDDO
   ENDDO
   !
   DO JNSOILCARB=1,IO%NNSOILCARB
     CALL INI_VAR_FROM_PATCH(DTCO, UG, U, NP, NPE, IO%NPATCH,HPROGRAM,ILUOUT,'SOILCARB',3,JNSOILCARB)
   ENDDO
   !
 ENDIF
 !
ENDIF
!
!-------------------------------------------------------------------------------
!  
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_ISBA_LANDUSE
