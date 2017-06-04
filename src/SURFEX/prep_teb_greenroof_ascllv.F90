!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GREENROOF_ASCLLV (DTCO, UG, U, USS, &
                                      HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GREENROOF_ASCLLV* - prepares ISBA field from prescribed values
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    Based on "prep_teb_garden_ASCLLV"
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!    A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!------------------------------------------------------------------
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NPROC, NINDEX, NNUM, NCOMM, NPIO, NRANK
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_PREP,              ONLY : CINTERP_TYPE
USE MODD_PGD_GRID,          ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_PGDWORK,           ONLY : CATYPE
USE MODD_DATA_COVER_PAR,    ONLY : NVEGTYPE
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_PREP_TEB_GREENROOF,ONLY : CTYPE_HUG     , CTYPE_TG     , &
                                   CFILE_HUG_SURF_GR, CFILE_TG_SURF_GR, &
                                   CFILE_HUG_ROOT_GR, CFILE_TG_ROOT_GR, &
                                   CFILE_HUG_DEEP_GR, CFILE_TG_DEEP_GR  
USE MODI_PGD_FIELD
USE MODI_GET_LATLONMASK_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: JV ! loop counter
INTEGER :: JLAYER
INTEGER :: IL
!
INTEGER, DIMENSION(0:NPROC-1) :: INB
INTEGER :: INFOMPI, JJ
!
REAL, ALLOCATABLE, DIMENSION(:,:)     :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_ASCLLV',0,ZHOOK_HANDLE)
!
IF (.NOT.ALLOCATED(NNUM)) THEN
  ALLOCATE(NNUM(U%NDIM_FULL))
  IF (NRANK/=NPIO) THEN
    IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
    ALLOCATE(NINDEX(U%NDIM_FULL))
  ENDIF  
  IF (NRANK==NPIO) THEN
    INB(:) = 0
    DO JJ=1,U%NDIM_FULL
      INB(NINDEX(JJ)) = INB(NINDEX(JJ))+1
      NNUM(JJ) = INB(NINDEX(JJ))
    ENDDO
  ENDIF
  IF (NPROC>1) THEN
#ifdef SFX_MPI          
    CALL MPI_BCAST(NINDEX,SIZE(NINDEX)*KIND(NINDEX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NNUM,SIZE(NNUM)*KIND(NNUM)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(UG%NGRID_FULL_PAR,KIND(UG%NGRID_FULL_PAR)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
    IF (NRANK/=NPIO) ALLOCATE(UG%XGRID_FULL_PAR(UG%NGRID_FULL_PAR))
#ifdef SFX_MPI    
    CALL MPI_BCAST(UG%XGRID_FULL_PAR,&
      SIZE(UG%XGRID_FULL_PAR)*KIND(UG%XGRID_FULL_PAR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)    
#endif
  ENDIF
ENDIF
!
CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'FULL  ',NL)
!
!*      2.    get nature dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',IL)
!
ALLOCATE(ZFIELD(IL,3))
!
!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(UG, &
                       LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!
SELECT CASE(HSURF)
!
!
!*      4.    Profile of soil relative humidity
!
  CASE('WG     ')

    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_SURF: relative humidity','TWN',CFILE_HUG_SURF_GR,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,1))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_ROOT: relative humidity','TWN',CFILE_HUG_ROOT_GR,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,2))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'HUG_DEEP: relative humidity','TWN',CFILE_HUG_DEEP_GR,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,3))  

    ALLOCATE(PFIELD(IL,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = ZFIELD(:,1)
      PFIELD(:,2,JV) = ZFIELD(:,2)
      PFIELD(:,3,JV) = ZFIELD(:,3)
    END DO

!*      5.    Profile of temperatures

  CASE('TG     ')

    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_SURF: temperature','TWN',CFILE_TG_SURF_GR,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,1))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_ROOT: temperature','TWN',CFILE_TG_ROOT_GR,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,2))  
    CALL PGD_FIELD(DTCO, UG, U, USS, &
                   HPROGRAM,'TG_DEEP: temperature','TWN',CFILE_TG_DEEP_GR,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,3))  

    ALLOCATE(PFIELD(IL,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = ZFIELD(:,1)
      PFIELD(:,2,JV) = ZFIELD(:,2)
      PFIELD(:,3,JV) = ZFIELD(:,3)
    END DO

END SELECT
!
!*      6.     Interpolation method
!              --------------------
!
CINTERP_TYPE='NONE  '
DEALLOCATE(ZFIELD)
!
DEALLOCATE(NNUM)
IF (NRANK/=NPIO) THEN
  DEALLOCATE(NINDEX,UG%XGRID_FULL_PAR)
  ALLOCATE(NINDEX(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GREENROOF_ASCLLV
