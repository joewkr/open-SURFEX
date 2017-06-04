!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_BUFFER (DTCO, U, KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_BUFFER * - Only extrapolation
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     S.Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005
!!------------------------------------------------------------------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_PREP,       ONLY : CMASK
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_PACK_SAME_RANK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKOUT ! output mask
INTEGER                         :: INO      ! output number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.    Initialisation of the output mask
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_BUFFER',0,ZHOOK_HANDLE)
!
INO = SIZE(PFIELDOUT,1)
ALLOCATE(IMASKOUT(INO))
 CALL GET_SURF_MASK_n(DTCO, U, CMASK,INO,IMASKOUT,NNI,KLUOUT)
!
!*      2.    Mask the input field with the output mask
!!mask du tableau de taille FULL en fonction du type de surface
 CALL PACK_SAME_RANK(IMASKOUT,PFIELDIN,PFIELDOUT)
!
!*      6.    Deallocations
!
DEALLOCATE(IMASKOUT)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_BUFFER',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_BUFFER
