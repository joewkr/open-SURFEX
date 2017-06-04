!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_LATLON(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_LATLON* - Interpolation from a lat/lon regular grid
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
!!     C. Lebeaupin Brossier 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!     B. Decharme  07/2014 use usual HORIBL_SURF for regular lat/lon grid
!!                         (ADAPT_HORIBL_SURF is not up to date and is wrong
!!                          for interpolation from a coarse grid to a finer)
!!  
!!------------------------------------------------------------------
!
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_HORIBL, ONLY : LGLOBLON, LGLOBS, LGLOBN, XILO1H, XILO2H, NINLOH, &
                        XLA, XOLA, XOLO, NP, XLOPH
USE MODD_PREP,             ONLY : XLAT_OUT, XLON_OUT, LINTERP
USE MODD_GRID_LATLONREGUL, ONLY : XILAT1, XILON1, XILAT2, XILON2,    &
                                  NINLAT, NINLON, NILENGTH,XILATARRAY  
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_HORIBL_SURF_GRIDIN
USE MODI_HORIBL_SURF_VALUE
USE MODI_HORIBL_SURF_EXTRAP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)    :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT)   :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:,:), POINTER :: ZFIELDIN0
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELDIN    
!
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ILSMIN  
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IMASKIN  ! input mask
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKOUT ! output mask
INTEGER, DIMENSION(:), POINTER :: IMASK=>NULL()
INTEGER, DIMENSION(SIZE(NP,1),SIZE(NP,2)) :: IP
INTEGER                            :: INO, INL     ! output number of points
INTEGER                            :: JL, JI       ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!*      1.    Allocations
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON',0,ZHOOK_HANDLE)
!
INO = SIZE(XLAT_OUT)
INL = SIZE(PFIELDOUT,2)
!
ALLOCATE(IMASKOUT(INO))
IMASKOUT = 1
!
ALLOCATE(IMASKIN (NILENGTH,INL))
!
IF (NRANK==NPIO) THEN
  IMASKIN(:,:) = 1.
  WHERE(PFIELDIN(:,:)==XUNDEF) IMASKIN(:,:) = 0.
ENDIF
!
ALLOCATE(ZFIELDIN(INO,INL,12))
ALLOCATE(ILSMIN(INO,INL,12))
!
!
CALL HORIBL_SURF_GRIDIN(NINLAT,NINLON,NILENGTH,PFIELDIN(:,:),INO, &
                        .FALSE.,KLUOUT,LGLOBS,LGLOBN,LGLOBLON,NP, &
                        ZFIELDIN0,ZFIELDIN,ILSMIN,IMASKIN,IMASKOUT,IMASK)
!
DO JL=1,SIZE(NP,2)
  DO JI = 1,SIZE(NP,1)
    IP(JI,JL) = IMASK(NP(JI,JL))
  ENDDO
ENDDO
!
!*      3. Interpolation with horibl
!
DO JL=1,INL
!
  CALL HORIBL_SURF_VALUE(NILENGTH,INO,PFIELDOUT(:,JL),LINTERP,ZFIELDIN(:,JL,:),&
                         ILSMIN(:,JL,:),XOLO,XOLA,XLA,XLOPH,IMASKIN(:,JL),IMASKOUT)
!
ENDDO
!
  CALL HORIBL_SURF_EXTRAP(XILAT1,XILO1H,XILAT2,XILO2H,NINLAT,NINLON,NILENGTH,&
                          PFIELDIN,INO,IP,XLON_OUT,XLAT_OUT,&
                          PFIELDOUT,KLUOUT,LINTERP,XILATARRAY)
!
!*      6.    Deallocations
!
IMASK => NULL()
DEALLOCATE(IMASKIN )
DEALLOCATE(IMASKOUT)
DEALLOCATE(ZFIELDIN)
DEALLOCATE(ZFIELDIN0)
DEALLOCATE(ILSMIN)
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_LATLON',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_LATLON
