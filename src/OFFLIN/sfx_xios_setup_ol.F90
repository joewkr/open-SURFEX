SUBROUTINE SFX_XIOS_SETUP_OL(YSC, KLUOUT, KYEAR, KMONTH, KDAY, PTIME, PSTEP)
!
!**** *SFX_XIOS_SETUP_OL*  - 
!
!     Purpose.
!     --------
!
!       Call SFX_XIOS_SETUP, providing it with Arpege MPI communicator
!       and passing args about : output logical unit, date, model
!       timestep and, for the whole MPI task : lat/lon of centers and
!       corners, cell index and mask, tile masks
!
!**   Interface.
!     ----------
!       *CALL*  *SFX_XIOS_SETUP_OL*
!
!     Input:
!     -----
!
!     Output:
!     ------
!
!
!     Method:
!     ------
!
!     Externals:
!     ---------
!
!     Reference:
!     ---------
!
!     Author:
!     -------
!      S.Senesi, aug 2015
!
!     Modifications.
!     --------------
!
!     -----------------------------------------------------------
!
USE MODD_SURFEX_n,    ONLY : SURFEX_t
!
USE MODN_IO_OFFLINE, ONLY    : LALLOW_ADD_DIM, LWRITE_COORD
USE MODD_SURFEX_MPI, ONLY    : NRANK, NCOMM, NINDEX
!
USE MODD_IO_SURF_OL, ONLY: NMASK_IGN
!
USE MODD_XIOS,  ONLY : LXIOS, LADD_DIM=>LALLOW_ADD_DIM, NBLOCK_XIOS=>NBLOCK
!
USE MODI_ABOR1_SFX
USE MODI_GET_SURF_GRID_DIM_n
USE MODI_GET_MESH_CORNER
USE MODI_SFX_XIOS_SETUP
USE MODI_GET_IGN_MASKALL
!
USE PARKIND1  ,ONLY : JPRB, JPIM
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!
IMPLICIT NONE
!
TYPE(SURFEX_t),  INTENT(INOUT) :: YSC
INTEGER,            INTENT(IN) :: KLUOUT
INTEGER(KIND=JPIM), INTENT(IN) :: KYEAR      ! Current Year
INTEGER(KIND=JPIM), INTENT(IN) :: KMONTH     ! Current Month
INTEGER(KIND=JPIM), INTENT(IN) :: KDAY       ! Current Day 
REAL              , INTENT(IN) :: PTIME      ! Time in the day
REAL              , INTENT(IN) :: PSTEP      ! Atmospheric time step
!
! Local variables
!
INTEGER                              :: IX, JX
INTEGER, ALLOCATABLE, DIMENSION(:)   :: IXINDEX ! Index of the grid meshes for the 
                                       ! current MPI-task  in global 1D grid (start at 0)
LOGICAL, ALLOCATABLE, DIMENSION(:)   :: GLXMASK ! Cells mask
REAL,    ALLOCATABLE, DIMENSION(:,:) :: ZCORLON
REAL,    ALLOCATABLE, DIMENSION(:,:) :: ZCORLAT
INTEGER, ALLOCATABLE, DIMENSION(:)   :: IMN,IMS,IMW,IMT ! Tile masks, 0-based
REAL, DIMENSION(:), ALLOCATABLE :: ZX, ZY
!
CHARACTER(LEN=10)   :: YGRID      ! grid type 
LOGICAL             :: GRECT
INTEGER             :: IDIM1, IDIM2
!
REAL(KIND=JPRB)     :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP_OL',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
IF (LXIOS) THEN 
   !
   LADD_DIM = LALLOW_ADD_DIM
   !
   NBLOCK_XIOS = 1

   !   Build XIOS index from Surfex's NINDEX
   ALLOCATE(IXINDEX(YSC%U%NSIZE_FULL))

   JX=0
   DO IX = 1,YSC%U%NDIM_FULL
      IF ( NINDEX(IX) == NRANK ) THEN 
         JX = JX+1
         IF (JX > YSC%U%NSIZE_FULL ) THEN
            CALL ABOR1_SFX('sfx_xios_setup_ol : internal error with XIOS index')
         ENDIF
         IXINDEX(JX) = IX - 1
      ENDIF
   ENDDO
   !
   ALLOCATE(GLXMASK(YSC%U%NSIZE_FULL))
   GLXMASK(:)=.TRUE.
   !
   ALLOCATE(IMN(SIZE(YSC%U%NR_NATURE)))
   ALLOCATE(IMS(SIZE(YSC%U%NR_SEA)))
   ALLOCATE(IMW(SIZE(YSC%U%NR_WATER )))
   ALLOCATE(IMT(SIZE(YSC%U%NR_TOWN  )))
   IMN(:) = YSC%U%NR_NATURE-1
   IMS(:) = YSC%U%NR_SEA   -1
   IMW(:) = YSC%U%NR_WATER -1
   IMT(:) = YSC%U%NR_TOWN  -1
   !   
   ALLOCATE(ZCORLAT(YSC%U%NSIZE_FULL,4))
   ALLOCATE(ZCORLON(YSC%U%NSIZE_FULL,4))
   CALL GET_MESH_CORNER(YSC%UG, KLUOUT,ZCORLAT(:,:),ZCORLON(:,:))
   !
   IF (LWRITE_COORD.AND.(TRIM(YSC%UG%G%CGRID)=='LONLATVAL'.OR.TRIM(YSC%UG%G%CGRID)=='IGN')) THEN
     IDIM1 = YSC%U%NDIM_FULL
     IDIM2 = 1     
   ELSE
     CALL GET_SURF_GRID_DIM_n(YSC%UG, YGRID, GRECT, IDIM1, IDIM2)
     IF (TRIM(YSC%UG%G%CGRID)=='IGN') THEN
       ALLOCATE(ZX(IDIM1),ZY(IDIM2))
       CALL GET_IGN_MASKALL(YSC%UG,YSC%U%NDIM_FULL,ZX,ZY)
       IXINDEX(:) = NMASK_IGN(IXINDEX(:)+1) -1
       DEALLOCATE(NMASK_IGN,ZX,ZY)
     ENDIF
   ENDIF
   !
   CALL SFX_XIOS_SETUP(YSC,NCOMM,KLUOUT,&
                       KYEAR,KMONTH,KDAY,PTIME,PSTEP,&
                       IDIM1,IDIM2,0,TRANSPOSE(ZCORLAT),TRANSPOSE(ZCORLON),&
                       IXINDEX,GLXMASK,IMN,IMS,IMW,IMT)
   !
   DEALLOCATE(IMN,IMS,IMW,IMT)
   DEALLOCATE(ZCORLAT, ZCORLON)
   DEALLOCATE(IXINDEX)
   DEALLOCATE(GLXMASK)
   !
ENDIF
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP_OL',1,ZHOOK_HANDLE)
END SUBROUTINE SFX_XIOS_SETUP_OL
