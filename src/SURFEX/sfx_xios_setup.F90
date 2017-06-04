!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_SETUP(YSC, KCOMM, KLUOUT, KYEAR, KMONTH, KDAY, PTIME, PTSTEP, &
     KDIM1, KDIM2, KEXT1, PCLAT, PCLON, KXINDEX, ODXMASK,&
     KMASKNAT, KMASKSEA, KMASKWAT, KMASKTOWN)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Initialize all Surfex context for XIOS (calendar, grids, masks, model time step, ...)
!!
!!
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     LXIOS, YXIOS_CONTEXT, TXIOS_CONTEXT
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> ; cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2015
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : SURFEX_t

USE MODD_CSTS,  ONLY : XPI
USE MODD_SURFEX_MPI, ONLY     : NRANK, NINDEX
!
USE MODD_XIOS      , ONLY : LXIOS,TXIOS_CONTEXT,YXIOS_CONTEXT, &
     LXIOS_DEF_CLOSED, YGROUND_LAYER_DIM_NAME, &
     YWGROUND_LAYER_DIM_NAME, YWIGROUND_LAYER_DIM_NAME, NBASE_XIOS_FREQ, &
     YSWBAND_DIM_NAME, YPATCH_DIM_NAME
!
#ifdef WXIOS 
USE XIOS, ONLY : XIOS_CONTEXT_INITIALIZE, XIOS_GET_HANDLE,   &
     XIOS_SET_CURRENT_CONTEXT, XIOS_SET_TIMESTEP, XIOS_DATE, &
     XIOS_DURATION, XIOS_DEFINE_CALENDAR, XIOS_GETVAR,       &
     XIOS_SOLVE_INHERITANCE
!
USE MODI_ABOR1_SFX
USE MODI_SFX_XIOS_SET_DOMAIN
USE MODI_SET_AXIS
!
#endif
!
USE YOMHOOK           , ONLY : LHOOK,   DR_HOOK
USE PARKIND1          , ONLY : JPRB
!
IMPLICIT NONE
!
!
!   Arguments
!
TYPE (SURFEX_t),    INTENT(IN) :: YSC
!
INTEGER,            INTENT(INOUT) :: KCOMM  ! Communicator
INTEGER,            INTENT(IN) :: KLUOUT    ! Listing logical unit number
INTEGER,            INTENT(IN) :: KYEAR     ! current year (UTC)
INTEGER,            INTENT(IN) :: KMONTH    ! current month (UTC)
INTEGER,            INTENT(IN) :: KDAY      ! current day (UTC)
REAL,               INTENT(IN) :: PTIME     ! current time since midnight (UTC,s)
REAL,               INTENT(IN) :: PTSTEP    ! model time step 
INTEGER,            INTENT(IN) :: KDIM1     ! Geometry param. (see  sfx_set_domain)
INTEGER,            INTENT(IN) :: KDIM2     ! Geometry param. (see  sfx_set_domain)
INTEGER,            INTENT(IN) :: KEXT1     ! Geometry param. (see  sfx_set_domain)
REAL   ,   INTENT(IN) , DIMENSION(:,:) :: PCLAT      ! Lat corners "
REAL   ,   INTENT(IN) , DIMENSION(:,:) :: PCLON      ! Lon corners "
INTEGER,   INTENT(IN) , DIMENSION(:)   :: KXINDEX    ! index of proc cells in global grid
LOGICAL,   INTENT(IN) , DIMENSION(:)   :: ODXMASK    ! Cells mask
INTEGER,   INTENT(IN) , DIMENSION(:)   :: KMASKNAT   ! Masks for the whole MPI task
INTEGER,   INTENT(IN) , DIMENSION(:)   :: KMASKSEA
INTEGER,   INTENT(IN) , DIMENSION(:)   :: KMASKWAT
INTEGER,   INTENT(IN) , DIMENSION(:)   :: KMASKTOWN

!
!  Local variables
!
#ifdef WXIOS
TYPE(XIOS_DURATION)   :: TDTIME ! Time-step 'a la XIOS'
INTEGER               :: INHOURS,INMINUTES,INSECONDS
#endif
!
INTEGER            :: ITMP
!
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP',0,ZHOOK_HANDLE)
!
IF (.NOT. LXIOS ) THEN 
   CALL DR_HOOK('SFX_XIOS_SETUP',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
#ifndef WXIOS
!
IF (LXIOS) THEN 
   CALL ABOR1_SFX('SFX_XIOS_INIT_CONTEXT : cannot setup : Surfex was compiled without XIOS support')
ELSE
   LXIOS_DEF_CLOSED=.TRUE.
ENDIF
!
#else 
!
LXIOS_DEF_CLOSED=.FALSE.
!
! -----------------------------------------------------------------------------
!
!       Set Xios context to Surfex's one
!
! -----------------------------------------------------------------------------
!
!$OMP SINGLE
!
!#ifndef CPLOASIS
!  CALL XIOS_INITIALIZE('surfex', return_comm=KCOMM)
!#endif

 CALL XIOS_CONTEXT_INITIALIZE(YXIOS_CONTEXT, KCOMM)
 CALL XIOS_GET_HANDLE(YXIOS_CONTEXT, TXIOS_CONTEXT)
 CALL XIOS_SET_CURRENT_CONTEXT(TXIOS_CONTEXT)
! 

! -----------------------------------------------------------------------------
!
!      Set date for XIOS
!
! -----------------------------------------------------------------------------
!
INHOURS   = INT(PTIME/3600)
INMINUTES = INT((PTIME - INHOURS*3600)/60)
INSECONDS = INT(PTIME - INHOURS*3600 - INMINUTES*60)
!
!WRITE(KLUOUT,*) 'initializing xios calendar '

! For XIOS-2.0 :
 CALL XIOS_DEFINE_CALENDAR("Gregorian", &
     start_date  = xios_date(KYEAR,KMONTH,KDAY,INHOURS,INMINUTES,INSECONDS), &
     time_origin = xios_date(KYEAR,KMONTH,KDAY,INHOURS,INMINUTES,INSECONDS))
!
! -----------------------------------------------------------------------------
!
!   Set duration between 2 calls to write_diag_surf
!
! -----------------------------------------------------------------------------
!
IF (XIOS_GETVAR("timesteps_between_samples",ITMP)) NBASE_XIOS_FREQ=ITMP
!
TDTIME%SECOND = INT(PTSTEP*NBASE_XIOS_FREQ)
 CALL XIOS_SET_TIMESTEP(TDTIME)
!
!$OMP END SINGLE
!
!
! ---------------------------------------------------------------------------------
!
!   Declare a 'full' domain and one domain per tile 
!
! ---------------------------------------------------------------------------------
!
 CALL SFX_XIOS_SET_DOMAIN(YSC%UG%G%CGRID, "FULL"  , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
     YSC%UG%G%XLON, YSC%UG%G%XLAT, PCLON, PCLAT )
 CALL SFX_XIOS_SET_DOMAIN(YSC%UG%G%CGRID, "SEA"   , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
     YSC%UG%G%XLON, YSC%UG%G%XLAT, PCLON, PCLAT, KMASK=KMASKSEA )
 CALL SFX_XIOS_SET_DOMAIN(YSC%UG%G%CGRID, "NATURE", KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
     YSC%UG%G%XLON, YSC%UG%G%XLAT, PCLON, PCLAT, KMASK=KMASKNAT)
 CALL SFX_XIOS_SET_DOMAIN(YSC%UG%G%CGRID, "WATER" , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
     YSC%UG%G%XLON, YSC%UG%G%XLAT , PCLON, PCLAT, KMASK=KMASKWAT )
 CALL SFX_XIOS_SET_DOMAIN(YSC%UG%G%CGRID, "TOWN"  , KDIM1, KDIM2, KEXT1, KXINDEX, ODXMASK, &
     YSC%UG%G%XLON, YSC%UG%G%XLAT , PCLON, PCLAT, KMASK=KMASKTOWN )
!
! Declare axes, depending on activated schemes
!
!IF (YSC%U%NDIM_NATURE>0) THEN
   CALL SET_AXIS(YPATCH_DIM_NAME , KSIZE=SIZE(YSC%IM%NPE%AL))
   CALL SET_AXIS(YGROUND_LAYER_DIM_NAME , KSIZE=SIZE(YSC%IM%NPE%AL(1)%XTG,2))
   CALL SET_AXIS(YWGROUND_LAYER_DIM_NAME, KSIZE=SIZE(YSC%IM%NPE%AL(1)%XWG,2))
   CALL SET_AXIS(YWIGROUND_LAYER_DIM_NAME, KSIZE=SIZE(YSC%IM%NPE%AL(1)%XWG,2))
   CALL SET_AXIS(YSWBAND_DIM_NAME, KSIZE=SIZE(YSC%IM%ID%D%XSWBD,2))
   ! CALL SET_AXIS(YSNOW_PATCH_DIM_NAME, KSIZE=)
!ENDIF
!IF (YSC%U%NDIM_SEA>0) THEN
   ! CALL SET_AXIS(YSEAICE_LAYER_DIM_NAME, KSIZE=)
!ENDIF
!
!  Force XIOS inheritance inorder that fields declarations can 
!  fully account for user settings
!
!   CALL XIOS_SOLVE_INHERITANCE()
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SETUP',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFX_XIOS_SETUP
