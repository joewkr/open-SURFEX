!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_TEB(HPROGRAM,KTEB_PATCH, HBEM,        &
                                  HCOOL_COIL, HHEAT_COIL, OAUTOSIZE,&
                                  KROAD_LAYER, KROOF_LAYER,         &
                                  KWALL_LAYER, KFLOOR_LAYER,        &
                                  OGREENROOF, OHYDRO, OSOLAR_PANEL  )
!     ##############################################################
!
!!**** *READ_NAM_PGD_TEB* reads namelist for TEB
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2005
!!       2008 B. Decharme : uniform value of subgrid drainage coefficient
!!    12/2008 E. Martin   : files of data for subgrid drainage 
!!                          and subgridrunoff
!!    06/2009 B. Decharme : files of data for topographic index
!!
!!      A. Lemonsu      07/2012         Key for greenroofs & greenwalls
!!      A. Lemonsu      07/2012         Key for urban hydrology
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM         ! Type of program
INTEGER,          INTENT(OUT) :: KTEB_PATCH       ! number of patches
 CHARACTER(LEN=3), INTENT(OUT) :: HBEM             ! to use BEM
 CHARACTER(LEN=6), INTENT(OUT) :: HCOOL_COIL       ! type of cooling coil
 CHARACTER(LEN=6), INTENT(OUT) :: HHEAT_COIL       ! type of heating coil
LOGICAL,          INTENT(OUT) :: OAUTOSIZE        ! Flag to activate autosize calculations
INTEGER,          INTENT(OUT) :: KROAD_LAYER      ! number of road layers
INTEGER,          INTENT(OUT) :: KROOF_LAYER      ! number of roof layers
INTEGER,          INTENT(OUT) :: KWALL_LAYER      ! number of wall layers
INTEGER,          INTENT(OUT) :: KFLOOR_LAYER     ! number of floor layers
LOGICAL,          INTENT(OUT) :: OGREENROOF       ! key for greenroof activation
LOGICAL,          INTENT(OUT) :: OHYDRO           ! key for urban hydrology activation
LOGICAL,          INTENT(OUT) :: OSOLAR_PANEL     ! key for solar panel activation
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: NTEB_PATCH       ! number of patches
 CHARACTER(LEN=3)         :: CBEM             ! to use BEM
 CHARACTER(LEN=6)         :: CCOOL_COIL       ! type of cooling coil
 CHARACTER(LEN=6)         :: CHEAT_COIL       ! type of heating coil
LOGICAL                  :: LAUTOSIZE        ! Flag to activate autosize calculations
INTEGER                  :: NROAD_LAYER      ! number of road layers
INTEGER                  :: NROOF_LAYER      ! number of roof layers
INTEGER                  :: NWALL_LAYER      ! number of wall layers
INTEGER                  :: NFLOOR_LAYER     ! number of floor layers
LOGICAL                  :: LGREENROOF       ! key for greenroof activation
LOGICAL                  :: LHYDRO           ! key for urban hydrology activation
LOGICAL                  :: LSOLAR_PANEL     ! key for solar panel activation
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_TEB/ NTEB_PATCH, CBEM, CCOOL_COIL, CHEAT_COIL, LAUTOSIZE, &
                  NROAD_LAYER, NFLOOR_LAYER, NROOF_LAYER, NWALL_LAYER, &
                  LGREENROOF, LHYDRO, LSOLAR_PANEL
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TEB',0,ZHOOK_HANDLE)
NTEB_PATCH         = 1
CBEM               = 'DEF'
LAUTOSIZE          =.FALSE.
CHEAT_COIL         ='IDEAL'
CCOOL_COIL         ='IDEAL'
NROAD_LAYER        = 5
NROOF_LAYER        = 5
NWALL_LAYER        = 5
NFLOOR_LAYER       = 5
LGREENROOF         = .FALSE.
LHYDRO             = .FALSE.
LSOLAR_PANEL       = .FALSE.
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_TEB',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_TEB)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
KTEB_PATCH   = NTEB_PATCH           ! number of patches
HBEM         = CBEM
HCOOL_COIL   = CCOOL_COIL
HHEAT_COIL   = CHEAT_COIL
OAUTOSIZE    = LAUTOSIZE
KROAD_LAYER  = NROAD_LAYER
KROOF_LAYER  = NROOF_LAYER
KWALL_LAYER  = NWALL_LAYER
KFLOOR_LAYER = NFLOOR_LAYER
!
OGREENROOF   = LGREENROOF
OHYDRO       = LHYDRO
OSOLAR_PANEL = LSOLAR_PANEL
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_TEB
