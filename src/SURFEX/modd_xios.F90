!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_XIOS
!     ######################
!
!!****  *MODD_XIOS - nest for variables used in interfacing XIOS to Surfex / Arpege
!!
!!    PURPOSE
!!    -------
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S.Sénési   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       08/2015
!
!*       0.   DECLARATIONS
!             ------------
#ifdef WXIOS
USE XIOS ,    ONLY : XIOS_CONTEXT
TYPE(xios_context) :: TXIOS_CONTEXT                ! Xios context handle 
#else
INTEGER            :: TXIOS_CONTEXT                ! Fake Xios context handle 
#endif
!
!
!  Basic toggle
!
LOGICAL            :: LXIOS=.FALSE.                ! Do we use XIOS for outputing diags
!
!
!  Setup variables 
!
CHARACTER(LEN=6)   :: YXIOS_CONTEXT= "surfex"      ! Context name known to Xios (must match a context declared in xml file)
CHARACTER(LEN=14)  :: COUTPUT_DEFAULT="surfex_cselect" ! XIOS id for the file receiving all Surfex variables selected 
                                                   ! by CSELECT (could/should exist in xml configuration file)
LOGICAL            :: LALLOW_ADD_DIM=.FALSE.       ! allow multi-dimensional output ?
INTEGER            :: NBASE_XIOS_FREQ=1            ! Base frequency for calling XIOS (unit=timestep)
!
!  Evolving variables
!
LOGICAL            :: LXIOS_DEF_CLOSED             ! Has the Surfex context definition already been closed ?
CHARACTER(LEN=6)   :: YXIOS_DOMAIN                 ! When writing diags using write_diag_surf, name of the current tile 
INTEGER            :: NTIMESTEP=-1                 ! Last value of timestep sent to XIOS (should be useless, now..)
INTEGER            :: NBLOCK=1                     ! Number of blocks in the MPI-task (NPROMA blocks when in 
                                                   ! Arpege). For xios_send_block
!
!  Names for various dimensions (this section could be move elsewhere when more 
!  output schemes will handle dimension names)
!
CHARACTER(LEN=30) :: YPATCH_DIM_NAME         ="patch"
CHARACTER(LEN=30) :: YGROUND_LAYER_DIM_NAME  ="ground_layer"
CHARACTER(LEN=30) :: YWGROUND_LAYER_DIM_NAME ="ground_water_layer"
CHARACTER(LEN=30) :: YWIGROUND_LAYER_DIM_NAME ="ground_ice_layer"
CHARACTER(LEN=30) :: YSNOW_PATCH_DIM_NAME    ="snow_patch"
CHARACTER(LEN=30) :: YSEAICE_LAYER_DIM_NAME  ="seaice_layer"
CHARACTER(LEN=30) :: YSWBAND_DIM_NAME        ="swband"
CHARACTER(LEN=30) :: YATM_VAXIS_NAME         ="klev"

!
END MODULE MODD_XIOS
