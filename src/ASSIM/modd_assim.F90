!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODD_ASSIM
!     ##################
!
!!****  *MODD_ASSIM - declaration of keys for assimilation schemes
!!
!!    PURPOSE
!!    -------
!
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
!!      L. Jarlan   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       23/02/05
! 
!       Inclusion of OI constants 21/05/09 (J.-F. Mahfouf)  
!!       Add all assim keys         04/2012  T.Aspelien
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE

!-------------------------------------------------------------------------------
!
! Assimilation Scheme Options:
!
 LOGICAL                               :: LASSIM               ! Assimilation or not
                                                               !'.TRUE.'
                                                               !'.FALSE.'
 LOGICAL                               :: LREAD_ALL = .TRUE.
 LOGICAL                               :: LAROME               ! If reading AROME fields
 LOGICAL                               :: LECSST               ! Use ECMWF SST
 LOGICAL                               :: LAESST               ! SST analysis performed
 LOGICAL                               :: LAESNM               ! Update snow analysis
 LOGICAL                               :: LALADSURF            
 LOGICAL                               :: LREAD_SST_FROM_FILE  ! Read SST from file
 CHARACTER(LEN=6)                      :: CFILE_FORMAT_SST     ! Format of the SST file ASCII/FA
 CHARACTER(LEN=6)                      :: CFILE_FORMAT_FG      ! Format of the first guess file ASCII/FA (OI)
 CHARACTER(LEN=6)                      :: CFILE_FORMAT_LSM     ! Format of the LSM file ASCII/FA (OI/extrapolations)
 CHARACTER(LEN=6)                      :: CFILE_FORMAT_OBS     ! Format of the observations file ASCII/FA
 CHARACTER(LEN=6)                      :: CFILE_FORMAT_CLIM    ! Format of the climate file ASCII/FA (OI)
 LOGICAL                               :: LWATERTG2            ! Use deep soil temperature as lake temp.
 LOGICAL                               :: LEXTRAP_SEA          ! Extrapolation of sea points
 LOGICAL                               :: LEXTRAP_WATER        ! Extrapolation of inland water  points
 LOGICAL                               :: LEXTRAP_NATURE       ! Extrapolation of nature points
 LOGICAL                               :: LPRT                 ! Running VARASSIM in a perturbation mode
 LOGICAL                               :: LSIM                 ! Running VARASSIM in a perturbation mode 
 LOGICAL                               :: LBEV                 ! Running VARASSIM to evolve B
 LOGICAL                               :: LBFIXED    
 LOGICAL                               :: LOBSHEADER
 LOGICAL                               :: LOBSNAT

 INTEGER, PARAMETER                    :: NOBSMAX = 5          ! Maximum number of observations
 INTEGER, PARAMETER                    :: NVARMAX = 9          ! Maximum number of control variables
 INTEGER,DIMENSION(NOBSMAX)            :: NNCO                 ! Select the type of observations to be assimilated 
 INTEGER,DIMENSION(NVARMAX)            :: NNCV                 ! Select the control variables to be used 
 INTEGER                               :: NOBSTYPE
 INTEGER                               :: NOBS
 INTEGER                               :: NIPERT 
 INTEGER                               :: NIFIC
 INTEGER                               :: NIVAR                ! counter for ctnrl vars
 INTEGER                               :: NVAR                 ! number of cntrl vars
 INTEGER                               :: NBOUTPUT  
 INTEGER                               :: NPRINTLEV            ! Verbosity 

 CHARACTER(LEN=10),DIMENSION(NOBSMAX)  :: COBS_M               ! Observation variable name
 CHARACTER(LEN=3),DIMENSION(NVARMAX)   :: CVAR_M               ! X is ctrl
                                                               ! 'PLUS ' (default)
                                                               ! 'AVERA'            
                                                               ! '2DVAR'
 CHARACTER(LEN=100),DIMENSION(NVARMAX) :: CPREFIX_M            ! The prefix of the control variables (in PREP.txt file) (max dim)      
 CHARACTER(LEN=10),DIMENSION(:), ALLOCATABLE  :: COBS          ! Identifier for simulated observations
 CHARACTER(LEN=3),DIMENSION(:), ALLOCATABLE   :: CVAR          ! Identifier for control variable
 CHARACTER(LEN=12)                     :: CBIO                 ! Name of Biomass variable
 CHARACTER(LEN=100)                    :: CPREFIX_BIO          ! The prefix of the Biomass variable 
 CHARACTER(LEN=5)                      :: CASSIM_ISBA          ! OI/EKF
 CHARACTER(LEN=5)                      :: CASSIM               ! type of correction

 REAL,DIMENSION(12)                     :: XALPH
 REAL,DIMENSION(NVARMAX)               :: XTPRT_M              ! The perturbation amplitude (max dim)
 REAL,DIMENSION(NVARMAX)               :: XSIGMA_M             ! covariance of background errors if B is fixed (max dim)
!                                                              ! covariance of model errors if B evolving (max dim)
 REAL,DIMENSION(NOBSMAX)               :: XERROBS_M            ! Observational standard deviation
 REAL,DIMENSION(NOBSMAX)               :: XQCOBS_M 
 REAL,DIMENSION(:,:,:,:),ALLOCATABLE   :: XF_PATCH             ! vector of model observations (for each pacth)
 REAL,DIMENSION(:,:,:,:),ALLOCATABLE   :: XF                   ! Vector of forecast control variables 
 REAL,DIMENSION(:,:,:),ALLOCATABLE     :: XI 
 REAL,DIMENSION(:,:), ALLOCATABLE      :: XYO                  ! vector of observations
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XLAI_PASS
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XBIO_PASS
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XAT2M_ISBA
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XAHU2M_ISBA
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XAZON10M_ISBA
 REAL,DIMENSION(:,:),ALLOCATABLE       :: XAMER10M_ISBA
 REAL,DIMENSION(:),ALLOCATABLE         :: XAT2M_TEB
 REAL,DIMENSION(:),ALLOCATABLE         :: XTPRT           ! The perturbation amplitude
 REAL,DIMENSION(:),ALLOCATABLE         :: XSIGMA          ! covariance of background errors if B is fixed
                                                          ! covariance of model errors if B evolving  
 REAL,DIMENSION(:),ALLOCATABLE         :: XERROBS 
 REAL,DIMENSION(:),ALLOCATABLE         :: XQCOBS
 REAL                                  :: XSCALE_Q        ! scaling factor of Q matrix w.r.t. the initial B
 REAL                                  :: XSCALE_QLAI
!
INTEGER :: NENS
INTEGER :: NIE
REAL :: XASSIM_WINH
REAL, DIMENSION(NVARMAX) :: XINFL_M
REAL, DIMENSION(NVARMAX) :: XADDINFL_M
REAL, DIMENSION(NVARMAX) :: XADDTIMECORR_M
REAL, DIMENSION(:), ALLOCATABLE :: XINFL
REAL, DIMENSION(:), ALLOCATABLE :: XADDINFL
REAL, DIMENSION(:), ALLOCATABLE :: XADDTIMECORR
LOGICAL :: LENKF
LOGICAL :: LDENKF
LOGICAL :: LENS_GEN
LOGICAL :: LPB_CORRELATIONS
LOGICAL :: LPERTURBATION_RUN
LOGICAL :: LBIAS_CORRECTION
!
! Constants and options of the soil OI analysis
!
 LOGICAL ::  LHUMID,  LIMVEG, LISSEW,   L_SM_WP, LFGEL,      LCLIM,   LLDHMT,  &
             LOBSWG,  LOBS2M 
 INTEGER ::  NMINDJ,   NNEBUL, NNEIGT,   NNEIGW,  NR_SM_WP,  NECHGU,  NTVGLA,  &
             NSEAICE, NLISSEW, NIDJ,    NITRAD 
 REAL    ::  XANEBUL,  XRCLIMN, XRCLIMTP,  XRCLIMTS, XRCLIMV, XRCLIMWP, XRCLIMWS, &
             XSCOEFH,  XSCOEFT, XSEVAP,    XSIGH2MO, XSIGT2MO,    XSNEIGT,  XSNEIGW,  &
             XSPRECIP, XSWFC,   XV10MX,    XRD1,     XRTINER,     XWCRIN,   XWPMX,    &
             XWSMX,    XTMERGL, XRZHZ0G,   XRCLIMCA, XRCLISST,    XRWPIA,   XRWPIB,   &
             XRSNSA,   XRSNSB,  XSALBM,    XSALBB,   XSEMIB,      XSZZ0B,   XSMU0,    &
             XSICE,    XSEMIM,  XRA_SM_WP, XRSCALDW, XSPRECIP2,                     &
             XREPSM,   XRCDTR,  XSIGHP1,   XSIGT2MR, XSIGH2MR,    XRSABR,            &
             XRARGR,   XGWFC,   XEWFC,     XGWWILT,  XEWWILT,     XG1WSAT,  XG2WSAT,  &
             XREPS1,   XREPS2,  XREPS3,    XADWR,    XSODELX(0:9),                  &
             XSIGWGO,  XSIGWGB, XSIGW2B,   XRTHR_QC, XSIGWGO_MAX, XRSCAL_JAC
!
END MODULE MODD_ASSIM
