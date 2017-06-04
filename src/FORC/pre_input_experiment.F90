!----------------------------
PROGRAM PRE_INPUT_EXPERIMENT
!----------------------------
!!
!!    PURPOSE
!!    -------
!!   This program prepares the input files for offline run:
!!   ie a file named PARAMS.nc containing information for the run
!!   and FORCING.nc which contains meteorological forcing      
!!
!!    METHOD
!!    ------
!!   
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. LeMoigne                  Meteo-France
!!    S. Lafont    05/2009 correct size (JNPTS1:JNPTS2,:) of output arrays for writing netcdf
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     06/06
!!
!----------------------------------------------------------------------------
!
USE MODD_IO_SURF_ASC,ONLY : NNI_FORC
USE MODD_SURF_CONF, ONLY: CPROGNAME
USE MODD_CSTS
USE MODD_TYPE_DATE_SURF
!
USE MODE_POS_SURF
!
USE MODI_CREATE_FILE
USE MODI_WRITE_NETCDF
USE MODI_DEF_VAR_NETCDF
USE MODI_MY_FORC
USE MODI_OPEN_NAMELIST
USE MODI_GET_DATE_OL
USE MODI_OPEN_CLOSE_BIN_ASC_FORC
USE MODI_GET_LUOUT
USE MODI_INI_CSTS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
!----------------------------------------------------------------------------
!
!*    0.     Declaration of parameters
!            -------------------------
!      
IMPLICIT NONE
!
!----------------------------------------------------------------------------
!
INTEGER :: INI                  ! number of forcing points
INTEGER :: INPTS                ! number of forcing time-step in input file
INTEGER :: JNPTS1, JNPTS2       ! number of forcing time-step written in netcdf file!
REAL    :: ZTSTEPFRC            ! time step of atmospheric forcing (s)
REAL*4  :: ZTSTEPFRC4 
!
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZCO2      ! CO2 concentration (kg/m3) 
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZDIR_SW   ! Solar direct   radiation (W/m2)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZSCA_SW   ! Solar diffused radiation (W/m2)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZLW       ! Longwave radiation (W/m2)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZWINDSPEED! Wind speed (m/s)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZWINDDIR  ! Wind dir. (deg. from N, clockwise)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZRAIN     ! rain rate (kg/m2/s)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZSNOW     ! snow rate (kg/m2/s)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZTA       ! temperature (K)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZQA       ! humidity (kg/m3)
REAL*4, DIMENSION(:,:),  ALLOCATABLE :: ZPS       ! pressure (Pa)
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZZREF     ! height of temperature forcing (m)
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZUREF     ! height of wind forcing (m)
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZZS       ! orography (m)
REAL, DIMENSION(:),    ALLOCATABLE :: ZLON      ! longitude (degrees)
REAL, DIMENSION(:),    ALLOCATABLE :: ZLAT      ! latitude  (degrees)
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZLON4      ! longitude (degrees)
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZLAT4      ! latitude  (degrees)
!
REAL*4, DIMENSION(:),    ALLOCATABLE :: ZTIME
!
!----------------------------------------------------------------------------
!      
!*    1.     Declaration of variables
!            ------------------------
!
CHARACTER(LEN=18)      :: YFILE_FORCING_OUT
!
CHARACTER(LEN=6)                  :: YPROG = 'OFFLIN'
!
CHARACTER(LEN=100)                :: YCOMMENT = ' '
!
TYPE (DATE_TIME)                  :: TDTCUR
!
INTEGER, DIMENSION(12)            :: IFILE_ID
INTEGER                           :: IVAR_ID
INTEGER                           :: IRES
INTEGER                           :: IRET
INTEGER,DIMENSION(2)              :: IDDIM,IDIMS
CHARACTER(LEN=100) ,DIMENSION(2)  :: YNAME_DIM
CHARACTER(LEN=100) ,DIMENSION(2)  :: YATT_TITLE,YATT
!
INTEGER                                 :: JT ! loop counter on times
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: ZF ! field to write
CHARACTER(LEN=12) ::  YEXPER    ! experiment name
INTEGER :: ILUNAM, ILUOUT
CHARACTER(LEN=28)  :: YLUOUT    ='LISTING_FORCING'   ! name of the listing
LOGICAL :: GFOUND
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
CHARACTER(LEN=12) :: YEXPERIMENT_NAME
CHARACTER(LEN=4)  :: YEAR, YEAR2
INTEGER           :: IYEAR1, IYEAR2, IYEAR
INTEGER           :: NYEAR1, NYEAR2, JRET
INTEGER           :: NUMBER_GRID_CELLS
INTEGER           :: NUMBER_OF_TIME_STEPS_INPUT
INTEGER           :: NUMBER_OF_TIME_STEPS_FINAL
INTEGER           :: FIRST_TIME_STEP_FINAL, LAST_TIME_STEP_FINAL
REAL              :: ZATM_FORC_STEP, ZDEN
LOGICAL           :: LSPLIT_NC
CHARACTER(LEN=6)  :: YFORCING_FILETYPE       ! output file type:'ASCII ', 'BINARY', 'NETCDF'
NAMELIST/NAM_MY_PARAM/YEXPERIMENT_NAME,NUMBER_GRID_CELLS,NUMBER_OF_TIME_STEPS_INPUT, &
NUMBER_OF_TIME_STEPS_FINAL,FIRST_TIME_STEP_FINAL,LAST_TIME_STEP_FINAL, &
NYEAR1,NYEAR2,LSPLIT_NC,ZATM_FORC_STEP,YFORCING_FILETYPE
!----------------------------------------------------------------------------
!
!*    1.     Initialization of parameter variables of the simulation
!            -------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PRE_INPUT_EXPERIMENT',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT('ASCII ',ILUOUT)
OPEN(UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YLUOUT)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
YFORCING_FILETYPE='NETCDF'
!
NUMBER_OF_TIME_STEPS_FINAL = 0
FIRST_TIME_STEP_FINAL = 0
LAST_TIME_STEP_FINAL = 0
!
NYEAR1 = 9999
NYEAR2 = 9999
!
LSPLIT_NC = .FALSE.
!
ILUOUT=0
CALL OPEN_NAMELIST('ASCII ',ILUNAM,'MY_PARAM.nam                ')
CALL POSNAM(ILUNAM,'NAM_MY_PARAM',GFOUND,ILUOUT)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_MY_PARAM)
CPROGNAME = 'ASCII '
!
YEXPER    = YEXPERIMENT_NAME
INI       = NUMBER_GRID_CELLS
INPTS     = NUMBER_OF_TIME_STEPS_INPUT
IYEAR1    = NYEAR1
IYEAR2    = NYEAR2
ZTSTEPFRC = ZATM_FORC_STEP
IF (NUMBER_OF_TIME_STEPS_FINAL/=0) THEN
  IF (FIRST_TIME_STEP_FINAL/=0) THEN 
    JNPTS1 = FIRST_TIME_STEP_FINAL
    JNPTS2 = FIRST_TIME_STEP_FINAL + NUMBER_OF_TIME_STEPS_FINAL - 1
  ELSEIF (LAST_TIME_STEP_FINAL/=0) THEN
    JNPTS2 = LAST_TIME_STEP_FINAL
    JNPTS1 = LAST_TIME_STEP_FINAL - NUMBER_OF_TIME_STEPS_FINAL + 1
  ELSE
    JNPTS1 = 1
    JNPTS2 = NUMBER_OF_TIME_STEPS_FINAL
  ENDIF
ELSEIF (FIRST_TIME_STEP_FINAL/=0 .AND. LAST_TIME_STEP_FINAL/=0) THEN
  JNPTS1 = FIRST_TIME_STEP_FINAL
  JNPTS2 = LAST_TIME_STEP_FINAL
ELSE
  PRINT*,' ABORT: PROBLEM IN DEFINING FINAL SPELL OF TIME STEPS '
  IF (LHOOK) CALL DR_HOOK('PRE_INPUT_EXPERIMENT',1,ZHOOK_HANDLE)
  STOP
ENDIF
!
print*,' > ==================================================================='
print*,' > PRE_INPUT_EXPERIMENT: YEXPER             = ',YEXPER
print*,' > PRE_INPUT_EXPERIMENT: INI                = ',INI   
print*,' > PRE_INPUT_EXPERIMENT: INPTS              = ',INPTS 
print*,' > PRE_INPUT_EXPERIMENT: JNPTS1             = ',JNPTS1
print*,' > PRE_INPUT_EXPERIMENT: JNPTS2             = ',JNPTS2
print*,' > PRE_INPUT_EXPERIMENT: ZTSTEPFRC          = ',ZTSTEPFRC 
print*,' > PRE_INPUT_EXPERIMENT: LSPLIT_NC          = ',LSPLIT_NC
print*,' > PRE_INPUT_EXPERIMENT: YFORCING_FILETYPE  = ',YFORCING_FILETYPE
print*,' > ==================================================================='
!
ALLOCATE(ZZREF(INI))
ALLOCATE(ZUREF(INI))
ALLOCATE(ZZS(INI))
ALLOCATE(ZLON(INI))
ALLOCATE(ZLAT(INI))
ALLOCATE(ZLON4(INI))
ALLOCATE(ZLAT4(INI))
ALLOCATE(ZTA(INPTS,INI))
ALLOCATE(ZQA(INPTS,INI))
ALLOCATE(ZPS(INPTS,INI))
ALLOCATE(ZWINDSPEED(INPTS,INI))
ALLOCATE(ZWINDDIR(INPTS,INI))
ALLOCATE(ZDIR_SW(INPTS,INI))
ALLOCATE(ZSCA_SW(INPTS,INI))
ALLOCATE(ZLW(INPTS,INI))
ALLOCATE(ZRAIN(INPTS,INI))
ALLOCATE(ZSNOW(INPTS,INI))
ALLOCATE(ZCO2(INPTS,INI))
!
! 
!*    2.     Initialization of forcing variables
!            -----------------------------------
!
CALL INI_CSTS
! 
IF (YFORCING_FILETYPE == 'NETCDF') THEN
  !
  ALLOCATE(ZTIME(INPTS))
  !
  IF (ZTSTEPFRC == FLOOR(ZTSTEPFRC/86400.)*86400) THEN 
    ZDEN = 86400.
  ELSEIF (ZTSTEPFRC == FLOOR(ZTSTEPFRC/3600.)*3600) THEN
    ZDEN = 3600.
  ELSEIF (ZTSTEPFRC == FLOOR(ZTSTEPFRC/60.)*60) THEN
    ZDEN = 60.
  ELSE
    ZDEN = 1.
  ENDIF
  !
  DO JT = 1, INPTS
    ZTIME(JT) = ZTSTEPFRC/ZDEN * (JT-1)
  ENDDO
  !
  TDTCUR%TIME = TDTCUR%TIME + (ZTSTEPFRC-FLOOR(ZTSTEPFRC/86400)*86400)
  !
ELSE
  !
  ALLOCATE(ZF(INI))
  NNI_FORC = INI
  !
ENDIF
!
DO IYEAR=IYEAR1,IYEAR2

  IF (IYEAR/=9999) THEN
    WRITE(YEAR,FMT='(i2,i2)') IYEAR-(IYEAR/100)*100, (IYEAR+1)-((IYEAR+1)/100)*100
    WRITE(YEAR2,FMT='(i2,i2)') (IYEAR+1)-((IYEAR+1)/100)*100, (IYEAR+2)-((IYEAR+2)/100)*100
    TDTCUR%TDATE%YEAR = IYEAR
  ELSE
    YEAR=""
    YEAR2=""
  ENDIF

  CALL MY_FORC(YEXPER,TRIM(YEAR),TRIM(YEAR2),INI,INPTS, &
             TDTCUR%TDATE%YEAR,TDTCUR%TDATE%MONTH,      &
             TDTCUR%TDATE%DAY,TDTCUR%TIME,              &
             ZLON, ZLAT, ZZS, ZZREF, ZUREF,             &
             ZTA, ZQA, ZPS, ZWINDSPEED, ZWINDDIR,       &
             ZDIR_SW, ZSCA_SW, ZLW, ZRAIN, ZSNOW, ZCO2  )
!
!----------------------------------------------------------------------------
!      
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!---- DONT MODIFY BELOW HERE !!! --------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
IF (YFORCING_FILETYPE == 'BINARY') THEN
!      
!*    4.     Writing in binary files
!            -----------------------
!
CALL OPEN_CLOSE_BIN_ASC_FORC('CONF ','BINARY','W',HYEAR=TRIM(YEAR))
CALL OPEN_CLOSE_BIN_ASC_FORC('OPEN ','BINARY','W',HYEAR=TRIM(YEAR))
!
!* configuration file is written for both BINARY and ASCII forcing file types
WRITE(21,*) 'N' !for the forcing swap: set Y to swap, N not to swap
WRITE(21,*) INI
WRITE(21,*) JNPTS2-JNPTS1+1
WRITE(21,*) ZTSTEPFRC
WRITE(21,*) TDTCUR%TDATE%YEAR
WRITE(21,*) TDTCUR%TDATE%MONTH
WRITE(21,*) TDTCUR%TDATE%DAY
WRITE(21,*) TDTCUR%TIME
WRITE(21,FMT='(50(F15.8))') ZLON
WRITE(21,FMT='(50(F15.8))') ZLAT
WRITE(21,FMT='(50(F15.8))') ZZS
WRITE(21,FMT='(50(F15.8))') ZZREF
WRITE(21,FMT='(50(F15.8))') ZUREF
!
DO JT=JNPTS1,JNPTS2
  ZF = ZTA(JT,:)
  WRITE(22,REC=JT) ZF(:)
  ZF = ZQA(JT,:)
  WRITE(23,REC=JT) ZF(:)
  ZF = ZWINDSPEED(JT,:)
  WRITE(24,REC=JT) ZF(:)
  ZF = ZLW(JT,:)
  WRITE(25,REC=JT) ZF(:)
  ZF = ZDIR_SW(JT,:)
  WRITE(26,REC=JT) ZF(:)
  ZF = ZSCA_SW(JT,:)
  WRITE(27,REC=JT) ZF(:)
  ZF = ZRAIN(JT,:)
  WRITE(28,REC=JT) ZF(:)
  ZF = ZSNOW(JT,:)
  WRITE(29,REC=JT) ZF(:)
  ZF = ZPS(JT,:)
  WRITE(30,REC=JT) ZF(:)
  ZF = ZWINDDIR(JT,:)
  WRITE(31,REC=JT) ZF(:)
  ZF = ZCO2(JT,:)
  WRITE(32,REC=JT) ZF(:)
END DO
!
CALL OPEN_CLOSE_BIN_ASC_FORC('CLOSE','BINARY','W')
!
!----------------------------------------------------------------------------
!
ELSE IF (YFORCING_FILETYPE == 'ASCII ') THEN
!      
!*    5.     Writing in ASCII files
!            ----------------------
!
CALL OPEN_CLOSE_BIN_ASC_FORC('CONF ','ASCII ','W',TRIM(YEAR))
CALL OPEN_CLOSE_BIN_ASC_FORC('OPEN ','ASCII ','W',TRIM(YEAR))
!
!* configuration file is written for both BINARY and ASCII forcing file types
WRITE(21,*) INI
WRITE(21,*) JNPTS2-JNPTS1+1
WRITE(21,*) ZTSTEPFRC
WRITE(21,*) TDTCUR%TDATE%YEAR
WRITE(21,*) TDTCUR%TDATE%MONTH
WRITE(21,*) TDTCUR%TDATE%DAY
WRITE(21,*) TDTCUR%TIME
WRITE(21,FMT='(50(F15.8))') ZLON
WRITE(21,FMT='(50(F15.8))') ZLAT
WRITE(21,FMT='(50(F15.8))') ZZS
WRITE(21,FMT='(50(F15.8))') ZZREF
WRITE(21,FMT='(50(F15.8))') ZUREF
!
!* writes forcing in ASCII files
DO JT=JNPTS1,JNPTS2
  WRITE(UNIT=22,FMT='(50(F20.5))') ZTA(JT,:)
  WRITE(UNIT=23,FMT='(50(F20.5))') ZQA(JT,:)
  WRITE(UNIT=24,FMT='(50(F20.5))') ZWINDSPEED(JT,:)
  WRITE(UNIT=25,FMT='(50(F20.5))') ZLW(JT,:)
  WRITE(UNIT=26,FMT='(50(F20.5))') ZDIR_SW(JT,:)
  WRITE(UNIT=27,FMT='(50(F20.5))') ZSCA_SW(JT,:)
  WRITE(UNIT=28,FMT='(50(F20.5))') ZRAIN(JT,:)
  WRITE(UNIT=29,FMT='(50(F20.5))') ZSNOW(JT,:)
  WRITE(UNIT=30,FMT='(50(F20.5))') ZPS(JT,:)
  WRITE(UNIT=31,FMT='(50(F20.5))') ZWINDDIR(JT,:)
  WRITE(UNIT=32,FMT='(50(F20.5))') ZCO2(JT,:)
END DO
CALL OPEN_CLOSE_BIN_ASC_FORC('CLOSE','ASCII ','W')
!
!----------------------------------------------------------------------------
!
ELSE IF (YFORCING_FILETYPE == 'NETCDF') THEN
!      
!*    4.     Writing of PARAMS.nc file
!            -------------------------
!
!----------------------------------------------------------------------------
!      
!        4.1    define dimensions
!               -----------------
!
IDIMS(1) = INI
!
!----------------------------------------------------------------------------
!      
!        4.2    define dimension names
!               ----------------------
!
YNAME_DIM(1) = 'Number_of_points'
!
!----------------------------------------------------------------------------
!      
!*       5.     Writing of FORCING.nc file
!               --------------------------
!
!----------------------------------------------------------------------------
!      
!        5.1    define dimensions
!               -----------------
!
IDIMS(1) = INI    ! space dimension
IDIMS(2) = JNPTS2-JNPTS1+1  ! time dimension   
!
!----------------------------------------------------------------------------
!      
!        5.2    define dimension names
!               ----------------------
!
YNAME_DIM(1) = 'Number_of_points'
YNAME_DIM(2) = 'time'
!
!----------------------------------------------------------------------------
!    
!        5.3    create file
!               -----------
!
IF (LSPLIT_NC) THEN
  !
  YFILE_FORCING_OUT = TRIM(YEAR)//'PARAMS.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS(1:1),YNAME_DIM(1:1),IFILE_ID(1),IDDIM)
  !
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_TA.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(2),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_QA.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(3),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_PS.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(4),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_DIR_SW.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(5),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_SCA_SW.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(6),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_LW.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(7),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_RAIN.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(8),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_SNOW.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(9),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_WIND.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(10),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_DIR.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(11),IDDIM)
  YFILE_FORCING_OUT = TRIM(YEAR)//'Forc_CO2.nc'
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(12),IDDIM)
  !
ELSE
  !
  YFILE_FORCING_OUT = TRIM(YEAR)//'FORCING.nc'
  !
  CALL CREATE_FILE(YFILE_FORCING_OUT,IDIMS,YNAME_DIM(1:2),IFILE_ID(1),IDDIM)
  IFILE_ID(2:12) = IFILE_ID(1)
  !
ENDIF
!----------------------------------------------------------------------------
!
CALL GET_DATE_OL(TDTCUR,ZTSTEPFRC,YATT(1))
YATT_TITLE(1) = 'units'
IF (LSPLIT_NC) THEN
  CALL WRITE_NETCDF(IFILE_ID(2),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(3),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(4),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(5),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(6),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(7),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(8),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(9),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(10),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(11),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
  CALL WRITE_NETCDF(IFILE_ID(12),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
ELSE
  CALL WRITE_NETCDF(IFILE_ID(1),'time','',ZTIME(:),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
ENDIF
!
ZTSTEPFRC4 = ZTSTEPFRC
CALL WRITE_NETCDF(IFILE_ID(1),'FRC_TIME_STP','Forcing_Time_Step',ZTSTEPFRC4)
!
ZLON4(:) = ZLON(:)
ZLAT4(:) = ZLAT(:)
CALL WRITE_NETCDF(IFILE_ID(1),'LON','Longitude', ZLON4, IDDIM(1))
CALL WRITE_NETCDF(IFILE_ID(1),'LAT','Latitude', ZLAT4, IDDIM(1))
!
CALL WRITE_NETCDF(IFILE_ID(1),'ZS','Surface_Orography',ZZS, IDDIM(1))
!
YATT_TITLE(1)='units'
YATT(1) = 'm'
CALL WRITE_NETCDF(IFILE_ID(1),'ZREF','Reference_Height',ZZREF,IDDIM(1),YATT_TITLE(1:1),YATT(1:1))
CALL WRITE_NETCDF(IFILE_ID(1),'UREF','Reference_Height_for_Wind',ZUREF,IDDIM(1),YATT_TITLE(1:1),YATT(1:1))
!
! 2D VARIABLES WITH 2 COMMENTS
!
YATT_TITLE(1) = 'measurement_height' 
YATT      (1) = '2m'
YATT_TITLE(2) = 'units'
!
YATT      (2) = 'K'
CALL WRITE_NETCDF(IFILE_ID(2),'Tair','Near_Surface_Air_Temperature',TRANSPOSE(ZTA(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:2),YATT(1:2))
!
YATT      (2) = 'Kg/Kg'
CALL WRITE_NETCDF(IFILE_ID(3),'Qair','Near_Surface_Specific_Humidity',TRANSPOSE(ZQA(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:2),YATT(1:2))
!
YATT(1) = 'Pa'
CALL WRITE_NETCDF(IFILE_ID(4),'PSurf','Surface_Pressure',TRANSPOSE(ZPS(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
!
! 2D VARIABLES WITH 1 COMMENT
!
YATT_TITLE(1) = 'units' 
!
YATT(1) = 'W/m2'
CALL WRITE_NETCDF(IFILE_ID(5),'DIR_SWdown','Surface_Indicent_Direct_Shortwave_Radiation' ,&
        TRANSPOSE(ZDIR_SW(JNPTS1:JNPTS2,:)),IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT(1) = 'W/m2'
CALL WRITE_NETCDF(IFILE_ID(6),'SCA_SWdown','Surface_Incident_Diffuse_Shortwave_Radiation' ,&
        TRANSPOSE(ZSCA_SW(JNPTS1:JNPTS2,:)),IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT(1) = 'W/m2'
CALL WRITE_NETCDF(IFILE_ID(7),'LWdown','Surface_Incident_Longwave_Radiation' ,&
        TRANSPOSE(ZLW(JNPTS1:JNPTS2,:)),IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT(1) = 'Kg/m2/s'
CALL WRITE_NETCDF(IFILE_ID(8),'Rainf','Rainfall_Rate',TRANSPOSE(ZRAIN(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT(1) = 'Kg/m2/s'
CALL WRITE_NETCDF(IFILE_ID(9),'Snowf','Snowfall_Rate',TRANSPOSE(ZSNOW(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT      (2) = 'm/s'
CALL WRITE_NETCDF(IFILE_ID(10),'Wind','Wind_Speed',TRANSPOSE(ZWINDSPEED(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:2),YATT(1:2))
!
YATT(1) = 'deg'
CALL WRITE_NETCDF(IFILE_ID(11),'Wind_DIR','Wind_Direction',TRANSPOSE(ZWINDDIR(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
YATT(1) = 'Kg/m3'
CALL WRITE_NETCDF(IFILE_ID(12),'CO2air','Near_Surface_CO2_Concentration',TRANSPOSE(ZCO2(JNPTS1:JNPTS2,:)),&
        IDDIM(1),IDDIM(2),YATT_TITLE(1:1),YATT(1:1))
!
!              2.4 closing file
!     ------------
!
IRET=NF90_CLOSE(IFILE_ID(1))
IF (LSPLIT_NC) THEN
  IRET=NF90_CLOSE(IFILE_ID(2))
  IRET=NF90_CLOSE(IFILE_ID(3))
  IRET=NF90_CLOSE(IFILE_ID(4))
  IRET=NF90_CLOSE(IFILE_ID(5))
  IRET=NF90_CLOSE(IFILE_ID(6))
  IRET=NF90_CLOSE(IFILE_ID(7))
  IRET=NF90_CLOSE(IFILE_ID(8))
  IRET=NF90_CLOSE(IFILE_ID(9))
  IRET=NF90_CLOSE(IFILE_ID(10))
  IRET=NF90_CLOSE(IFILE_ID(11))
  IRET=NF90_CLOSE(IFILE_ID(12))
  ENDIF  
!
ELSE
  PRINT*,' ABORT: CHECK YFORCING_FILETYPE '
  IF (LHOOK) CALL DR_HOOK('PRE_INPUT_EXPERIMENT',1,ZHOOK_HANDLE)
  STOP
ENDIF
!
CLOSE(ILUOUT)
!
ENDDO
!
IF (YFORCING_FILETYPE=='NETCDF') THEN
  DEALLOCATE(ZTIME)
ELSE
  DEALLOCATE(ZF)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PRE_INPUT_EXPERIMENT',1,ZHOOK_HANDLE)
END
