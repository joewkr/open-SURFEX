!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE INIT_OUTFN_ISBA_n (IM, UG, U, HSELECT, OSNOWDIMNC, HPROGRAM, KLUOUT)
!     ###############################
!
!
!!****  *INIT_OUTFN_ISBA_n* -  create output files and defines variables
!!
!!    PURPOSE
!!    -------
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
!!      F. Habets   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07-03
!!      modified    11-03,by P. Le Moigne   *Meteo France*
!!      modified    05-04,by P. Le Moigne : surf_atm diagnostics moved at the
!!                                           right place
!!      modified    10-04,by P. Le Moigne : add new diagnostics
!!      modified    10-04,by P. Le Moigne : add Halstead coefficient
!!      modified     2008,by B. Decharme  : limit the number of diag
!!                                           Add floodplains diag
!!      modified    04-09,by A.L. Gibelin : Add respiration diagnostics
!!      modified    05-09,by A.L. Gibelin : Add carbon spinup
!!      modified    07-09,by A.L. Gibelin : Add carbon prognostic variables
!!  
!!      modified    09-12,by B. Decharme  : delete LPROVAR_TO_DIAG for prognostic variables
!!                                           delete NWG_LAYER
!!                                           Erroneous description in diag comments
!!      modified    06-13,by B. Decharme  : good dimension for Tg,Wg,et Wgi
!!                                           bug : TSN_VEG if Snowlayer = 1 ; 
!!                                           bug : TSRAD_P and not TTSRAD_P
!!                                           add diag (Qsb,Subl) and Snow noted SN
!!      modified    10-14,by P. Samuelsson: Added MEB output
!!      modified    09-15  by M. Lafaysse  : new Crocus-MEPRA outputs
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_SURF_ATM_GRID_n,ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n,ONLY : SURF_ATM_t
!
USE MODD_OL_FILEID,       ONLY : XNETCDF_FILEID_OUT, XNETCDF_FILENAME_OUT
!
USE MODD_ASSIM,          ONLY : LASSIM, CASSIM, CASSIM_ISBA
!
USE MODD_WRITE_SURF_ATM, ONLY : LSPLIT_PATCH
!
USE MODN_IO_OFFLINE,      ONLY : XTSTEP_OUTPUT
!
USE MODI_GET_DIM_FULL_n
USE MODI_GET_ISBA_CONF_n
USE MODI_OL_DEFINE_DIM
USE MODI_GET_DATE_OL
USE MODI_CREATE_FILE
USE MODI_DEF_VAR_NETCDF
USE MODI_OL_WRITE_COORD
USE MODI_OL_WRITE_PROJ
!
USE YOMHOOK ,ONLY : LHOOK,  DR_HOOK
USE PARKIND1,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
TYPE(ISBA_MODEL_t), INTENT(IN) :: IM
!
TYPE(SURF_ATM_GRID_t),INTENT(INOUT) :: UG
TYPE(SURF_ATM_t),INTENT(INOUT) :: U
!
 CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: HSELECT
LOGICAL, INTENT(IN) :: OSNOWDIMNC
!
 CHARACTER(LEN=6),INTENT(IN) :: HPROGRAM
INTEGER,         INTENT(IN) :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100),DIMENSION(:),POINTER :: YNAME_DIM
 CHARACTER(LEN=100),DIMENSION(1)  :: YATT_TITLE,YATT
 CHARACTER(LEN=40),DIMENSION(1)   :: YDATE
 CHARACTER(LEN=13),DIMENSION(1)   :: YUNIT1,YUNIT2
 CHARACTER(LEN=100)               :: YCOMMENT  
 CHARACTER(LEN=50)                :: YFILE
 CHARACTER(LEN=12)                :: YRECFM
 CHARACTER(LEN=3)                 :: YPAS,YPAT
 CHARACTER(LEN=6)                 :: YLVL
 CHARACTER(LEN=3)                 :: YISBA
 CHARACTER(LEN=1)                 :: YNDAYS 
 CHARACTER(LEN=2)                 :: YLVLV
 CHARACTER(LEN=3) :: YSNOW_SCHEME
! 
TYPE(DATE_TIME) :: TPTIME
REAL,DIMENSION(:),POINTER     :: ZX,ZY
REAL,DIMENSION(:), POINTER    :: ZLAT,ZLON
!
INTEGER :: ISNOW_LAYER
INTEGER,DIMENSION(:),POINTER :: IDIMS, IDDIMALL
INTEGER                      :: INI, INPATCH, INLVLD, INLVLS, INBIOMASS,&
                                INLITTER, INLITTLEVS, INSOILCARB
INTEGER                      :: IDIM1, INDIMS, INDIMSALL, INJDIMS
INTEGER                      :: IFILE_ID, IDIMID, JSV
INTEGER                      :: IL,JRET, INSNLAYER, JFILE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------

! 1. Compute output lenght dimension
!-----------------------------------

IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_ISBA_N',0,ZHOOK_HANDLE)
!
TPTIME = IM%S%TTIME
YSNOW_SCHEME = IM%NPE%AL(1)%TSNOW%SCHEME
ISNOW_LAYER  = IM%NPE%AL(1)%TSNOW%NLAYER
!
 CALL GET_DIM_FULL_n(U%NDIM_FULL,INI)
 CALL GET_ISBA_CONF_n(IM%O,ISNOW_LAYER,YISBA,INPATCH,INLVLD,INLVLS,INBIOMASS,&
                       INLITTER,INLITTLEVS,INSOILCARB)  
!
INSNLAYER = 1
IF ( OSNOWDIMNC ) INSNLAYER = ISNOW_LAYER
IF (LSPLIT_PATCH) THEN
  CALL OL_DEFINE_DIM(UG, U%NSIZE_FULL, HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2,&
                     ZX, ZY, IDIMS, IDDIMALL, YNAME_DIM, KNSNLAYER=INSNLAYER,PLAT=ZLAT,PLON=ZLON)
ELSE
  CALL OL_DEFINE_DIM(UG, U%NSIZE_FULL, HPROGRAM, KLUOUT, INI, IDIM1, YUNIT1, YUNIT2,&
                     ZX, ZY, IDIMS, IDDIMALL, YNAME_DIM, KNPATCH=INPATCH, &
                     KNSNLAYER=INSNLAYER, PLAT=ZLAT, PLON=ZLON)
ENDIF
 CALL GET_DATE_OL(TPTIME,XTSTEP_OUTPUT,YDATE(1))
!
INDIMSALL = SIZE(IDDIMALL)
!
IF ( OSNOWDIMNC ) THEN
  INJDIMS = INDIMSALL-2
  INDIMS  = INDIMSALL-1
ELSE
  INJDIMS = INDIMSALL-1
  INDIMS  = INDIMSALL
ENDIF
!
! 4. Create output file for prognostic variables
!----------------------------------------------------------
!
YATT_TITLE(1)='units'
!
YFILE='ISBA_PROGNOSTIC.OUT.nc'
 CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIMALL)
JRET=NF90_REDEF(IFILE_ID) 
!
 CALL OL_WRITE_PROJ(HSELECT,IFILE_ID,UG)
!
DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
  IF (TRIM(YFILE)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
    XNETCDF_FILEID_OUT(JFILE) = IFILE_ID
    EXIT
  ENDIF
ENDDO
!
IF (.NOT. IM%ID%DM%LPROSNOW) THEN
  !
  CALL OL_WRITE_COORD(HSELECT,YFILE,IFILE_ID,IDDIMALL,YATT_TITLE,YNAME_DIM,&
                      YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY,ZLON,ZLAT)
  !
  !
  ! 4. Create output file for fluxes values
  !----------------------------------------------------------
  !
  YFILE='ISBA_DIAGNOSTICS.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIMALL)
  JRET=NF90_REDEF(IFILE_ID) 
  YATT ='dimensionless'
  !
  CALL OL_WRITE_PROJ(HSELECT,IFILE_ID,UG)
  !
  DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
    IF (TRIM(YFILE)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
      XNETCDF_FILEID_OUT(JFILE) = IFILE_ID
      EXIT
    ENDIF
  ENDDO
  !
ENDIF
!
 CALL OL_WRITE_COORD(HSELECT,YFILE,IFILE_ID,IDDIMALL,YATT_TITLE,YNAME_DIM,&
                     YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY,ZLON,ZLAT)
!
IF (IM%ID%O%LSURF_BUDGETC) THEN
  !
  YFILE='ISBA_DIAG_CUMUL.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIMALL)
  JRET=NF90_REDEF(IFILE_ID)
  !
  CALL OL_WRITE_PROJ(HSELECT,IFILE_ID,UG)
  !
  DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
    IF (TRIM(YFILE)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
      XNETCDF_FILEID_OUT(JFILE) = IFILE_ID
      EXIT
    ENDIF
  ENDDO
  !
  CALL OL_WRITE_COORD(HSELECT,YFILE,IFILE_ID,IDDIMALL,YATT_TITLE,&
                      YNAME_DIM,YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY,ZLON,ZLAT)
  !
ENDIF


! 6. Create file for vegetation parameter values
!----------------------------------------------------------

IF( LASSIM.OR.IM%ID%O%LPGD ) THEN
  !
  YFILE='ISBA_VEG_EVOLUTION.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIMALL)
  JRET=NF90_REDEF(IFILE_ID)
  !
  CALL OL_WRITE_PROJ(HSELECT,IFILE_ID,UG)
  !
  DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
    IF (TRIM(YFILE)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
      XNETCDF_FILEID_OUT(JFILE) = IFILE_ID
      EXIT
    ENDIF
  ENDDO  
  !
  CALL OL_WRITE_COORD(HSELECT,YFILE,IFILE_ID,IDDIMALL,YATT_TITLE,YNAME_DIM,&
                      YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY,ZLON,ZLAT)
  !  
ENDIF
!
! 7. Create file for analysis increments for EKF
!----------------------------------------------------------

IF(LASSIM .AND. CASSIM_ISBA=='EKF  ') THEN
  !
  YFILE='ISBA_ANALYSIS.OUT.nc'
  CALL CREATE_FILE(YFILE,IDIMS,YNAME_DIM,IFILE_ID,IDDIMALL)
  JRET=NF90_REDEF(IFILE_ID)
  !  
  CALL OL_WRITE_PROJ(HSELECT,IFILE_ID,UG)
  !
  DO JFILE = 1,SIZE(XNETCDF_FILENAME_OUT) 
    IF (TRIM(YFILE)==TRIM(XNETCDF_FILENAME_OUT(JFILE))) THEN
      XNETCDF_FILEID_OUT(JFILE) = IFILE_ID
      EXIT
    ENDIF
  ENDDO  
  !
  CALL OL_WRITE_COORD(HSELECT,YFILE,IFILE_ID,IDDIMALL,YATT_TITLE,YNAME_DIM,&
                      YUNIT1,YUNIT2,IDIM1,YDATE,ZX,ZY,ZLON,ZLAT)
ENDIF
!
IF (ASSOCIATED(ZX)) DEALLOCATE(ZX,ZY)
DEALLOCATE(ZLON,ZLAT)
!
IF (LHOOK) CALL DR_HOOK('INIT_OUTFN_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_OUTFN_ISBA_n
