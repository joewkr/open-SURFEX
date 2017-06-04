!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_OROGRAPHY (DTCO, UG, U, USS, HPROGRAM, HFILE, HFILETYPE, OZS)
!     ##############################################################
!
!!**** *PGD_OROGRAPHY* monitor for averaging and interpolations of cover fractions
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
!!    Original    10/12/97
!!    12/2008 E. Martin : add case 'MAX' for choice of orography
!!    11/2012 M. Lafaysse : read ZS from a NETCDF file at the same resolution
!!    07/2013 M. Lafaysse : explicit slope from resolved orography
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_PGD_GRID,       ONLY : NL, CGRID, XGRID_PAR
USE MODD_PGDWORK,        ONLY : XALL, NSIZE_ALL, XSSQO, LSSQO, NSSO, &
                                XEXT_ALL, XSUMVAL, NSIZE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_NAM_PGD_OROGRAPHY
USE MODI_READ_SURF
USE MODI_TREAT_FIELD
USE MODI_READ_PGD_NETCDF
USE MODI_INTERPOL_FIELD
USE MODI_SSO
USE MODI_SUBSCALE_AOS
USE MODI_GET_SIZE_FULL_n
USE MODI_TEST_NAM_VAR_SURF
!
USE MODI_READ_SSO_n
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
#ifdef SFX_ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef SFX_FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef SFX_LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE MODI_EXPLICIT_SLOPE

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!

USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE    ! atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE! atmospheric file type
LOGICAL,              INTENT(IN)  :: OZS      ! .true. if orography is imposed by atm. model
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT    ! output listing logical unit
!

REAL,DIMENSION(:),POINTER :: ZSLOPE ! degrees
INTEGER::JJ
REAL,PARAMETER :: PP_DEG2RAD= 3.141592654/180.
LOGICAL:: LPRESENT

LOGICAL, DIMENSION(NL)   :: GSSO        ! mask where SSO are computed
LOGICAL, DIMENSION(NL)   :: GSSO_ANIS   ! mask where SSO anisotropy is computed
LOGICAL, DIMENSION(NL)   :: GZ0EFFI     ! mask where z0  is  computed in subgrid direction x
LOGICAL, DIMENSION(NL)   :: GZ0EFFJ     ! mask where z0  is  computed in subgrid direction y
INTEGER, DIMENSION(NL)   :: IFLAG       ! flag for SSO and z0 fields interpolations
INTEGER                  :: IRESP       ! error code
REAL                     :: ZEPS = 1.E-4! a small number
INTEGER                  :: IDIM_FULL   ! total size of orographic array in atmospheric file
INTEGER                  :: IZS         ! size of orographic array in atmospheric file
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YZS         ! file name for orography
 CHARACTER(LEN=6)         :: YFILETYPE   ! data file type
CHARACTER(LEN=28)        :: YSLOPE         ! file name for orography
CHARACTER(LEN=6)         :: YSLOPEFILETYPE   ! data file type
REAL                     :: XUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3)         :: COROGTYPE   ! orogpraphy type 
!                                       ! 'AVG' : average orography
!                                       ! 'SIL' : silhouette orography
!                                       ! 'ENV' : enveloppe orography
REAL                     :: XENV        ! parameter for enveloppe orography:
!                                       ! zs = avg_zs + XENV * SSO_STEDV
LOGICAL                  :: LIMP_ZS     ! Imposed orography from another PGD file
LOGICAL                  :: LEXPLICIT_SLOPE ! Slope is computed from explicit ZS field and not subgrid orography

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_OROGRAPHY(HPROGRAM, YZS, YFILETYPE, XUNIF_ZS, &
                              COROGTYPE, XENV, LIMP_ZS, &
                              YSLOPE, YSLOPEFILETYPE, LEXPLICIT_SLOPE)  
!
 CALL TEST_NAM_VAR_SURF(ILUOUT,'YSLOPEFILETYPE',YSLOPEFILETYPE,'      ','NETCDF')
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocations of orographic arrays
!             --------------------------------
!
ALLOCATE(U%XZS        (NL))
!
ALLOCATE(USS%XAVG_ZS    (NL))
ALLOCATE(USS%XSIL_ZS    (NL))
ALLOCATE(USS%XSSO_STDEV (NL))
ALLOCATE(USS%XMIN_ZS    (NL))
ALLOCATE(USS%XMAX_ZS    (NL))
!
ALLOCATE(USS%XSSO_ANIS  (NL))
ALLOCATE(USS%XSSO_DIR   (NL))
ALLOCATE(USS%XSSO_SLOPE (NL))
!
ALLOCATE(USS%XAOSIP     (NL))
ALLOCATE(USS%XAOSIM     (NL))
ALLOCATE(USS%XAOSJP     (NL))
ALLOCATE(USS%XAOSJM     (NL))
ALLOCATE(USS%XHO2IP     (NL))
ALLOCATE(USS%XHO2IM     (NL))
ALLOCATE(USS%XHO2JP     (NL))
ALLOCATE(USS%XHO2JM     (NL))
!
U%XZS       (:) = XUNDEF
USS%XAVG_ZS   (:) = XUNDEF
USS%XSIL_ZS   (:) = XUNDEF
USS%XSSO_STDEV(:) = XUNDEF
USS%XMIN_ZS   (:) = 99999.
USS%XMAX_ZS   (:) =-99999. 
!
USS%XSSO_ANIS (:) = XUNDEF
USS%XSSO_DIR  (:) = XUNDEF
USS%XSSO_SLOPE(:) = XUNDEF
!
USS%XAOSIP    (:) = XUNDEF
USS%XAOSIM    (:) = XUNDEF
USS%XAOSJP    (:) = XUNDEF
USS%XAOSJM    (:) = XUNDEF
USS%XHO2IP    (:) = XUNDEF
USS%XHO2IM    (:) = XUNDEF
USS%XHO2JP    (:) = XUNDEF
USS%XHO2JM    (:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*    4.      Allocations of work arrays
!             --------------------------
!
!-------------------------------------------------------------------------------
!
!*    5.      Uniform field is prescribed
!             ---------------------------
!
IF (OZS) THEN
!
!*    5.1     Use of imposed field
!             --------------------
!
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
  CALL READ_SURF(HFILETYPE,'DIM_FULL  ',IDIM_FULL,IRESP)
  CALL GET_SIZE_FULL_n(HPROGRAM,IDIM_FULL,U%NSIZE_FULL,IZS)
  IF (IZS /= NL) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in orography preparation                          *'
    WRITE(ILUOUT,*) '* Prescribed orography from atmospheric model does not    *'
    WRITE(ILUOUT,*) '* have the correct number of points                       *'
    WRITE(ILUOUT,*) '* number of points in atmospheric orography: ', IZS
    WRITE(ILUOUT,*) '* number of points in the surface          : ', NL
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_OROGRAPHY: ATMOSPHERIC PRESCRIBED OROGRAPHY DOES NOT HAVE THE CORRECT NB OF POINTS')
  END IF
  CALL READ_SURF(HFILETYPE,'ZS',U%XZS(:),IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  !
  USS%XAVG_ZS(:)    = U%XZS(:)
  USS%XSIL_ZS(:)    = U%XZS(:)
  USS%XMIN_ZS(:)    = U%XZS(:)
  USS%XMAX_ZS(:)    = U%XZS(:)
  USS%XSSO_STDEV(:) = 0.
  USS%XHO2IP(:)     = 0.
  USS%XHO2IM(:)     = 0.
  USS%XHO2JP(:)     = 0.
  USS%XHO2JM(:)     = 0.
  USS%XAOSIP(:)     = 0.
  USS%XAOSIM(:)     = 0.
  USS%XAOSJP(:)     = 0.
  USS%XAOSJM(:)     = 0.
  USS%XSSO_ANIS(:)  = 0.
  USS%XSSO_DIR(:)   = 0.
  USS%XSSO_SLOPE(:) = 0.

  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN

!
ELSE IF (XUNIF_ZS/=XUNDEF) THEN
!
!*    5.2     Use of the presribed cover fractions
!             ------------------------------------
!
  U%XZS(:)        = XUNIF_ZS
  USS%XAVG_ZS(:)    = U%XZS(:)
  USS%XSIL_ZS(:)    = U%XZS(:)
  USS%XMIN_ZS(:)    = U%XZS(:)
  USS%XMAX_ZS(:)    = U%XZS(:)
  USS%XSSO_STDEV(:) = 0.
  USS%XHO2IP(:)     = 0.
  USS%XHO2IM(:)     = 0.
  USS%XHO2JP(:)     = 0.
  USS%XHO2JM(:)     = 0.
  USS%XAOSIP(:)     = 0.
  USS%XAOSIM(:)     = 0.
  USS%XAOSJP(:)     = 0.
  USS%XAOSJM(:)     = 0.
  USS%XSSO_ANIS(:)  = 0.
  USS%XSSO_DIR(:)   = 0.
  USS%XSSO_SLOPE(:) = 0.

  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN
!
!*    5.3     No data
!             -------
!
ELSEIF (LEN_TRIM(YZS)==0) THEN

  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in orography preparation                          *'
  WRITE(ILUOUT,*) '* There is no prescribed orography and no input file      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_OROGRAPHY: NO PRESCRIBED OROGRAPHY NOR INPUT FILE')
!  
ELSEIF (LIMP_ZS) THEN !LIMP_ZS (impose topo from input file at the same resolution)
!
  IF(YFILETYPE=='NETCDF')THEN
     
!      CALL ABOR1_SFX('Use another format than netcdf for topo input file with LIMP_ZS')
    CALL READ_PGD_NETCDF(UG, U, USS, &
                          HPROGRAM,'SURF  ','      ',YZS,'ZS                  ',U%XZS)
     
    USS%XSIL_ZS(:)    = U%XZS(:)
    USS%XAVG_ZS(:)    = U%XZS(:)
    USS%XMIN_ZS(:)    = U%XZS(:)
    USS%XMAX_ZS(:)    = U%XZS(:)
    USS%XSSO_STDEV(:) = 0.
    USS%XHO2IP(:)     = 0.
    USS%XHO2IM(:)     = 0.
    USS%XHO2JP(:)     = 0.
    USS%XHO2JM(:)     = 0.
    USS%XAOSIP(:)     = 0.
    USS%XAOSIM(:)     = 0.
    USS%XAOSJP(:)     = 0.
    USS%XAOSJM(:)     = 0.
    USS%XSSO_ANIS(:)  = 0.
    USS%XSSO_DIR(:)   = 0.
     
     
    ! read slope in file
    IF (LEN_TRIM(YSLOPE)/=0) THEN
      ALLOCATE(ZSLOPE(NL))

    ! Read field on the same grid as FORCING
      CALL READ_PGD_NETCDF(UG, U, USS, &
                          HPROGRAM,'SURF  ','      ',YSLOPE,'slope               ',ZSLOPE)

      DO JJ=1,NL
        USS%XSSO_SLOPE(JJ)=TAN(ZSLOPE(JJ)*PP_DEG2RAD)
      END DO
      DEALLOCATE(ZSLOPE)     
    ELSE
      USS%XSSO_SLOPE=0.
    ENDIF
      
  ELSE
#ifdef SFX_ASC
    CFILEIN     = ADJUSTL(ADJUSTR(YZS)//'.txt')
#endif
#ifdef SFX_FA
    CFILEIN_FA  = ADJUSTL(ADJUSTR(YZS)//'.fa')
#endif
#ifdef SFX_LFI
    CFILEIN_LFI = ADJUSTL(YZS)
#endif
    CALL INIT_IO_SURF_n(DTCO, U, YFILETYPE,'FULL  ','SURF  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(YFILETYPE,'ZS',U%XZS(:),IRESP) 
  CALL READ_SSO_n(U%NSIZE_FULL, U%XSEA, USS, YFILETYPE)
!
  CALL END_IO_SURF_n(YFILETYPE)
!
  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN
!

ELSE
  !
  ALLOCATE(NSIZE_ALL(U%NDIM_FULL,1))
  ALLOCATE(XEXT_ALL (U%NDIM_FULL,2))
  ALLOCATE(XALL     (U%NDIM_FULL,2,1))  
  NSIZE_ALL(:,1) = 0.
  XEXT_ALL (:,1) = -99999.
  XEXT_ALL (:,2) = 99999.
  XALL   (:,:,1) = 0.  
  !
  !-------------------------------------------------------------------------------
!
!*    6.      Averages the field
!             ------------------
!
  CALL TREAT_FIELD(UG, U, USS, &
                  HPROGRAM,'SURF  ',YFILETYPE,'A_OROG',YZS, 'ZS                  ' )  
!

  DEALLOCATE(XSUMVAL )
  !
ENDIF  
!
IF (.NOT.ALLOCATED(NSIZE)) THEN
  ALLOCATE(NSIZE(NL,1))
  NSIZE(:,1) = 0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    7.      Coherence with land sea mask
!             ----------------------------
!
WHERE (U%XSEA(:)==1. .AND. NSIZE(:,1)==0) NSIZE(:,1) = -1
!
!-------------------------------------------------------------------------------
!
!*    8.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
! note that if no orography data exists near points that need to be defined,
! these points are probably small isolated islands, and a default value of 1m is assumed.
!
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,NSIZE(:,1),USS%XAVG_ZS,   'average orography',PDEF=1.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,NSIZE(:,1),USS%XSIL_ZS,   'silhouette orography',PDEF=1.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,NSIZE(:,1),USS%XMIN_ZS,   'minimum orography',PDEF=1.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,NSIZE(:,1),USS%XMAX_ZS,   'maximum orography',PDEF=1.)
!
IFLAG(:) = NSIZE(:,1)
WHERE (NSIZE(:,1)==1) IFLAG(:) = 0 ! only 1 data point was not enough for standard deviation
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XSSO_STDEV,'standard deviation of orography',PDEF=0.)
!
!-------------------------------------------------------------------------------
!
!*    9.      Coherence with land sea mask
!             ----------------------------
!
USS%XAVG_ZS   (:) = USS%XAVG_ZS   (:) * (1. - U%XSEA(:))
USS%XSIL_ZS   (:) = USS%XSIL_ZS   (:) * (1. - U%XSEA(:))
!
WHERE (U%XSEA(:)==1.)
  USS%XSSO_STDEV(:) = XUNDEF
END WHERE
!
WHERE (U%XWATER(:)==1.)
  USS%XSSO_STDEV(:) = 0.
END WHERE
!
WHERE(U%XSEA(:)>0.)
  USS%XMIN_ZS(:) = 0.
END WHERE
!
WHERE(U%XSEA(:)==1.)
  USS%XMAX_ZS(:) = 0.
END WHERE
!
!* slightly modifies the orography values when there are by coincidence equal to
!  default value.
!
WHERE (USS%XAVG_ZS==XUNDEF) USS%XAVG_ZS = USS%XAVG_ZS + ZEPS
WHERE (USS%XSIL_ZS==XUNDEF) USS%XSIL_ZS = USS%XSIL_ZS + ZEPS
WHERE (USS%XMIN_ZS==XUNDEF) USS%XMIN_ZS = USS%XMIN_ZS + ZEPS
WHERE (USS%XMAX_ZS==XUNDEF) USS%XMAX_ZS = USS%XMAX_ZS + ZEPS
!
!-------------------------------------------------------------------------------
!
!*   10.      Choice of orography
!             -------------------
!
SELECT CASE (COROGTYPE)
  CASE ('AVG')
    U%XZS(:) = USS%XAVG_ZS(:)
  CASE ('ENV')
    U%XZS(:) = USS%XAVG_ZS(:)
    WHERE (U%XSEA(:)<1.) U%XZS(:) = USS%XAVG_ZS(:) + XENV * USS%XSSO_STDEV
  CASE ('SIL')
    U%XZS(:) = USS%XSIL_ZS(:)
  CASE ('MAX')
    U%XZS(:) = USS%XMAX_ZS(:)
  CASE DEFAULT
    CALL ABOR1_SFX('PGD_OROGRAPHY: OROGRAPHY TYPE NOT SUPPORTED '//COROGTYPE)
END SELECT
!
!-------------------------------------------------------------------------------
!
!*   12.      Subgrid scale orography characteristics
!             ---------------------------------------
!
 CALL SSO(U, UG, USS, GSSO, GSSO_ANIS)
!
IFLAG(:) = NSIZE(:,1)
WHERE(.NOT. GSSO(:))                 IFLAG(:) = 0
WHERE(U%XSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
!
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XSSO_DIR,  'subgrid orography direction',PDEF=0.)
!
IF (LEXPLICIT_SLOPE) THEN
  CALL EXPLICIT_SLOPE(UG, U%NDIM_FULL, U%XZS, USS%XSSO_SLOPE) 
ELSEIF (LEN_TRIM(YSLOPE)==0) THEN
  CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XSSO_SLOPE,'subgrid orography slope',PDEF=0.)  
END IF
!
IFLAG(:) = NSIZE(:,1)
WHERE(.NOT. GSSO_ANIS(:))            IFLAG(:) = 0
WHERE(U%XSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
!
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XSSO_ANIS, 'subgrid orography anisotropy',PDEF=0.)
!
WHERE (U%XSEA(:)==1.)
  USS%XSSO_ANIS (:) = XUNDEF
  USS%XSSO_DIR  (:) = XUNDEF
  USS%XSSO_SLOPE(:) = XUNDEF
END WHERE
!
WHERE (U%XWATER(:)==1.)
  USS%XSSO_ANIS (:) = 1.
  USS%XSSO_DIR  (:) = 0.
  USS%XSSO_SLOPE(:) = 0.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*   13.      Subgrid scale orography roughness
!             ---------------------------------
!
 CALL SUBSCALE_AOS(U, UG, USS, GZ0EFFI, GZ0EFFJ)
!
IFLAG(:) = NSIZE(:,1)
WHERE(.NOT. GZ0EFFI(:))              IFLAG(:) = 0
WHERE(U%XSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XAOSIP, 'subgrid orography A/S, direction i+',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XAOSIM, 'subgrid orography A/S, direction i-',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XHO2IP, 'subgrid orography h/2, direction i+',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XHO2IM, 'subgrid orography h/2, direction i-',PDEF=0.)
!
IFLAG(:) = NSIZE(:,1)
WHERE(.NOT. GZ0EFFJ(:))              IFLAG(:) = 0
WHERE(U%XSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XAOSJP, 'subgrid orography A/S, direction j+',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XAOSJM, 'subgrid orography A/S, direction j-',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XHO2JP, 'subgrid orography h/2, direction j+',PDEF=0.)
 CALL INTERPOL_FIELD(UG, U, &
                     HPROGRAM,ILUOUT,IFLAG,USS%XHO2JM, 'subgrid orography h/2, direction j-',PDEF=0.)
!
WHERE (U%XSEA(:)==1.)
  USS%XHO2IP(:) = XUNDEF
  USS%XHO2IM(:) = XUNDEF
  USS%XHO2JP(:) = XUNDEF
  USS%XHO2JM(:) = XUNDEF
  USS%XAOSIP(:) = XUNDEF
  USS%XAOSIM(:) = XUNDEF
  USS%XAOSJP(:) = XUNDEF
  USS%XAOSJM(:) = XUNDEF
END WHERE
!
WHERE (U%XWATER(:)==1.)
  USS%XHO2IP(:) = 0.
  USS%XHO2IM(:) = 0.
  USS%XHO2JP(:) = 0.
  USS%XHO2JM(:) = 0.
  USS%XAOSIP(:) = 0.
  USS%XAOSIM(:) = 0.
  USS%XAOSJP(:) = 0.
  USS%XAOSJM(:) = 0.
END WHERE
!-------------------------------------------------------------------------------
DEALLOCATE(NSIZE    )
IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_OROGRAPHY
