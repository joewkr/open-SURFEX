!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SEAICE_n (G, S, HPROGRAM,KLU,KLUOUT)
!     #########################################
!
!!****  *READ_SEAICE_n* - read seaice scheme variables
!!
!!
!!    PURPOSE : feed seaice scheme state variable and 'domain' structure
!!    -------
!!
!!**  METHOD : 
!!    -------
!!      For now, only Gelato model is handled
!!
!!      for state variable : quite standard in Surfex : use READ_SURF with 
!!         relevant field names (same names as in genuine gelato restarts)
!!      for domain information : copy from MODD_SEAFLUX_GRID
!!      for bathymetry : copy from MODD_SEAFLUX
!!
!!    EXTERNALS : READ_SURF, GLTOOLS_ALLOC, GET_TYPE_DIM, ABOR1_SFX
!!    --------
!!
!!    IMPLICIT ARGUMENTS : Gelato state variable, and a few namelist parameters
!!    ------------------
!!
!!    REFERENCE : routine restartr in original Gelato sources (V6.0.20)
!!    ---------
!!
!!    AUTHOR : S. Sénési   *Meteo France*	
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS, ONLY           : XPI, XTTSI, XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
USE MODD_WATER_PAR,      ONLY : XALBSEAICE
!
USE MODD_TYPES_GLT,   ONLY : T_GLT
USE MODD_GLT_PARAM, ONLY : nl, nt, nx, ny, nxglo, nyglo, xdomsrf, &
                           xdomsrf_g, nprinto, CFSIDMP, CHSIDMP,  &
                           XFSIDMPEFT, XHSIDMPEFT, ntd
USE MODD_GLT_CONST_THM, ONLY : epsil1
USE LIB_MPP,            ONLY : MPP_SUM
USE MODI_GLT_SNDATMF
USE MODI_GLTOOLS_ALLOC
USE MODI_GLTOOLS_READNAM
!
USE MODI_READ_SURF
USE MODI_INTERPOL_SST_MTH
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KLU      ! number of sea patch point
INTEGER,           INTENT(IN)  :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after reading
!
INTEGER           :: JMTH, INMTH
CHARACTER(LEN=2 ) :: YMTH
CHARACTER(LEN=5)  :: YLVL
!
CHARACTER(LEN=12) :: YCATEG         ! category to read
CHARACTER(LEN=12) :: YLEVEL         ! Level to read
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=200) :: YMESS         ! Error Message
!
INTEGER :: JX,JK,JL                 ! loop counter on ice categories and layers and grid points
INTEGER :: inl_in_file,int_in_file  ! file values for ice catgories and layers numbers
REAL :: ZFSIT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',0,ZHOOK_HANDLE)
!
IF (.NOT.S%LHANDLE_SIC) THEN 
   ALLOCATE(S%XSIC(0))
   IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
nx=KLU
!
ALLOCATE(S%XSIC(KLU))
S%XSIC(:)=XUNDEF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Dealing with external sea-ice cover data (either for nudging or forcing)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
IF(S%LINTERPOL_SIC)THEN
   !
   ALLOCATE(S%XFSIC(KLU))
   !
   !Precedent, Current, Next, and Second-next Monthly SIC
   INMTH=4   
   !
   ALLOCATE(S%XSIC_MTH(KLU,INMTH))
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIC_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL READ_SURF(HPROGRAM,YRECFM,S%XSIC_MTH(:,JMTH),IRESP)
      CALL CHECK_SEAICE(YRECFM,S%XSIC_MTH(:,JMTH))
   ENDDO
   !
   CALL INTERPOL_SST_MTH(S,'C')
   !
   IF (ANY(S%XFSIC(:)>1.0).OR.ANY(S%XFSIC(:)<0.0)) THEN
     CALL ABOR1_SFX('READ_SEAICE_n: FSIC should be >=0 and <=1') 
   ENDIF                 
   !
ELSE
   ! 
   ALLOCATE(S%XFSIC(0))
   ALLOCATE(S%XSIC_MTH(0,0))
   !
ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                             Worrying about a seaice scheme 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
CALL READ_SURF(HPROGRAM,'SEAICE_SCHEM',S%CSEAICE_SCHEME,IRESP)
!
IF (TRIM(S%CSEAICE_SCHEME) == 'NONE' ) THEN
   IF (S%LINTERPOL_SIC ) THEN
      S%XTICE=S%XSST
      S%XSIC=S%XFSIC
      S%XICE_ALB=XALBSEAICE           
      IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
      RETURN
   ELSE
      CALL ABOR1_SFX("READ_SEAICE_n: MUST HAVE CINTERPOL_SIC /= NONE WITH CSEAICE_SCHEME == NONE ") 
   ENDIF
ELSE 
   IF (TRIM(S%CSEAICE_SCHEME) /= 'GELATO') THEN 
      WRITE(KLUOUT,*)'READ_SEAICE_n:CSEAICE_SCHEME read in PREP, ',S%CSEAICE_SCHEME,', is not yet handled'
      CALL ABOR1_SFX("READ_SEAICE_n:CAN ONLY HANDLE GELATO SEAICE MODEL YET (and not the one quoted in PREP)") 
   ENDIF
ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                             Start of Gelato specifics
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
IF(LCPL_SEAICE)THEN       
   CALL ABOR1_SFX('READ_SEAICEN: CANNOT YET MANAGE BOTH TRUE LCPL_SEAICE AND CSEAICE_SCHEME = GELATO')
ENDIF
nxglo=nx
#if ! defined in_arpege
CALL mpp_sum(nxglo) ! Should also sum up over NPROMA blocks, in Arpege; but not that easy....
#else
IF (NPRINTO > 0) THEN
   WRITE(KLUOUT,*)'Gelato cannot yet compute global averages when running in Arpege (because of collective comm vs. NPROMA blocks)'
ENDIF
nxglo=max(nxglo,1)
#endif
!
! Use convention XSIC_EFOLDING_TIME=0 for avoiding any relaxation 
! toward SIC observation and impose it.
!
IF(S%LINTERPOL_SIC)THEN
  IF (S%XSIC_EFOLDING_TIME==0.0) THEN 
     CFSIDMP='PRESCRIBE'
  ELSE
     CFSIDMP='DAMP'
     XFSIDMPEFT=S%XSIC_EFOLDING_TIME 
  ENDIF
ENDIF
!
IF(S%LINTERPOL_SIT)THEN
  IF (S%XSIT_EFOLDING_TIME==0.0) THEN 
     CHSIDMP='PRESCRIBE'
  ELSE
     CHSIDMP='DAMP_FAC'
     XHSIDMPEFT= S%XSIT_EFOLDING_TIME 
  ENDIF
ENDIF
!
!* Physical dimensions are set for Gelato , as a 1D field (second dimension is degenerated)
!
! Supersedes Gelato hard defaults with a Gelato genuine namelist 
! if available (for Gelato wizzards !)
CALL GLTOOLS_READNAM(.FALSE.,KLUOUT)  
!
ny=1
nyglo=1
CALL GLTOOLS_ALLOC(S%TGLT)
!
!*       0.     Check dimensions : number of layers and ice categories
!
CALL READ_SURF(HPROGRAM,'ICENL',inl_in_file,IRESP)
IF (inl_in_file /= nl) THEN 
   WRITE(YMESS,'("Mismatch in # of seaice layers : prep=",I2," nml=",I2)') inl_in_file, nl
   CALL ABOR1_SFX(YMESS)
END IF
CALL READ_SURF(HPROGRAM,'ICENT',int_in_file,IRESP)
IF (int_in_file /= nt) THEN
   WRITE(YMESS,'("Mismatch in # of seaice categories : prep=",I2," nml=",I2)') int_in_file, nt
   CALL ABOR1_SFX(YMESS)
END IF
!
!*       1.     (Semi-)prognostic fields with only space dimension(s) :
!
CALL READ_SURF(HPROGRAM,'ICEUSTAR',S%TGLT%ust(:,1),IRESP)
!
!*       2.     Prognostic fields with space and ice-category dimension(s) :
!
DO JK=1,nt
   WRITE(YLVL,'(I2)') JK
   YCATEG='_'//ADJUSTL(YLVL)
   ! .. Read sea ice age for type JK
   CALL READ_SURF(HPROGRAM,'ICEAGE'//YCATEG,S%TGLT%sit(JK,:,1)%age,IRESP)
   ! .. Read melt pond volume for type JK
   CALL READ_SURF(HPROGRAM,'ICEVMP'//YCATEG,S%TGLT%sit(JK,:,1)%vmp,IRESP)
   ! .. Read sea ice surface albedo for type JK
   CALL READ_SURF(HPROGRAM,'ICEASN'//YCATEG,S%TGLT%sit(JK,:,1)%asn,IRESP)
   ! .. Read sea ice fraction for type JK
   CALL READ_SURF(HPROGRAM,'ICEFSI'//YCATEG, S%TGLT%sit(JK,:,1)%fsi,IRESP)
   ! .. Read sea ice thickness for type JK
   CALL READ_SURF(HPROGRAM,'ICEHSI'//YCATEG, S%TGLT%sit(JK,:,1)%hsi,IRESP)
   ! .. Read sea ice salinity for type JK
   CALL READ_SURF(HPROGRAM,'ICESSI'//YCATEG, S%TGLT%sit(JK,:,1)%ssi,IRESP)
   ! .. Read sea ice surface temperature for type JK
   CALL READ_SURF(HPROGRAM,'ICETSF'//YCATEG, S%TGLT%sit(JK,:,1)%tsf,IRESP)
   ! .. Read snow thickness for type JK
   CALL READ_SURF(HPROGRAM,'ICEHSN'//YCATEG, S%TGLT%sit(JK,:,1)%hsn,IRESP)
   ! .. Read snow density for type JK
   CALL READ_SURF(HPROGRAM,'ICERSN'//YCATEG, S%TGLT%sit(JK,:,1)%rsn,IRESP)
   !
   !*       3.     Prognostic fields with space, ice-category and layer dimensions :
   !
   DO JL=1,nl
      WRITE(YLVL,'(I2)') JL
      YLEVEL=YCATEG(1:LEN_TRIM(YCATEG))//'_'//ADJUSTL(YLVL)   
      ! .. Read sea ice vertical gltools_enthalpy profile for type JK and level JL  
      CALL READ_SURF(HPROGRAM,'ICEH'//YLEVEL, S%TGLT%sil(JL,JK,:,1)%ent,IRESP)
   END DO
END DO
!
!    4.  Compute ice class existence boolean from ice fractions:
!
WHERE ( S%TGLT%sit(:,:,1)%fsi<epsil1 )
   S%TGLT%sit(:,:,1)%esi = .FALSE. 
ELSEWHERE 
   S%TGLT%sit(:,:,1)%esi = .TRUE. 
ENDWHERE
!
!    4.1 Run original Gelato checks on values read in restart
!
! .. Detect negative ice concentrations
!
DO JX=1,nx
   DO JL=1,nt 
      IF ( S%TGLT%sit(JL,JX,1)%fsi<0. ) THEN
         WRITE(KLUOUT,*)  &
              '**** WARNING **** Correcting problem in ice conc. < 0 at i=',  &
              1,' j=',JX,' k=',JL
         S%TGLT%sit(JL,JX,1)%fsi = 0.
      ENDIF
   END DO
   !
   zfsit = SUM( S%TGLT%sit(:,JX,1)%fsi )
   !
   ! .. Detect total concentrations that exceed unity
   !
   IF ( zfsit>1. ) THEN
      WRITE(KLUOUT,*)  &
           '**** WARNING **** Correcting problem in total ice conc. >1 at i=',  &
           1,' j=',JX,' fsi=',zfsit
      S%TGLT%sit(:,JX,1)%fsi = S%TGLT%sit(:,JX,1)%fsi / zfsit
   ENDIF
   !
   ! .. Detect non zero concentrations but zero thickness (no consequence) 
   !
   WHERE( S%TGLT%sit(:,JX,1)%fsi>epsil1 .AND. S%TGLT%sit(:,JX,1)%hsi<epsil1)
      S%TGLT%sit(:,JX,1)%fsi=0.
      S%TGLT%sit(:,JX,1)%hsi=0.
      S%TGLT%sit(:,JX,1)%hsn=0.
   ENDWHERE
   !
END DO

!    5. Initalize Gelato domain parameters
!
!    All points of Surfex 1D grid in seaflux are sea points
!
S%TGLT%dom(:,1)%tmk=1
S%TGLT%dom(:,1)%imk=1
!
!    Masks for U- and V- grid point are not used
!
S%TGLT%dom(:,1)%umk=1
S%TGLT%dom(:,1)%vmk=1
!
!    lat,lon,srf are inherited from seaflux grid
!
S%TGLT%dom(:,1)%lon=G%XLON(:)*XPI/180.
S%TGLT%dom(:,1)%lat=G%XLAT(:)*XPI/180.
!
!    Except in Gelato dynamics, mesh lengths are used only to compute mesh area
!    Hence, a simple setting can be used
!
S%TGLT%dom(:,1)%dxc=G%XMESH_SIZE(:)**0.5
S%TGLT%dom(:,1)%dyc=S%TGLT%dom(:,1)%dxc
S%TGLT%dom(:,1)%srf=G%XMESH_SIZE(:)
!
!    Surface of local and global ocean domain (ghost points are masked out)
!
xdomsrf = SUM( S%TGLT%dom(:,1)%srf, MASK=(S%TGLT%dom(:,1)%tmk==1) )
xdomsrf_g = xdomsrf
#if ! defined in_arpege
CALL mpp_sum(xdomsrf_g) 
#else
! Avoid zero divide in Gelato computation of global area averages
xdomsrf_g = MAX(xdomsrf_g, 1.e-9)
#endif
!
!    7. Initalize Gelato time parameters
!
S%TGLT%ind%beg=1
!
!   Dummy high value for end time. Implies only that Gelato won't output 
!   its own format of run-long averaged diagnostics (which are useless 
!   in Surfex diags logic) 
!
S%TGLT%ind%end=50000000 
!
!   8. Initalize Gelato bathymetry - change sign w.r.t Surfex
!
S%TGLT%bat(:,1)=-S%XSEABATHY
!
!
!* Sea ice thickness nudging data
!
IF(S%LINTERPOL_SIT)THEN
   !
   ALLOCATE(S%XFSIT(KLU))
   !
   !Precedent, Current, Next, and Second-next Monthly SIT
   INMTH=4   
   !
   ALLOCATE(S%XSIT_MTH(KLU,INMTH))
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIT_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL READ_SURF(HPROGRAM,YRECFM,S%XSIT_MTH(:,JMTH),IRESP)
      CALL CHECK_SEAICE(YRECFM,S%XSIT_MTH(:,JMTH))
   ENDDO
   !
   CALL INTERPOL_SST_MTH(S,'H')
   !
ELSE
   ! 
   ALLOCATE(S%XFSIT(0))
   ALLOCATE(S%XSIT_MTH(0,0))
   !
ENDIF
!
!! Initialize the coupling variables with 'snapshot' prognostic variables
! (for now, averaged over ice categories)  
!
CALL GLT_SNDATMF( S%TGLT, XTTSI - XTT )
S%XSIC(:)     = S%TGLT%ice_atm(1,:,1)%fsi 
S%XTICE(:)    = S%TGLT%ice_atm(1,:,1)%tsf 
S%XICE_ALB(:) = S%TGLT%ice_atm(1,:,1)%alb 
!
! Must init ocean mixed layer temp with sensible value for getting correct diag for time step=0
S%TGLT%oce_all(:,1)%tml=S%XSST(:)
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_SEAICE(HFIELD,PFIELD)
!
!
IMPLICIT NONE
!
CHARACTER(LEN=12),  INTENT(IN) :: HFIELD
REAL, DIMENSION(:), INTENT(IN) :: PFIELD
!
REAL            :: ZMAX,ZMIN
INTEGER         :: JI, IERRC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n:CHECK_SEAICE',0,ZHOOK_HANDLE)
!
ZMIN=-1.0E10
ZMAX=1.0E10
!
IERRC=0
!
DO JI=1,KLU
   IF(PFIELD(JI)>ZMAX.OR.PFIELD(JI)<ZMIN)THEN
      IERRC=IERRC+1
      WRITE(KLUOUT,*)'PROBLEM FIELD '//TRIM(HFIELD)//' =',PFIELD(JI),&
                     'NOT REALISTIC AT LOCATION (LAT/LON)',G%XLAT(JI),G%XLON(JI)
   ENDIF
ENDDO
!         
IF(IERRC>0) CALL ABOR1_SFX('READ_SEAICE_n: FIELD '//TRIM(HFIELD)//' NOT REALISTIC')
!
IF (LHOOK) CALL DR_HOOK('READ_SEAICE_n:CHECK_SEAICE',1,ZHOOK_HANDLE)

END SUBROUTINE CHECK_SEAICE
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_SEAICE_n
