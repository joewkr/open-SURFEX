!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE BUILD_EMISSTAB_n (PCONVERSION, HPROGRAM,KCH,HEMIS_GR_NAME, KNBTIMES,&
              KEMIS_GR_TIME,KOFFNDX,TPEMISS,KSIZE,KLUOUT, KVERB,PRHODREF)  
!!    #####################################################################
!!
!!*** *BUILD_EMISSTAB*
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    AUTHOR
!!    ------
!!    D. Gazen
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 01/02/00
!!    C. Mari  30/10/00  call of MODD_TYPE_EFUTIL and MODD_CST
!!    D.Gazen  01/12/03  change emissions handling for surf. externalization!!
!!    P.Tulet  01/01/04  change conversion for externalization (flux unit is
!!                        molec./m2/s)
!!    M.Leriche  04/14   apply conversion factor if lead = f
!!                       change emissions name EMIS_ -> E_ name for coherence with PGD
!!    M.Moge    01/2016  using READ_SURF_FIELD2D for 2D surfex fields reads
!!
!!    EXTERNAL
!!    --------
!
USE MODI_CH_OPEN_INPUTB
USE MODI_READ_SURF_FIELD2D
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_TYPE_EFUTIL, ONLY : EMISSVAR_T
USE MODD_CSTS,        ONLY : NDAYSEC, XMD, XAVOGADRO
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1  declaration of arguments
!
!
!
REAL, DIMENSION(:), POINTER :: PCONVERSION
!
 CHARACTER(LEN=6),                INTENT(IN) :: HPROGRAM   ! Program name
INTEGER,                         INTENT(IN) :: KCH
 CHARACTER(LEN=*),DIMENSION(:),   INTENT(IN) :: HEMIS_GR_NAME ! Offline species name
INTEGER, DIMENSION(:),           INTENT(IN) :: KNBTIMES ! nb of emis times array
INTEGER, DIMENSION(:),           INTENT(IN) :: KEMIS_GR_TIME
INTEGER, DIMENSION(:),           INTENT(IN) :: KOFFNDX ! index of offline species
TYPE(EMISSVAR_T),DIMENSION(:),   INTENT(OUT):: TPEMISS ! emission struct array to fill
INTEGER,                         INTENT(IN) :: KSIZE   ! size X*Y (1D) of physical domain
INTEGER,                         INTENT(IN) :: KLUOUT  ! output listing channel
INTEGER,                         INTENT(IN) :: KVERB   ! verbose level
REAL, DIMENSION(:),              INTENT(IN) :: PRHODREF ! dry density for ref. state
!
!
!*       0.2  declaration of local variables
!
 CHARACTER(LEN=3):: YUNIT       ! unit of the flux
INTEGER         :: INBTS       ! Number of emis times for a species
INTEGER         :: IRESP       ! I/O return value
INTEGER         :: IIND1, IIND2
INTEGER         :: JSPEC       ! loop index
INTEGER         :: ITIME       ! loop index
INTEGER         :: IWS_DEFAULT ! Default Memory window size for emission reading
 CHARACTER (LEN=16):: YRECFM    ! LFI article name
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!------------------------------------------------------------------------------
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!

IF (LHOOK) CALL DR_HOOK('BUILD_EMISSTAB_N',0,ZHOOK_HANDLE)
IF (KVERB >= 5) THEN
  WRITE(KLUOUT,*) '********     SUBROUTINE (CHIMIE): BUILD_EMISSTAB_n     ********'
END IF
!
!*       1.   READ DATA 
!        --------------
!
 CALL CH_OPEN_INPUTB("EMISUNIT", KCH, KLUOUT)
!
! read unit identifier
READ(KCH,'(A3)') YUNIT
!
!*       2.   MAP DATA ONTO PROGNOSTIC VARIABLES
!        ---------------------------------------
!
ALLOCATE (PCONVERSION(SIZE(PRHODREF,1)))
! determine the conversion factor
  PCONVERSION(:) = 1.
SELECT CASE (YUNIT)
CASE ('MIX') ! flux given ppp*m/s,  conversion to molec/m2/s
! where 1 molecule/cm2/s = (224.14/6.022136E23) ppp*m/s
  PCONVERSION(:) = XAVOGADRO * PRHODREF(:) / XMD
CASE ('CON') ! flux given in molecules/cm2/s, conversion to molec/m2/s 
  PCONVERSION(:) =  1E4
CASE ('MOL') ! flux given in microMol/m2/day, conversion to molec/m2/s  
! where 1 microMol/m2/day = (22.414/86.400)*1E-12 ppp*m/s
  !XCONVERSION(:) = (22.414/86.400)*1E-12 * XAVOGADRO * PRHODREF(:) / XMD
  PCONVERSION(:) = 1E-6 * XAVOGADRO / 86400.

CASE DEFAULT
  CALL ABOR1_SFX('CH_BUILDEMISSN: UNKNOWN CONVERSION FACTOR')
END SELECT
!
! Read Window size default value >= 2
IWS_DEFAULT = 5 ! Should be set by namelist
IF (IWS_DEFAULT < 2) IWS_DEFAULT = 2
!
IIND1 = 0
IIND2 = 0
DO JSPEC=1,SIZE(TPEMISS) ! loop on offline emission species
!
  INBTS = KNBTIMES(JSPEC)
!
! Fill %CNAME
  TPEMISS(JSPEC)%CNAME = HEMIS_GR_NAME(KOFFNDX(JSPEC))
! Allocate and Fill %NETIMES 
  ALLOCATE(TPEMISS(JSPEC)%NETIMES(INBTS))
  IIND1 = IIND2+1
  IIND2 = IIND2+INBTS
  TPEMISS(JSPEC)%NETIMES(:) = KEMIS_GR_TIME(IIND1:IIND2)
! 
! Update %NWS, %NDX, %NTX, %LREAD, %XEMISDATA
  IF (INBTS <= IWS_DEFAULT) THEN
! Number of times smaller than read window size allowed
! Read emis data once and for all
    TPEMISS(JSPEC)%NWS = INBTS
    TPEMISS(JSPEC)%NDX = 1
    TPEMISS(JSPEC)%NTX = 1
    TPEMISS(JSPEC)%LREAD = .FALSE. ! to prevent future reading
    ALLOCATE(TPEMISS(JSPEC)%XEMISDATA(KSIZE,INBTS))
! Read file for emission data
    YRECFM='E_'//TRIM(TPEMISS(JSPEC)%CNAME)
    CALL READ_SURF_FIELD2D(HPROGRAM,TPEMISS(JSPEC)%XEMISDATA(:,:),YRECFM)
!
! Correction : Replace 999. with 0. value in the Emission FLUX
! and apply conversion
    WHERE(TPEMISS(JSPEC)%XEMISDATA(:,:) == 999.)
      TPEMISS(JSPEC)%XEMISDATA(:,:) = 0. 
    END WHERE
    WHERE(TPEMISS(JSPEC)%XEMISDATA(:,:) == 1.E20)
      TPEMISS(JSPEC)%XEMISDATA(:,:) = 0. 
    END WHERE
      DO ITIME=1,INBTS
      ! XCONVERSION HAS BEEN ALREADY APPLY IN CH_EMISSION_FLUXN ONLY FOR LREAD = T
      TPEMISS(JSPEC)%XEMISDATA(:,ITIME) = TPEMISS(JSPEC)%XEMISDATA(:,ITIME) * PCONVERSION(:)
      !TPEMISS(JSPEC)%XEMISDATA(:,ITIME) = TPEMISS(JSPEC)%XEMISDATA(:,ITIME)
      END DO
    ELSE
! Read window size is smaller than number of emission times
    TPEMISS(JSPEC)%NWS = IWS_DEFAULT
    TPEMISS(JSPEC)%NDX = IWS_DEFAULT
    TPEMISS(JSPEC)%NTX = 0
    TPEMISS(JSPEC)%LREAD = .TRUE.
    ALLOCATE(TPEMISS(JSPEC)%XEMISDATA(KSIZE,IWS_DEFAULT))
  END IF
 
  IF (INBTS == 1) THEN
    TPEMISS(JSPEC)%XFWORK=>TPEMISS(JSPEC)%XEMISDATA(:,1)
  ELSE
    ALLOCATE(TPEMISS(JSPEC)%XFWORK(KSIZE))
  END IF
! Compute index for periodic case
  TPEMISS(JSPEC)%NPX = MAXVAL(MINLOC(TPEMISS(JSPEC)%NETIMES(:)+&
         (1+(TPEMISS(JSPEC)%NETIMES(INBTS)-&
         TPEMISS(JSPEC)%NETIMES(:))/NDAYSEC)*NDAYSEC))  
!
! Some di###ay
  IF (KVERB >= 6) THEN
    WRITE(KLUOUT,*) '====== Species ',TRIM(TPEMISS(JSPEC)%CNAME), ' ======'
    WRITE(KLUOUT,*) '  Emission Times :' ,TPEMISS(JSPEC)%NETIMES
    WRITE(KLUOUT,*) '  Current time index :' ,TPEMISS(JSPEC)%NTX
    WRITE(KLUOUT,*) '  Current data index :' ,TPEMISS(JSPEC)%NDX
    WRITE(KLUOUT,*) '  Periodic index = ',TPEMISS(JSPEC)%NPX,&
            ' at time :',TPEMISS(JSPEC)%NETIMES(TPEMISS(JSPEC)%NPX)  
    WRITE(KLUOUT,*) '  Read window size :', TPEMISS(JSPEC)%NWS
    IF (TPEMISS(JSPEC)%LREAD) THEN
      WRITE(KLUOUT,*) ' -> Data must be read during simulation.'
    ELSE
      WRITE(KLUOUT,*) ' -> Data already in memory.'
    END IF
  END IF
END DO

IF (KVERB >= 5) THEN
  WRITE(KLUOUT,*) '******** END SUBROUTINE (CHIMIE) : BUILD_EMISSTAB_n     ********'
END IF
IF (LHOOK) CALL DR_HOOK('BUILD_EMISSTAB_N',1,ZHOOK_HANDLE)

END SUBROUTINE BUILD_EMISSTAB_n
