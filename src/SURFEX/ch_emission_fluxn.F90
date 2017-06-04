!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_EMISSION_FLUX_n (DTCO, U, CHE, SV, CHU, HPROGRAM,PSIMTIME,PSFSV, PRHOA, PTSTEP, KNBTS_MAX)
!     ######################################################################
!!
!!***  *CH_EMISSION_FLUX_n* - 
!!
!!    PURPOSE
!!    -------
!!      Return a time-dependent emission flux based on tabulated values
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
!!    Original 08/02/00
!!    C. Mari  30/10/00 call to MODD_TYPE_EFUTIL and MODD_CST
!!    D.Gazen  01/12/03  change emissions handling for surf. externalization
!!    P.Tulet  01/01/04  change emission conversion factor
!!    P.Tulet  01/01/05  add dust, orilam
!!    M.Leriche    2015  suppress ZDEPOT
!!    M.Moge    01/2016  using READ_SURF_FIELD2D for 2D surfex fields reads
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_EMIS_FIELD_n, ONLY : CH_EMIS_FIELD_t
USE MODD_SV_n, ONLY : SV_t
USE MODD_CH_SURF_n, ONLY : CH_SURF_t
!
USE MODD_TYPE_EFUTIL,      ONLY: EMISSVAR_T, PRONOSVAR_T
USE MODD_CSTS,             ONLY: NDAYSEC
!
USE MODI_READ_SURF_FIELD2D
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
USE MODI_GET_LUOUT
!UPG*AERO1
USE MODD_CHS_AEROSOL, ONLY: LCH_AERO_FLUX
USE MODI_CH_AER_EMISSION
!UPG*AERO1
!!
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_EMIS_FIELD_t), INTENT(INOUT) :: CHE
TYPE(SV_t), INTENT(INOUT) :: SV
TYPE(CH_SURF_t), INTENT(INOUT) :: CHU
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
REAL,               INTENT(IN)  :: PSIMTIME    ! time of simulation in sec UTC
                                               ! (counting from midnight of
                                               ! the current day)
REAL,DIMENSION(:,:),  INTENT(INOUT) :: PSFSV   ! emission flux in ppp*m/s
REAL, DIMENSION(:),   INTENT(IN)    :: PRHOA     ! air density (kg/m3)
REAL,                 INTENT(IN)    :: PTSTEP    ! atmospheric time-step                 (s)
INTEGER,              INTENT(IN)    :: KNBTS_MAX !max size of TEMISS%NETIMES

!
!*       0.2  declaration of local variables
!
INTEGER       :: IVERB   ! verbosity level
INTEGER       :: KSIZE1D ! 1D size = X*Y physical domain 
INTEGER       :: JI      ! loop control
REAL          :: ZALPHA  ! interpolation weight
!
INTEGER :: INBTS       ! Number of emission times for a species
INTEGER :: ITIM1,ITIM2 ! first/last time for interpolation
INTEGER :: INDX1,INDX2 ! first/next index for data interpolation
INTEGER :: ISIMTIME, ITPERIOD
 CHARACTER (LEN=16)  :: YRECFM          ! LFI article name
TYPE(PRONOSVAR_T),POINTER :: CURPRONOS !Current pronostic variable
!
!*       0.3  declaration of saved local variables
!
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: CNAMES
REAL,DIMENSION(SIZE(PSFSV,1),KNBTS_MAX)     :: ZWORK ! temporary array for reading data
REAL,DIMENSION(SIZE(PSFSV,1),SIZE(PSFSV,2)) :: ZEMIS ! interpolated in time emission flux
REAL,DIMENSION(SIZE(PSFSV,1))               :: ZFCO  ! CO flux
INTEGER                          :: INEQ  ! number of chemical var
                                          !(=NEQ (chimie gaz) + NSV_AER (chimie aerosol)
INTEGER                          :: IWS   ! window size
INTEGER                          :: IRESP ! return code for I/O
INTEGER                          :: ILUOUT ! Outputlisting unit
LOGICAL                          :: LIOINIT ! True if I/O init done
INTEGER                          :: JW
INTEGER                          :: ITIME
LOGICAL                          :: GCO = .FALSE. ! switch if CO emission are available
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*    EXECUTABLE STATEMENTS
!     ---------------------
!
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_FLUX_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
LIOINIT = .FALSE.
IVERB   = 5
KSIZE1D = SIZE(PSFSV,1)
INEQ    = SIZE(PSFSV,2)
!
!------------------------------------------------------------------------------
!
!*    3.  INTERPOLATE SURFACE FLUXES IN TIME IF NEEDED
!     ------------------------------------------------
!
IF (CHE%XTIME_SIMUL == 0.) THEN
   CHE%XTIME_SIMUL = PSIMTIME
ELSE
   CHE%XTIME_SIMUL = CHE%XTIME_SIMUL + PTSTEP
END IF

IF (IVERB >= 5) WRITE(ILUOUT,*) '******** CH_EMISSION_FLUX  ********'
DO JI=1,SIZE(CHE%TSEMISS)
! Simulation time (counting from midnight) is saved
  ISIMTIME = CHE%XTIME_SIMUL
!
  INBTS = SIZE(CHE%TSEMISS(JI)%NETIMES) ! 
  IWS   = CHE%TSEMISS(JI)%NWS           ! Window Size for I/O
  INDX1 = CHE%TSEMISS(JI)%NDX           ! Current data index
!
  IF (INBTS == 1) THEN
!   Time Constant Flux
!   XFWORK already points on data (see ch_buildemiss.f90)
    IF (IVERB >= 6) THEN
      WRITE(ILUOUT,*) 'NO interpolation for ',TRIM(CHE%TSEMISS(JI)%CNAME)
      IF (IVERB >= 10 ) WRITE(ILUOUT,*) CHE%TSEMISS(JI)%XFWORK
    END IF
  ELSE
    IF (IVERB >= 6) THEN
      WRITE(ILUOUT,*) 'Interpolation (T =',ISIMTIME,') : ',CHE%TSEMISS(JI)%CNAME
    END IF
    IF (ISIMTIME < CHE%TSEMISS(JI)%NETIMES(1)) THEN
!     Tsim < T(1)=Tmin should not happen but who knows ?
      CHE%TSEMISS(JI)%NTX = 1
    ELSE
!     Check for periodicity when ISIMTIME is beyond last emission time
!     and probably correct ISIMTIME
      IF (ISIMTIME > CHE%TSEMISS(JI)%NETIMES(INBTS)) THEN 
!       Tsim > T(INBTS)=Tmax
        ITPERIOD = (1+(CHE%TSEMISS(JI)%NETIMES(INBTS)-&
                CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NPX))/NDAYSEC)*NDAYSEC  
        ISIMTIME = MODULO(ISIMTIME-CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NPX),ITPERIOD)+&
                CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NPX)  
        IF (IVERB >= 6) THEN
          WRITE(ILUOUT,*) '  ITPERIOD = ', ITPERIOD
          WRITE(ILUOUT,*) '  ISIMTIME modifie = ', ISIMTIME
        END IF
        IF (CHE%TSEMISS(JI)%NTX == INBTS .AND. ISIMTIME<CHE%TSEMISS(JI)%NETIMES(INBTS)) THEN
!         Update time index NTX 
          CHE%TSEMISS(JI)%NTX = CHE%TSEMISS(JI)%NPX
!         Increment data index NDX : NDX correction will occur later
!                                    to assure 1 <= NDX <= IWS
          INDX1 = INDX1 + 1
        END IF
      END IF
!
!     search NTX such that : ETIMES(NTX) < ISIMTIME <= ETIMES(NTX+1)
!     and make NDX follow NTX : NDX correction will occur later
!                               to assure 1 <= NDX <= IWS
      DO WHILE (CHE%TSEMISS(JI)%NTX < INBTS)
        IF (ISIMTIME >= CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NTX+1)) THEN
          CHE%TSEMISS(JI)%NTX = CHE%TSEMISS(JI)%NTX + 1
          INDX1 = INDX1 + 1
          INDX2 = INDX1 + 1
        ELSE
          EXIT
        END IF
      END DO
    END IF
!
!   Check availability of data within memory Window (XEMISDATA(:,1:IWS))
    IF (INDX1 >= IWS) THEN
!
!     Data index reached the memory window limits
!
      IF (CHE%TSEMISS(JI)%LREAD) THEN 
!
!       File must be read to update XEMISDATA array for this species 
!
        IF (.NOT. LIOINIT) THEN
!         Must be done once before reading
          CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','READ ')
          IF (IVERB >= 6) WRITE(ILUOUT,*) 'INIT des I/O DONE.'
          LIOINIT=.TRUE.
        END IF
        YRECFM='E_'//TRIM(CHE%TSEMISS(JI)%CNAME)
        IF (IVERB >= 6)&
               WRITE (ILUOUT,*) 'READ emission :',TRIM(YRECFM),&
               ', SIZE(ZWORK)=',SIZE(ZWORK,1),INBTS 
        CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK(:,1:INBTS),YRECFM)
!
! Correction : Replace 999. with 0. value in the Emission FLUX
        WHERE(ZWORK(:,1:INBTS) == 999.)
          ZWORK(:,1:INBTS) = 0. 
        END WHERE
        WHERE(ZWORK(:,1:INBTS) == 1.E20)
          ZWORK(:,1:INBTS) = 0. 
        END WHERE
        DO ITIME=1,INBTS
        ZWORK(:,ITIME) = ZWORK(:,ITIME)*CHU%XCONVERSION(:)
        END DO
!
!       
        IF ((CHE%TSEMISS(JI)%NTX+IWS-1) > INBTS) THEN
!
!         ===== Periodic CASE =====
!
          IF (IVERB >= 6)&
                 WRITE (ILUOUT,*) 'Periodic CASE : NPX =',CHE%TSEMISS(JI)%NPX  
          IF (IWS <  (INBTS-CHE%TSEMISS(JI)%NPX+1)) THEN
!           Window size is smaller then number of periodical times
!
!           example : IWS=5, NPX=2, INBTS=11, NTX=9
!                               NTX       NPX
!                                |         |
!           time index :      ...9 10 11 # 2 3 4...11 #
!       old data index :[1 2 3 4 5] 
!       new data index :        [1  2  3   4 5]
!                                |  
!                               NDX    
!
            CHE%TSEMISS(JI)%XEMISDATA(:,1:INBTS-CHE%TSEMISS(JI)%NTX+1) = &
                   ZWORK(:,CHE%TSEMISS(JI)%NTX:INBTS)  
!
            IF (IVERB >= 6) THEN
              WRITE(ILUOUT,*) 'Window SIZE smaller than INBTS !'
              WRITE(ILUOUT,*) 'Window index, Time index'
              DO JW=1,INBTS-CHE%TSEMISS(JI)%NTX+1
                WRITE(ILUOUT,*) JW,CHE%TSEMISS(JI)%NTX+JW-1
              END DO
            END IF
!
            CHE%TSEMISS(JI)%XEMISDATA(:,INBTS-CHE%TSEMISS(JI)%NTX+2:IWS) = &
                   ZWORK(:,CHE%TSEMISS(JI)%NPX:CHE%TSEMISS(JI)%NPX+IWS-INBTS+CHE%TSEMISS(JI)%NTX-2)  
!
            IF (IVERB >= 6) THEN
              DO JW=INBTS-CHE%TSEMISS(JI)%NTX+2,IWS
                WRITE(ILUOUT,*) JW,CHE%TSEMISS(JI)%NPX+JW-(INBTS-CHE%TSEMISS(JI)%NTX+2)
              END DO
            END IF
            INDX1 = 1
            INDX2 = 2
          ELSE
!           Window size may get smaller AND it will be the last reading
!
!           example : IWS=6, NPX=7, INBTS=11, NTX=9
!
!                         NTX       NPX NTX
!                          |         |   |
!           time index: ...9 10 11 # 7 8 9 10 11 #
!       old data index: ...6]
!       new data index:             [1 2 3  4  5]
!                                        |
!                                       NDX=NTX-NPX+1
!
            IWS = INBTS-CHE%TSEMISS(JI)%NPX+1
            CHE%TSEMISS(JI)%NWS = IWS
            CHE%TSEMISS(JI)%XEMISDATA(:,1:IWS) = ZWORK(:,CHE%TSEMISS(JI)%NPX:INBTS)
            IF (IVERB >= 6) THEN
              WRITE(ILUOUT,*) 'Window SIZE equal or greater than INBTS !'
              WRITE(ILUOUT,*) 'Window index, Time index'
              DO JW=1,IWS
                WRITE(ILUOUT,*) JW,CHE%TSEMISS(JI)%NPX+JW-1
              END DO
            END IF
            INDX1 = CHE%TSEMISS(JI)%NTX-CHE%TSEMISS(JI)%NPX+1
            INDX2 = MOD((INDX1+1),IWS)
            CHE%TSEMISS(JI)%LREAD = .FALSE. ! no more reading
          END IF
        ELSE
!
!         ===== NON periodic (normal) CASE =====
!
! example : with IWS=5, the window moves forward
!                             NTX
!                              | 
!         time index : 1 2 3 4 5 6 7 8 9 10 11 ... INBTS # 
!     old data index :[1 2 3 4 5] 
!     new data index :        [1 2 3 4 5] 
!                              |
!                             NDX
!
          CHE%TSEMISS(JI)%XEMISDATA(:,1:IWS) = ZWORK(:,CHE%TSEMISS(JI)%NTX:CHE%TSEMISS(JI)%NTX+IWS-1)
          IF (IVERB >= 6) THEN
            WRITE(ILUOUT,*) 'Window index, Time index'
            DO JW=1,IWS
              WRITE(ILUOUT,*) JW,CHE%TSEMISS(JI)%NTX+JW-1
            END DO
          END IF
          INDX1 = 1
          INDX2 = 2
        END IF
      ELSE
!       Data is already in memory because window size is sufficient 
!       to hold INBTS emission times => simply update NDX according to NTX
!       
        IF (IWS==INBTS) THEN 
!
!         'Window size' = 'Nb emis times' at INIT (ch_init_emission)
!         so NDX must be set equal to NTX (the window does not move)
! example :
!                         NPX    NTX
!                          |      | 
!         time index :  1  2  3  ... INBTS
!         data index : [1  2  3  ... INBTS]
!                                 |
!                                NDX

          INDX1 = CHE%TSEMISS(JI)%NTX
          INDX2 = INDX1+1
          IF (INDX2 > IWS) INDX2=CHE%TSEMISS(JI)%NPX
        ELSE
!          
!         Windows size changed during periodic case
!         NDX must be equal to NTX - NPX + 1
!         (the window does not move)
! example :
!                                NTX
!                                 | 
!         time index : NPX NPX+1 NPX+2 ... INBTS
!         data index : [1    2    3    ...   IWS]
!                                 |
!                                NDX
          INDX1 = CHE%TSEMISS(JI)%NTX-CHE%TSEMISS(JI)%NPX+1
          INDX2 = MOD((INDX1+1),IWS)
        END IF
      END IF
    ELSE ! (INDX1 < IWS)
      INDX2 = INDX1+1
    END IF
!
!   Don't forget to update NDX with new value INDX1
    CHE%TSEMISS(JI)%NDX = INDX1
!
!   Compute both times for interpolation
    IF (CHE%TSEMISS(JI)%NTX < INBTS) THEN 
      ITIM1 = CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NTX)
      ITIM2 = CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NTX+1)
    ELSE
      ITIM1 = CHE%TSEMISS(JI)%NETIMES(INBTS)
      ITIM2 = CHE%TSEMISS(JI)%NETIMES(CHE%TSEMISS(JI)%NPX)+ITPERIOD
    END IF
!
! Interpolate variables in time -> update XFWORK
!
!
!  time       :  ITIM1...Tsim...ITIM2
!                  |              |  
!  data index :  INDX1          INDX2
!
!
    ZALPHA = (REAL(ISIMTIME) - ITIM1) / (ITIM2-ITIM1)
    CHE%TSEMISS(JI)%XFWORK(:) = ZALPHA*CHE%TSEMISS(JI)%XEMISDATA(:,INDX2) +&
            (1.-ZALPHA)*CHE%TSEMISS(JI)%XEMISDATA(:,INDX1)  
    IF (IVERB >= 6) THEN
      WRITE(ILUOUT,*) '  Current time INDEX : ',CHE%TSEMISS(JI)%NTX
      WRITE(ILUOUT,*) '  TIME : ',ISIMTIME, ' (',ITIM1,',',ITIM2,')'
      WRITE(ILUOUT,*) '  Window size : ',CHE%TSEMISS(JI)%NWS
      WRITE(ILUOUT,*) '  Current data INDEX : ',INDX1,INDX2
      IF (IVERB >= 10) WRITE(ILUOUT,*) '  FLUX : ',CHE%TSEMISS(JI)%XFWORK
    END IF
  END IF
END DO
! 
! Agregation : flux computation
!
ZEMIS(:,:) = 0.
!
! Point on head of Pronostic variable list
! to cover the entire list.
IF (SV%NSV_AEREND > 0) THEN
CNAMES=>SV%CSV(SV%NSV_CHSBEG:SV%NSV_AEREND)
ELSE
CNAMES=>SV%CSV(SV%NSV_CHSBEG:SV%NSV_CHSEND)
END IF
CURPRONOS=>CHE%TSPRONOSLIST
DO WHILE(ASSOCIATED(CURPRONOS))
  IF (CURPRONOS%NAMINDEX > INEQ) THEN
    WRITE(ILUOUT,*) 'FATAL ERROR in CH_EMISSION_FLUXN : SIZE(ZEMIS,2) =',&
           INEQ,', INDEX bugge =',CURPRONOS%NAMINDEX  
    CALL ABOR1_SFX('CH_EMISSION_FLUXN: FATAL ERROR')
  END IF
  
  ZEMIS(:,CURPRONOS%NAMINDEX) = 0.
!
! Loop on the number of agreg. coeff.
  DO JI=1,CURPRONOS%NBCOEFF
!   Compute agregated flux    
    ZEMIS(:,CURPRONOS%NAMINDEX) = ZEMIS(:,CURPRONOS%NAMINDEX)+&
            CURPRONOS%XCOEFF(JI)*CHE%TSEMISS(CURPRONOS%NEFINDEX(JI))%XFWORK(:)  
  END DO

  IF (IVERB >= 6) THEN
    WRITE(ILUOUT,*) 'Agregation for ',CNAMES(CURPRONOS%NAMINDEX)
    IF (IVERB >= 10) WRITE(ILUOUT,*) 'ZEMIS = ',ZEMIS(:,CURPRONOS%NAMINDEX)
  END IF
  IF ((CNAMES(CURPRONOS%NAMINDEX) == "CO") .AND. ANY(ZEMIS(:,CURPRONOS%NAMINDEX).GT.0.)) THEN
  ZFCO(:) = ZEMIS(:,CURPRONOS%NAMINDEX)
  GCO = .TRUE.
  END IF

  CURPRONOS=>CURPRONOS%NEXT
!
END DO
!
IF ((LCH_AERO_FLUX).AND.(SV%NSV_AERBEG > 0)) THEN
  IF (GCO) THEN
    CALL CH_AER_EMISSION(ZEMIS, PRHOA, SV%CSV, SV%NSV_CHSBEG, PFCO=ZFCO)
  ELSE
    CALL CH_AER_EMISSION(ZEMIS, PRHOA, SV%CSV, SV%NSV_CHSBEG)
  ENDIF
END IF
!
PSFSV(:,:) = PSFSV(:,:) + ZEMIS(:,:)
!
IF (LIOINIT) CALL END_IO_SURF_n(HPROGRAM)
!
IF (IVERB >= 6) WRITE(ILUOUT,*) '******** END CH_EMISSION_FLUX  ********'
IF (LHOOK) CALL DR_HOOK('CH_EMISSION_FLUX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE CH_EMISSION_FLUX_n
