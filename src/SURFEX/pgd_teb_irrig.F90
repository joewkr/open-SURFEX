!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_IRRIG (DTCO, UG, U, USS, KDIM, TIR, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_IRRIG* monitor for averaging and interpolations of cover fractions
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
!!    Original    05/2013
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
!
USE MODD_SURF_PAR,          ONLY : XUNDEF
!
USE MODD_PGDWORK,           ONLY : CATYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_ABOR1_SFX
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
INTEGER, INTENT(IN) :: KDIM
TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
LOGICAL               :: GNO_PAR_GD_IRRIG ! flag if no irrigation is prescribed for gardens
LOGICAL               :: GNO_PAR_GR_IRRIG ! flag if no irrigation is prescribed for greenroofs
LOGICAL               :: GNO_PAR_RD_IRRIG ! flag if no irrigation is prescribed for roads
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!
!
! uniform value
!
REAL               :: XUNIF_GD_START_MONTH  ! start month for irrigation      for gardens
REAL               :: XUNIF_GD_END_MONTH    ! end   month for irrigation      for gardens
REAL               :: XUNIF_GD_START_HOUR   ! start solar hour for irrigation for gardens
REAL               :: XUNIF_GD_END_HOUR     ! end   solar hour for irrigation for gardens
REAL               :: XUNIF_GD_24H_IRRIG    ! total irrigation over 24h       for gardens (kg/m2)
REAL               :: XUNIF_GR_START_MONTH  ! start month for irrigation      for greenroofs
REAL               :: XUNIF_GR_END_MONTH    ! end   month for irrigation      for greenroofs
REAL               :: XUNIF_GR_START_HOUR   ! start solar hour for irrigation for greenroofs
REAL               :: XUNIF_GR_END_HOUR     ! end   solar hour for irrigation for greenroofs
REAL               :: XUNIF_GR_24H_IRRIG    ! total irrigation over 24h       for greenroofs (kg/m2)
REAL               :: XUNIF_RD_START_MONTH  ! start month for irrigation      for roads
REAL               :: XUNIF_RD_END_MONTH    ! end   month for irrigation      for roads
REAL               :: XUNIF_RD_START_HOUR   ! start solar hour for irrigation for roads
REAL               :: XUNIF_RD_END_HOUR     ! end   solar hour for irrigation for roads
REAL               :: XUNIF_RD_24H_IRRIG    ! total irrigation over 24h       for roads (kg/m2)
!
! name of files containing data
!
 CHARACTER(LEN=28)                      :: CFNAM_GD_START_MONTH
 CHARACTER(LEN=28)                      :: CFNAM_GD_END_MONTH 
 CHARACTER(LEN=28)                      :: CFNAM_GD_START_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_GD_END_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_GD_24H_IRRIG
 CHARACTER(LEN=28)                      :: CFNAM_GR_START_MONTH
 CHARACTER(LEN=28)                      :: CFNAM_GR_END_MONTH 
 CHARACTER(LEN=28)                      :: CFNAM_GR_START_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_GR_END_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_GR_24H_IRRIG
 CHARACTER(LEN=28)                      :: CFNAM_RD_START_MONTH
 CHARACTER(LEN=28)                      :: CFNAM_RD_END_MONTH 
 CHARACTER(LEN=28)                      :: CFNAM_RD_START_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_RD_END_HOUR
 CHARACTER(LEN=28)                      :: CFNAM_RD_24H_IRRIG
!
! type of files containing data
!
 CHARACTER(LEN=28)                      :: CFTYP_GD_START_MONTH
 CHARACTER(LEN=28)                      :: CFTYP_GD_END_MONTH 
 CHARACTER(LEN=28)                      :: CFTYP_GD_START_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_GD_END_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_GD_24H_IRRIG
 CHARACTER(LEN=28)                      :: CFTYP_GR_START_MONTH
 CHARACTER(LEN=28)                      :: CFTYP_GR_END_MONTH 
 CHARACTER(LEN=28)                      :: CFTYP_GR_START_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_GR_END_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_GR_24H_IRRIG
 CHARACTER(LEN=28)                      :: CFTYP_RD_START_MONTH
 CHARACTER(LEN=28)                      :: CFTYP_RD_END_MONTH 
 CHARACTER(LEN=28)                      :: CFTYP_RD_START_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_RD_END_HOUR
 CHARACTER(LEN=28)                      :: CFTYP_RD_24H_IRRIG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TEB_IRRIG /   XUNIF_GD_START_MONTH, &
                                XUNIF_GD_END_MONTH,   &
                                XUNIF_GD_START_HOUR,  &
                                XUNIF_GD_END_HOUR,    &
                                XUNIF_GD_24H_IRRIG,   &
                                XUNIF_GR_START_MONTH, &
                                XUNIF_GR_END_MONTH,   &
                                XUNIF_GR_START_HOUR,  &
                                XUNIF_GR_END_HOUR,    &
                                XUNIF_GR_24H_IRRIG,   &
                                XUNIF_RD_START_MONTH, &
                                XUNIF_RD_END_MONTH,   &
                                XUNIF_RD_START_HOUR,  &
                                XUNIF_RD_END_HOUR,    &
                                XUNIF_RD_24H_IRRIG,   &
                                CFNAM_GD_START_MONTH, &
                                CFNAM_GD_END_MONTH ,  &
                                CFNAM_GD_START_HOUR,  &
                                CFNAM_GD_END_HOUR,    &
                                CFNAM_GD_24H_IRRIG,   &
                                CFNAM_GR_START_MONTH, &
                                CFNAM_GR_END_MONTH ,  &
                                CFNAM_GR_START_HOUR,  &
                                CFNAM_GR_END_HOUR,    &
                                CFNAM_GR_24H_IRRIG,   &
                                CFNAM_RD_START_MONTH, &
                                CFNAM_RD_END_MONTH ,  &
                                CFNAM_RD_START_HOUR,  &
                                CFNAM_RD_END_HOUR,    &
                                CFNAM_RD_24H_IRRIG,   &
                                CFTYP_GD_START_MONTH, &
                                CFTYP_GD_END_MONTH,   &
                                CFTYP_GD_START_HOUR,  &
                                CFTYP_GD_END_HOUR,    &
                                CFTYP_GD_24H_IRRIG,   &
                                CFTYP_GR_START_MONTH, &
                                CFTYP_GR_END_MONTH,   &
                                CFTYP_GR_START_HOUR,  &
                                CFTYP_GR_END_HOUR,    &
                                CFTYP_GR_24H_IRRIG,   &
                                CFTYP_RD_START_MONTH, &
                                CFTYP_RD_END_MONTH,   &
                                CFTYP_RD_START_HOUR,  &
                                CFTYP_RD_END_HOUR,    &
                                CFTYP_RD_24H_IRRIG

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_IRRIG',0,ZHOOK_HANDLE)
!
!
XUNIF_GD_START_MONTH= XUNDEF
XUNIF_GD_END_MONTH  = XUNDEF
XUNIF_GD_START_HOUR = XUNDEF
XUNIF_GD_END_HOUR   = XUNDEF
XUNIF_GD_24H_IRRIG  = XUNDEF
XUNIF_GR_START_MONTH= XUNDEF
XUNIF_GR_END_MONTH  = XUNDEF
XUNIF_GR_START_HOUR = XUNDEF
XUNIF_GR_END_HOUR   = XUNDEF
XUNIF_GR_24H_IRRIG  = XUNDEF
XUNIF_RD_START_MONTH= XUNDEF
XUNIF_RD_END_MONTH  = XUNDEF
XUNIF_RD_START_HOUR = XUNDEF
XUNIF_RD_END_HOUR   = XUNDEF
XUNIF_RD_24H_IRRIG  = XUNDEF
!
CFNAM_GD_START_MONTH  = '                            '
CFNAM_GD_END_MONTH    = '                            '
CFNAM_GD_START_HOUR   = '                            '
CFNAM_GD_END_HOUR     = '                            '
CFNAM_GD_24H_IRRIG    = '                            '
CFNAM_GR_START_MONTH  = '                            '
CFNAM_GR_END_MONTH    = '                            '
CFNAM_GR_START_HOUR   = '                            '
CFNAM_GR_END_HOUR     = '                            '
CFNAM_GR_24H_IRRIG    = '                            '
CFNAM_RD_START_MONTH  = '                            '
CFNAM_RD_END_MONTH    = '                            '
CFNAM_RD_START_HOUR   = '                            '
CFNAM_RD_END_HOUR     = '                            '
CFNAM_RD_24H_IRRIG    = '                            '
!
CFTYP_GD_START_MONTH  = '      '
CFTYP_GD_END_MONTH    = '      '
CFTYP_GD_START_HOUR   = '      '
CFTYP_GD_END_HOUR     = '      '
CFTYP_GD_24H_IRRIG    = '      '
CFTYP_GR_START_MONTH  = '      '
CFTYP_GR_END_MONTH    = '      '
CFTYP_GR_START_HOUR   = '      '
CFTYP_GR_END_HOUR     = '      '
CFTYP_GR_24H_IRRIG    = '      '
CFTYP_RD_START_MONTH  = '      '
CFTYP_RD_END_MONTH    = '      '
CFTYP_RD_START_HOUR   = '      '
CFTYP_RD_END_HOUR     = '      '
CFTYP_RD_24H_IRRIG    = '      '
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB_IRRIG',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB_IRRIG)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    3.      Coherence check for gardens
!             ---------------------------
!
TIR%LPAR_GD_IRRIG =     (XUNIF_GD_START_MONTH /= XUNDEF .OR. LEN_TRIM(CFNAM_GD_START_MONTH) >0 )&
              .AND. (XUNIF_GD_END_MONTH   /= XUNDEF .OR. LEN_TRIM(CFNAM_GD_END_MONTH  ) >0 )&
              .AND. (XUNIF_GD_START_HOUR  /= XUNDEF .OR. LEN_TRIM(CFNAM_GD_START_HOUR ) >0 )&
              .AND. (XUNIF_GD_END_HOUR    /= XUNDEF .OR. LEN_TRIM(CFNAM_GD_END_HOUR   ) >0 )&
              .AND. (XUNIF_GD_24H_IRRIG   /= XUNDEF .OR. LEN_TRIM(CFNAM_GD_24h_IRRIG  ) >0 )

GNO_PAR_GD_IRRIG =  (XUNIF_GD_START_MONTH == XUNDEF .AND. LEN_TRIM(CFNAM_GD_START_MONTH)==0 )&
              .AND. (XUNIF_GD_END_MONTH   == XUNDEF .AND. LEN_TRIM(CFNAM_GD_END_MONTH  )==0 )&
              .AND. (XUNIF_GD_START_HOUR  == XUNDEF .AND. LEN_TRIM(CFNAM_GD_START_HOUR )==0 )&
              .AND. (XUNIF_GD_END_HOUR    == XUNDEF .AND. LEN_TRIM(CFNAM_GD_END_HOUR   )==0 )&
              .AND. (XUNIF_GD_24H_IRRIG   == XUNDEF .AND. LEN_TRIM(CFNAM_GD_24h_IRRIG  )==0 )

IF ( .NOT. TIR%LPAR_GD_IRRIG .AND. .NOT. GNO_PAR_GD_IRRIG) THEN
  WRITE(ILUOUT,*) ' Error for prescription of irrigation in gardens '
  WRITE(ILUOUT,*) ' You need to specify the five parameters ... or none. '
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_IRRIG: you need to specify ALL of parameters for GARDEN or NONE of them')
END IF
!
!-------------------------------------------------------------------------------
IF (TIR%LPAR_GD_IRRIG) THEN
!
ALLOCATE(TIR%XGD_START_MONTH   (KDIM        ))
ALLOCATE(TIR%XGD_END_MONTH     (KDIM        ))
ALLOCATE(TIR%XGD_START_HOUR    (KDIM        ))
ALLOCATE(TIR%XGD_END_HOUR      (KDIM        ))
ALLOCATE(TIR%XGD_24H_IRRIG     (KDIM        ))
!
!-------------------------------------------------------------------------------
!
!*    4.      Fields are prescribed for gardens
!             ---------------------------------
!
CATYPE = 'MAJ'
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GD_START_MONTH : start month for irrigation of gardens','TWN',CFNAM_GD_START_MONTH,   &
                 CFTYP_GD_START_MONTH,XUNIF_GD_START_MONTH,TIR%XGD_START_MONTH(:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GD_END_MONTH   : end   month for irrigation of gardens','TWN',CFNAM_GD_END_MONTH,     &
                 CFTYP_GD_END_MONTH  ,XUNIF_GD_END_MONTH  ,TIR%XGD_END_MONTH  (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GD_START_HOUR  : start HOUR  for irrigation of gardens','TWN',CFNAM_GD_START_HOUR ,   &
                 CFTYP_GD_START_HOUR ,XUNIF_GD_START_HOUR ,TIR%XGD_START_HOUR (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GD_END_HOUR    : end   HOUR  for irrigation of gardens','TWN',CFNAM_GD_END_HOUR ,     &
                 CFTYP_GD_END_HOUR   ,XUNIF_GD_END_HOUR   ,TIR%XGD_END_HOUR   (:))  
CATYPE = 'ARI'
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GD_24H_IRRIG   : total irrigation over 24h for gardens','TWN',CFNAM_GD_24H_IRRIG ,    &
                 CFTYP_GD_24H_IRRIG  ,XUNIF_GD_24H_IRRIG  ,TIR%XGD_24H_IRRIG  (:))  
!
!
END IF
!-------------------------------------------------------------------------------
!
!*    5.      Coherence check for greenroofs
!             ------------------------------
!
TIR%LPAR_GR_IRRIG =     (XUNIF_GR_START_MONTH /= XUNDEF .OR. LEN_TRIM(CFNAM_GR_START_MONTH) >0 )&
              .AND. (XUNIF_GR_END_MONTH   /= XUNDEF .OR. LEN_TRIM(CFNAM_GR_END_MONTH  ) >0 )&
              .AND. (XUNIF_GR_START_HOUR  /= XUNDEF .OR. LEN_TRIM(CFNAM_GR_START_HOUR ) >0 )&
              .AND. (XUNIF_GR_END_HOUR    /= XUNDEF .OR. LEN_TRIM(CFNAM_GR_END_HOUR   ) >0 )&
              .AND. (XUNIF_GR_24H_IRRIG   /= XUNDEF .OR. LEN_TRIM(CFNAM_GR_24h_IRRIG  ) >0 )

GNO_PAR_GR_IRRIG =  (XUNIF_GR_START_MONTH == XUNDEF .AND. LEN_TRIM(CFNAM_GR_START_MONTH)==0 )&
              .AND. (XUNIF_GR_END_MONTH   == XUNDEF .AND. LEN_TRIM(CFNAM_GR_END_MONTH  )==0 )&
              .AND. (XUNIF_GR_START_HOUR  == XUNDEF .AND. LEN_TRIM(CFNAM_GR_START_HOUR )==0 )&
              .AND. (XUNIF_GR_END_HOUR    == XUNDEF .AND. LEN_TRIM(CFNAM_GR_END_HOUR   )==0 )&
              .AND. (XUNIF_GR_24H_IRRIG   == XUNDEF .AND. LEN_TRIM(CFNAM_GR_24h_IRRIG  )==0 )

IF ( .NOT. TIR%LPAR_GR_IRRIG .AND. .NOT. GNO_PAR_GR_IRRIG) THEN
  WRITE(ILUOUT,*) ' Error for prescription of irrigation in greenroofs '
  WRITE(ILUOUT,*) ' You need to specify the five parameters ... or none. '
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_IRRIG: you need to specify ALL of parameters for GREENROOFS or NONE of them')
END IF
!
!-------------------------------------------------------------------------------
IF (TIR%LPAR_GR_IRRIG) THEN
!
ALLOCATE(TIR%XGR_START_MONTH   (KDIM        ))
ALLOCATE(TIR%XGR_END_MONTH     (KDIM        ))
ALLOCATE(TIR%XGR_START_HOUR    (KDIM        ))
ALLOCATE(TIR%XGR_END_HOUR      (KDIM        ))
ALLOCATE(TIR%XGR_24H_IRRIG     (KDIM        ))
!
!-------------------------------------------------------------------------------
!
!*    6.      fields are prescribed for greenroofs
!             ------------------------------------
!
CATYPE = 'MAJ'
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GR_START_MONTH : start month for irrigation of greenroofs','TWN',CFNAM_GR_START_MONTH,   &
                 CFTYP_GR_START_MONTH,XUNIF_GR_START_MONTH,TIR%XGR_START_MONTH(:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GR_END_MONTH   : end   month for irrigation of greenroofs','TWN',CFNAM_GR_END_MONTH,     &
                 CFTYP_GR_END_MONTH  ,XUNIF_GR_END_MONTH  ,TIR%XGR_END_MONTH  (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GR_START_HOUR  : start HOUR  for irrigation of greenroofs','TWN',CFNAM_GR_START_HOUR ,   &
                 CFTYP_GR_START_HOUR ,XUNIF_GR_START_HOUR ,TIR%XGR_START_HOUR (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GR_END_HOUR    : end   HOUR  for irrigation of greenroofs','TWN',CFNAM_GR_END_HOUR ,     &
                 CFTYP_GR_END_HOUR   ,XUNIF_GR_END_HOUR   ,TIR%XGR_END_HOUR   (:))  
CATYPE = 'ARI'
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'GR_24H_IRRIG   : total irrigation over 24h for greenroofs','TWN',CFNAM_GR_24H_IRRIG ,    &
                 CFTYP_GR_24H_IRRIG  ,XUNIF_GR_24H_IRRIG  ,TIR%XGR_24H_IRRIG  (:))  
!
END IF
!-------------------------------------------------------------------------------
!
!*    7.      Coherence check for roads
!             -------------------------
!
TIR%LPAR_RD_IRRIG =     (XUNIF_RD_START_MONTH /= XUNDEF .OR. LEN_TRIM(CFNAM_RD_START_MONTH) >0 )&
              .AND. (XUNIF_RD_END_MONTH   /= XUNDEF .OR. LEN_TRIM(CFNAM_RD_END_MONTH  ) >0 )&
              .AND. (XUNIF_RD_START_HOUR  /= XUNDEF .OR. LEN_TRIM(CFNAM_RD_START_HOUR ) >0 )&
              .AND. (XUNIF_RD_END_HOUR    /= XUNDEF .OR. LEN_TRIM(CFNAM_RD_END_HOUR   ) >0 )&
              .AND. (XUNIF_RD_24H_IRRIG   /= XUNDEF .OR. LEN_TRIM(CFNAM_RD_24h_IRRIG  ) >0 )

GNO_PAR_RD_IRRIG =  (XUNIF_RD_START_MONTH == XUNDEF .AND. LEN_TRIM(CFNAM_RD_START_MONTH)==0 )&
              .AND. (XUNIF_RD_END_MONTH   == XUNDEF .AND. LEN_TRIM(CFNAM_RD_END_MONTH  )==0 )&
              .AND. (XUNIF_RD_START_HOUR  == XUNDEF .AND. LEN_TRIM(CFNAM_RD_START_HOUR )==0 )&
              .AND. (XUNIF_RD_END_HOUR    == XUNDEF .AND. LEN_TRIM(CFNAM_RD_END_HOUR   )==0 )&
              .AND. (XUNIF_RD_24H_IRRIG   == XUNDEF .AND. LEN_TRIM(CFNAM_RD_24h_IRRIG  )==0 )

IF ( .NOT. TIR%LPAR_RD_IRRIG .AND. .NOT. GNO_PAR_RD_IRRIG) THEN
  WRITE(ILUOUT,*) ' Error for prescription of irrigation on roads '
  WRITE(ILUOUT,*) ' You need to specify the five parameters ... or none. '
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_IRRIG: you need to specify ALL of parameters for ROADS or NONE of them')
END IF
!
!-------------------------------------------------------------------------------
IF (TIR%LPAR_RD_IRRIG) THEN
!
ALLOCATE(TIR%XRD_START_MONTH   (KDIM        ))
ALLOCATE(TIR%XRD_END_MONTH     (KDIM        ))
ALLOCATE(TIR%XRD_START_HOUR    (KDIM        ))
ALLOCATE(TIR%XRD_END_HOUR      (KDIM        ))
ALLOCATE(TIR%XRD_24H_IRRIG     (KDIM        ))
!
!-------------------------------------------------------------------------------
!
!*    8.      fields are prescribed for roads
!             -------------------------------
!
CATYPE = 'MAJ'
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'RD_START_MONTH : start month for irrigation of roads','TWN',CFNAM_RD_START_MONTH,   &
                 CFTYP_RD_START_MONTH,XUNIF_RD_START_MONTH,TIR%XRD_START_MONTH(:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'RD_END_MONTH   : end   month for irrigation of roads','TWN',CFNAM_RD_END_MONTH,     &
                 CFTYP_RD_END_MONTH  ,XUNIF_RD_END_MONTH  ,TIR%XRD_END_MONTH  (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'RD_START_HOUR  : start HOUR  for irrigation of roads','TWN',CFNAM_RD_START_HOUR ,   &
                 CFTYP_RD_START_HOUR ,XUNIF_RD_START_HOUR ,TIR%XRD_START_HOUR (:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'RD_END_HOUR    : end   HOUR  for irrigation of roads','TWN',CFNAM_RD_END_HOUR ,     &
                 CFTYP_RD_END_HOUR   ,XUNIF_RD_END_HOUR   ,TIR%XRD_END_HOUR   (:))  
CATYPE = 'ARI'
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'RD_24H_IRRIG   : total irrigation over 24h for roads','TWN',CFNAM_RD_24H_IRRIG ,    &
                 CFTYP_RD_24H_IRRIG  ,XUNIF_RD_24H_IRRIG  ,TIR%XRD_24H_IRRIG  (:))  
!
END IF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_IRRIG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_IRRIG
