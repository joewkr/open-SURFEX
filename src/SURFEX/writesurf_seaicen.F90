!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SEAICE_n (HSELECT, S, HPROGRAM)
!     #########################################
!
!!****  *WRITESURF_SEAICE_n* - write seaice scheme variables
!!
!!
!!    PURPOSE : writes state variable and 'domain' structure
!!    -------
!!
!!**  METHOD : 
!!    ------
!!      For now, only Gelato scheme is handled
!!
!!      quite standard in Surfex : use WRITE_SURF with 
!!         relevant field names (same names as in genuine gelato restarts)
!!
!!    EXTERNALS : WRITE_SURF, GLT_ALLOC, GET_TYPE_DIM
!!    --------
!!
!!    IMPLICIT ARGUMENTS : Gelato state variable, and some namelist parameters
!!    ------------------
!!
!!    REFERENCE : 
!!    ---------
!!
!!    AUTHOR : S. Sénési   *Meteo France*
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_GLT_PARAM, ONLY : nl, nt
USE MODD_TYPES_GLT,   ONLY : T_GLT
!
USE MODI_WRITE_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP           ! Error code after reading
!
INTEGER           :: JMTH, INMTH
CHARACTER(LEN=2 ) :: YMTH
CHARACTER(LEN=5)  :: YLVL
!
CHARACTER(LEN=6)  :: YICECAT
CHARACTER(LEN=20) :: YFORM
CHARACTER(LEN=12) :: YRECFM           ! Name of the article to be read
CHARACTER(LEN=12) :: YCATEG           ! Category to write
CHARACTER(LEN=12) :: YLEVEL           ! Level to write
CHARACTER(LEN=100):: YCOMMENT         ! Error Message
!
INTEGER :: JK,JL                   ! loop counter on ice categories and layes 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAICE_n',0,ZHOOK_HANDLE)
!
!
YCOMMENT='(-)'
CALL WRITE_SURF(HSELECT,  HPROGRAM,'SEAICE_SCHEM',S%CSEAICE_SCHEME,IRESP,YCOMMENT)
!
!
IF (S%CSEAICE_SCHEME == 'GELATO') THEN 
   YCOMMENT='Number of sea-ice layers'
   CALL WRITE_SURF(HSELECT,HPROGRAM,'ICENL',nl,IRESP,YCOMMENT)
   YCOMMENT='Number of ice categories'
   CALL WRITE_SURF(HSELECT,HPROGRAM,'ICENT',nt,IRESP,YCOMMENT)
   !
   !*       1.     Prognostic fields with only space dimension(s) :
   !
   YCOMMENT='ICEUSTAR ()'
   CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEUSTAR',S%TGLT%ust(:,1),IRESP,YCOMMENT)
   !
   !*       2.     Prognostic fields with space and ice-category dimension(s) :
   !
   DO JK=1,nt
      WRITE(YICECAT,'(I2)') JK
      YCATEG='_'//ADJUSTL(YICECAT)
      ! .. Write sea ice age for type JK
      YCOMMENT='X_Y_ICEAGE'//YCATEG//' (s)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEAGE'//YCATEG,S%TGLT%sit(JK,:,1)%age,IRESP,YCOMMENT)
      ! .. Write melt pond volume for type JK
      YCOMMENT='X_Y_ICEVMP'//YCATEG//' (m3)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEVMP'//YCATEG,S%TGLT%sit(JK,:,1)%vmp,IRESP,YCOMMENT)
      ! .. Write sea ice surface albedo for type JK
      YCOMMENT='X_Y_ICEASN'//YCATEG//' ([0-1])'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEASN'//YCATEG,S%TGLT%sit(JK,:,1)%asn,IRESP,YCOMMENT)
      ! .. Write sea ice fraction for type JK
      YCOMMENT='X_Y_ICEFSI'//YCATEG//' ([0-1])'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEFSI'//YCATEG, S%TGLT%sit(JK,:,1)%fsi,IRESP,YCOMMENT)
      ! .. Write sea ice thickness for type JK
      YCOMMENT='X_Y_ICEHSI'//YCATEG//' (m)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEHSI'//YCATEG, S%TGLT%sit(JK,:,1)%hsi,IRESP,YCOMMENT)
      ! .. Write sea ice salinity for type JK
      YCOMMENT='X_Y_ICESSI'//YCATEG//' (psu)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICESSI'//YCATEG, S%TGLT%sit(JK,:,1)%ssi,IRESP,YCOMMENT)
      ! .. Write sea ice surface temperature for type JK
      YCOMMENT='X_Y_ICETSF'//YCATEG//' (K)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICETSF'//YCATEG, S%TGLT%sit(JK,:,1)%tsf,IRESP,YCOMMENT)
      ! .. Write snow thickness for type JK
      YCOMMENT='X_Y_ICEHSN'//YCATEG//' (m)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICEHSN'//YCATEG, S%TGLT%sit(JK,:,1)%hsn,IRESP,YCOMMENT)
      ! .. Write snow density for type JK
      YCOMMENT='X_Y_ICERSN'//YCATEG//' (kg m-3)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,'ICERSN'//YCATEG, S%TGLT%sit(JK,:,1)%rsn,IRESP,YCOMMENT)
      !
      !*       3.     Prognostic fields with space and ice-category and layer dimension(s) :
      !
      DO JL=1,NL
        WRITE(YLVL,'(I2)') JL
        YLEVEL = YCATEG(1:LEN_TRIM(YCATEG))//'_'//ADJUSTL(YLVL)
        YFORM='(A6,I1.1,A4)'
        IF (JL >= 10)  YFORM='(A6,I2.2,A4)'
        WRITE(YCOMMENT,FMT=YFORM) 'X_Y_ICEH',JL,' (J/kg)'
        ! .. Write sea ice vertical gltools_enthalpy profile for type JK and level JL  
        CALL WRITE_SURF(HSELECT, &
                HPROGRAM,'ICEH'//YLEVEL, S%TGLT%sil(JL,JK,:,1)%ent,IRESP,YCOMMENT)
      END DO

   END DO
ELSE
   ! This is a placeholder for writing state variables for another seaice scheme
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!* sea ice cover
!
IF(S%LINTERPOL_SIC)THEN
!
   INMTH=SIZE(S%XSIC_MTH,2)
!
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIC_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      YCOMMENT='Sea ice coverage at month t'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIC_MTH(:,JMTH),IRESP,HCOMMENT=YCOMMENT)
   ENDDO
!
ENDIF
!
YRECFM='SIC'
YCOMMENT='Sea ice coverage'
CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIC(:),IRESP,HCOMMENT=YCOMMENT)  
!
!
!* sea ice thickness constraint
!
IF(S%LINTERPOL_SIT)THEN
!
   INMTH=SIZE(S%XSIT_MTH,2)
!
   DO JMTH=1,INMTH
      WRITE(YMTH,'(I2)') (JMTH-1)
      YRECFM='SIT_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      YCOMMENT='Sea ice thickness constraint at month t'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,S%XSIT_MTH(:,JMTH),IRESP,HCOMMENT=YCOMMENT)
   ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SEAICE_n',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
END SUBROUTINE WRITESURF_SEAICE_n
