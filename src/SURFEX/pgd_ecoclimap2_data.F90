!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE PGD_ECOCLIMAP2_DATA (KYEAR, PDATA_VEGTYPE, &
                                      HPROGRAM)
!     #########################
!
!!**** *PGD_ECOCLIMAP2_DATA* initializes cover-field correspondance arrays
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
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
!!    Original    15/12/97
!!    F.solmon    01/06/00 adaptation for patch approach
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODD_DATA_COVER,     ONLY : TDATA_SEED, TDATA_REAP, XDATA_WATSUP, XDATA_IRRIG,&
                                  LDATA_IRRIG, XDATA_VEGTYPE, LCLIM_LAI  

!

USE MODD_DATA_COVER_PAR, ONLY : NVT_IRR, JPCOVER
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_ECOCLIMAP2_LAI
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
INTEGER, INTENT(INOUT) :: KYEAR
REAL, DIMENSION(:,:), INTENt(IN) :: PDATA_VEGTYPE
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: IGLB      ! logical units
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: IERR      ! return codes
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JCOVER,JDEC,JVEGTYPE ! loop counters on covers and decades
!
INTEGER, DIMENSION(:), ALLOCATABLE   :: IVALUE   ! value of a record of data points

!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YIRRIG   ! file name for irrigation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                        
NAMELIST/NAM_ECOCLIMAP2/  YIRRIG, LCLIM_LAI
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    1.      Read namelist
!  -------------------------------------
!
!* Initializations 
!
IF (LHOOK) CALL DR_HOOK('PGD_ECOCLIMAP2_DATA',0,ZHOOK_HANDLE)
YIRRIG         = '                          '
LCLIM_LAI      = .TRUE.
KYEAR          = NUNDEF
!
!* Reading
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_ECOCLIMAP2',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_ECOCLIMAP2)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
!*    2.         Verifications
!               ----------------
!
  LDATA_IRRIG=(LEN_TRIM(YIRRIG)>0)
!
!-------------------------------------------------------------------------------
!
!
!*   3.    second version of ecoclimap (europe)
!          ----------------------------------- 
!
!
!*   3.1.   read the irrigation data
!           ----------------------
!
IERR=0

IF (LEN_TRIM(YIRRIG)>0) THEN
ALLOCATE(IVALUE(7))    

 CALL OPEN_FILE(HPROGRAM,IGLB,YIRRIG,'FORMATTED',HACTION='READ') 
               
DO JCOVER=301,JPCOVER
  READ(IGLB,FMT='(7I4)') IVALUE
  IF (XDATA_VEGTYPE(JCOVER,NVT_IRR).NE.0) THEN
    TDATA_SEED(JCOVER,NVT_IRR )%TDATE%MONTH = IVALUE(2)
    TDATA_SEED(JCOVER,NVT_IRR )%TDATE%DAY   = IVALUE(3)
    TDATA_REAP(JCOVER,NVT_IRR )%TDATE%MONTH = IVALUE(4)
    TDATA_REAP(JCOVER,NVT_IRR )%TDATE%DAY   = IVALUE(5)
    XDATA_WATSUP(JCOVER,NVT_IRR) = IVALUE(6)
    XDATA_IRRIG (JCOVER,NVT_IRR) = IVALUE(7)
  ENDIF
  !
  IF (XDATA_VEGTYPE(JCOVER,NVT_IRR).NE.0 .AND. &
      (IVALUE(2).EQ.0 .OR. IVALUE(3).EQ.0 .OR. IVALUE(4).EQ.0 .OR. &
      IVALUE(5).EQ.0 .OR. IVALUE(6).EQ.0 .OR. IVALUE(7).EQ.0)) THEN    
      WRITE(ILUOUT,*)'**************************************************'
      WRITE(ILUOUT,*)'* error, missing data in ',YIRRIG,' for          *'
      WRITE(ILUOUT,*)'* the class ',JCOVER,'.                          *'
     WRITE(ILUOUT,*)'**************************************************'
     IERR=1
   ENDIF
   IF (XDATA_VEGTYPE(JCOVER,NVT_IRR).EQ.0 .AND. &
      (IVALUE(2).NE.0 .OR. IVALUE(3).NE.0 .OR. IVALUE(4).NE.0 .OR. &
      IVALUE(5).NE.0 .OR. IVALUE(6).NE.0 .OR. IVALUE(7).NE.0)) THEN    
      WRITE(ILUOUT,*)'**************************************************'
      WRITE(ILUOUT,*)'* error, too many data in ',YIRRIG,' for         *'
      WRITE(ILUOUT,*)'* the class ',JCOVER,'.                          *'
      WRITE(ILUOUT,*)'**************************************************'
     IERR=1
   ENDIF
ENDDO
                
 CALL CLOSE_FILE(HPROGRAM,IGLB)

IF (IERR.EQ.1) CALL ABOR1_SFX('PGD_ECOCLIMAP2_DATA (3)')

DEALLOCATE(IVALUE)
END IF
!
!-------------------------------------------------------------------------------
!
!    4.    Computes LAI evolution for the chosen year
!          ------------------------------------------
!
 CALL ECOCLIMAP2_LAI(KYEAR)
!
IF (LHOOK) CALL DR_HOOK('PGD_ECOCLIMAP2_DATA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_ECOCLIMAP2_DATA
