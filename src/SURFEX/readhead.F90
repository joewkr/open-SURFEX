!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READHEAD(KGLB,PGLBLATMIN,PGLBLATMAX,PGLBLONMIN,PGLBLONMAX,&
                           KNBLAT,KNBLON,PCUTVAL,PDLAT,PDLON,PLAT,PLON,KERR,KFACT,&
                           OCOMPRESS)  
!     ################################################################
!
!!**** *READHEAD* writes the head a the local 'latlon' file.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!     A header of a data set is of the form:
!!
!!     1 line of comment
!!     nodata: -999
!!     north: 90N             (or S or nothing)
!!     south: 50N             (or S or nothing)    
!!     east: 90W              (or E or nothing)
!!     west: 110W             (or E or nothing)
!!     rows: 180
!!     cols: 60
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
!!    V. Masson              Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    29/08/95
!!      J.Escobar     06/2013  for REAL4/8 add EPSILON management
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODD_CSTS ,ONLY : XSURF_EPSILON
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,           INTENT(IN)  :: KGLB        ! logical unit of the file
REAL,              INTENT(OUT) :: PGLBLATMIN  ! min latitude  of the file.
REAL,              INTENT(OUT) :: PGLBLATMAX  ! max latitude  of the file.
REAL,              INTENT(OUT) :: PGLBLONMIN  ! min longitude of the file.
REAL,              INTENT(OUT) :: PGLBLONMAX  ! min longitude of the file.
INTEGER,           INTENT(OUT) :: KNBLAT      ! number of latitude  rows in file
INTEGER,           INTENT(OUT) :: KNBLON      ! number of longitude rows in file
REAL,              INTENT(OUT) :: PCUTVAL     ! special value in data file
REAL,              INTENT(OUT) :: PDLAT       ! latitude  mesh in the data file
REAL,              INTENT(OUT) :: PDLON       ! longitude mesh in the data file
REAL, DIMENSION(:), POINTER    :: PLAT        ! latitude  of data points
REAL, DIMENSION(:), POINTER    :: PLON        ! longitude of data points
INTEGER,           INTENT(OUT) :: KERR        ! return code
LOGICAL, INTENT(OUT) :: OCOMPRESS
INTEGER, INTENT(OUT) :: KFACT
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                    :: JLAT       ! loop control
INTEGER                    :: JLON       ! loop control
INTEGER                    :: JHEAD      ! loop control
INTEGER                    :: ININDEX    ! index of character 'N' in YSTRING1
INTEGER                    :: ISINDEX    ! index of character 'S' in YSTRING1
INTEGER                    :: IEINDEX    ! index of character 'E' in YSTRING1
INTEGER                    :: IWINDEX    ! index of character 'W' in YSTRING1
REAL, DIMENSION(9)         :: ZVAL       ! values of the head data
INTEGER                    :: IHEAD      ! index of the data in the array ZVAL
 CHARACTER(LEN=100)         :: YSTRING    ! total string in the head
 CHARACTER(LEN=100)         :: YSTRING1   ! string less the begining line descriptor
 CHARACTER(LEN=100)         :: YVAL       ! absolute value of the data of the line
INTEGER                    :: IPOINT     ! index of '.' in the string YVAL
INTEGER                    :: ILENGTH    ! length of the string YVAL
INTEGER                    :: IFRACLENGTH! length of the fractional part in string YVAL
 CHARACTER(LEN=2)           :: YLENGTH    ! length of the string YVAL
 CHARACTER(LEN=2)           :: YFRACLENGTH! length of the fractional part in string YVAL
 CHARACTER(LEN=10)          :: YINTERNALFORMAT ! format to read YVAL in real ZVAL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READHEAD',0,ZHOOK_HANDLE)
KERR=0
!
!*         1.    Line of comments
!                ----------------
!
ZVAL(1:8) = 1.
ZVAL(9)   = 0.
!
READ (KGLB,'(A100)',END=99) YSTRING
!
!-------------------------------------------------------------------------------
!
!*         2.    Other lines
!                -----------
!
DO JHEAD=1,9
  READ (KGLB,'(A100)',END=99) YSTRING
  YSTRING=ADJUSTL(YSTRING)
!
!*         2.1   Selection of the line
!                ---------------------
!
  SELECT CASE (YSTRING(1:5))
         CASE('cutva')
           IHEAD=1
           YSTRING1=YSTRING(10:100)
         CASE('nodat')
           IHEAD=1
           YSTRING1=YSTRING(8:100)
         CASE('north')
           IHEAD=2
           YSTRING1=YSTRING(7:100)           
         CASE('south')
           IHEAD=3
           YSTRING1=YSTRING(7:100) 
         CASE('east:')
           IHEAD=4
           YSTRING1=YSTRING(6:100)  
         CASE('west:')
           IHEAD=5
           YSTRING1=YSTRING(6:100)  
         CASE('rows:')
           IHEAD=6
           YSTRING1=YSTRING(6:100) 
         CASE('cols:')
           IHEAD=7
           YSTRING1=YSTRING(6:100)
         CASE('fact:')
           IHEAD=8
           YSTRING1=YSTRING(6:100)
         CASE('compr')
           IHEAD=9
           YSTRING1=YSTRING(10:100)          
  END SELECT
!
!*         2.2   Test on presence of geographical descritor (N, E, S or W)
!                ---------------------------------------------------------
!
  ININDEX=INDEX(YSTRING1,'N')
  ISINDEX=INDEX(YSTRING1,'S')
  IEINDEX=INDEX(YSTRING1,'E')
  IWINDEX=INDEX(YSTRING1,'W')
  YVAL=ADJUSTL(YSTRING1)
  IF (ININDEX/=0) YVAL=ADJUSTL(YSTRING1(1:ININDEX-1))
  IF (ISINDEX/=0) YVAL='-'//ADJUSTL(YSTRING1(1:ISINDEX-1))
  IF (IEINDEX/=0) YVAL=ADJUSTL(YSTRING1(1:IEINDEX-1))
  IF (IWINDEX/=0) YVAL='-'//ADJUSTL(YSTRING1(1:IWINDEX-1))
!
!*         2.3   Transformation of the data in real
!                ----------------------------------
!
  IPOINT=INDEX(YVAL,'.')
  IF (IPOINT==0) YVAL=ADJUSTL(ADJUSTR(YVAL)//'.')
!
!*         2.4   Definition of the format of the data
!                ------------------------------------
!
  ILENGTH=LEN_TRIM(ADJUSTL(ADJUSTR(YVAL)))
  IFRACLENGTH=ILENGTH-INDEX(YVAL,'.')
  WRITE(YLENGTH,'(I2)') ILENGTH
  WRITE(YFRACLENGTH,'(I2)') IFRACLENGTH
  YINTERNALFORMAT='(F'//YLENGTH//'.'//YFRACLENGTH//')'
!
!*         2.5   Data definition
!                ---------------
!
  READ(YVAL,ADJUSTL(YINTERNALFORMAT)) ZVAL(IHEAD)
!
ENDDO
!
99 CONTINUE
!-------------------------------------------------------------------------------
!
!*         3.    Initialization of arguments, longitudes and latitudes
!                -----------------------------------------------------
!
PCUTVAL=ZVAL(1)
PGLBLATMAX=ZVAL(2)
PGLBLATMIN=ZVAL(3)
PGLBLONMIN=ZVAL(5)
PGLBLONMAX=ZVAL(4)+NINT((ZVAL(5)-ZVAL(4)+180.*(1.0+XSURF_EPSILON))/360.)*360.
KNBLAT=NINT(ZVAL(6))
KNBLON=NINT(ZVAL(7))
KFACT=NINT(ZVAL(8))
OCOMPRESS = .FALSE.
IF (ZVAL(9)==1.) OCOMPRESS = .TRUE.
!
PDLAT=(PGLBLATMAX-PGLBLATMIN)/KNBLAT
PDLON=(PGLBLONMAX-PGLBLONMIN)/KNBLON
ALLOCATE(PLAT(KNBLAT))
ALLOCATE(PLON(KNBLON))
PLAT(:)=(/ (PGLBLATMAX-(JLAT-0.5)*PDLAT, JLAT=1,KNBLAT) /)
PLON(:)=(/ (PGLBLONMIN+(JLON-0.5)*PDLON, JLON=1,KNBLON) /)
!
!IF (LHOOK) CALL DR_HOOK('READHEAD',1,ZHOOK_HANDLE)
!RETURN
!KERR=-1
IF (LHOOK) CALL DR_HOOK('READHEAD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE READHEAD
