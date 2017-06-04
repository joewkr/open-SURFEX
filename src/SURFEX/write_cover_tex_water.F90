!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      SUBROUTINE WRITE_COVER_TEX_WATER
!     ##########################
!
!!**** *WRITE_COVER_TEX* writes the water data arrays into a tex file
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
!!    Original    08/01/98
!!    03/2011 E. Bazile (MK10) albedo from Marat Khairoutdinov
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODE_WRITE_COVER_TEX
!
USE MODI_ALBEDO_TA96
!
USE MODD_CSTS,           ONLY : XPI
USE MODD_WRITE_COVER_TEX,ONLY : NTEX, CNAME, CLANG, NLINES
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_DATA_COVER,     ONLY : XDATA_SEA, XDATA_WATER
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_WATER_PAR,      ONLY : XALBWAT, XALBSCA_WAT, XEMISWAT
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
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
!
INTEGER :: I,IP
!
 CHARACTER(LEN=6)  :: YSTRING6
!
 CHARACTER(LEN=200):: YFMT  ! fortran format
!
LOGICAL           :: GLINE ! flag to write an additional horizontal line
!
 CHARACTER(LEN=6), DIMENSION(6) :: YDATA_WATER   ! water parameters
REAL,             DIMENSION(1) :: ZZENITH       ! zenithal angle
REAL,             DIMENSION(1) :: ZALBEDO       ! direct albedo
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_WATER',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_WATER',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
I=0
DO 
  IF (I==JPCOVER) EXIT
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf water parameters}\\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres aquatiques}\\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$\alpha$&$\alpha_{dir}$&$\alpha_{sca}$&$\epsilon$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_SEA(I) + XDATA_WATER(I)>0.) THEN
      IP=IP+1
      WRITE(YSTRING6, FMT='(F3.2)') XALBWAT
      YDATA_WATER(1) = YSTRING6

      ZZENITH(:) = 0.
      ZALBEDO(:) = ALBEDO_TA96(ZZENITH)
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZALBEDO(1)),'.',DEC(ZALBEDO(1)),')'
      WRITE(YSTRING6, FMT=YFMT) ZALBEDO(1)
      YDATA_WATER(2) = YSTRING6

      ZZENITH(:) = XPI/2.
      ZALBEDO(:) = ALBEDO_TA96(ZZENITH)  
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZALBEDO(1)),'.',DEC(ZALBEDO(1)),')'
      WRITE(YSTRING6, FMT=YFMT) ZALBEDO(1)
      YDATA_WATER(3) = YSTRING6

      WRITE(YSTRING6, FMT='(F3.2)') XALBSCA_WAT
      YDATA_WATER(4) = YSTRING6
      WRITE(YSTRING6, FMT='(F4.2)') XEMISWAT
      YDATA_WATER(5) = YSTRING6

      WRITE(NTEX,FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_WATER(1),' & ', &
            YDATA_WATER(2),'-',YDATA_WATER(3),' & ', &
            YDATA_WATER(4),' & ', &
            YDATA_WATER(5),' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_WATER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_WATER
