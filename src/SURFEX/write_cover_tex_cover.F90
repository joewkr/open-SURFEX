!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      SUBROUTINE WRITE_COVER_TEX_COVER
!     ##########################
!
!!**** *WRITE_COVER_TEX* writes the cover data arrays into a tex file
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
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODE_WRITE_COVER_TEX

USE MODD_WRITE_COVER_TEX,ONLY : NTEX, CNAME, CLANG, NLINES
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_SEA, XDATA_WATER, XDATA_NATURE, XDATA_TOWN

USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
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
 CHARACTER(LEN=5), DIMENSION(4       ) :: YDATA_SURFTYPE! main surface type
 CHARACTER(LEN=6)                      :: YSTRING6
!
 CHARACTER(LEN=200):: YFMT  ! fortran format
!
LOGICAL           :: GLINE ! flag to write an additional horizontal line
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_COVER',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_COVER',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
GLINE = .FALSE.
!
!
I=0
DO 

  IF (I==JPCOVER) EXIT

  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '&& urban and & cultivated and &inland waters&seas and\\'
    WRITE(NTEX,*) '&Cover Type& artificial areas & natural areas&&oceans\\'
  ELSE
    WRITE(NTEX,*) "&& zones urbanis\'ees & zones cultiv\'ees &eaux int\'erieures& mers et\\"
    WRITE(NTEX,*) "&Type de Surface& ou artificielles & ou naturelles &&oc\'eans\\"
  END IF
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO 
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_TOWN(I)+XDATA_NATURE(I)+XDATA_WATER(I)+XDATA_SEA(I)>0.) THEN
     IP=IP+1
     IF (XDATA_TOWN  (I)==0.) THEN
        YDATA_SURFTYPE(1) = '    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TOWN(I)),'.',DEC(XDATA_TOWN(I)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_TOWN(I)
        YDATA_SURFTYPE(1) = YSTRING6
      END IF
      IF (XDATA_NATURE(I)==0.) THEN
        YDATA_SURFTYPE(2) = '    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_NATURE(I)),'.',DEC(XDATA_NATURE(I)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_NATURE(I)
        YDATA_SURFTYPE(2) = YSTRING6
      END IF
      IF (XDATA_WATER (I)==0.) THEN
        YDATA_SURFTYPE(3) = '    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_WATER(I)),'.',DEC(XDATA_WATER(I)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_WATER(I)
        YDATA_SURFTYPE(3) = YSTRING6
      END IF
      IF (XDATA_SEA (I)==0.) THEN
        YDATA_SURFTYPE(4) = '    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_SEA(I)),'.',DEC(XDATA_SEA(I)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_SEA(I)
        YDATA_SURFTYPE(4) = YSTRING6
      END IF

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I),' & ',YDATA_SURFTYPE(1),' & ',YDATA_SURFTYPE(2),' & ',&
                  YDATA_SURFTYPE(3),' & ', YDATA_SURFTYPE(4), ' \\'  
      GLINE=.TRUE.
      WRITE(NTEX,*) '\hline'
      CALL HLINE(NTEX,GLINE,I)
      IF (IP==NLINES) EXIT
    END IF

  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
!
ENDDO
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_COVER
