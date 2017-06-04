!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      SUBROUTINE WRITE_COVER_TEX_TEB
!     ##########################
!
!!**** *WRITE_COVER_TEX* writes the TEB data arrays into a tex file
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
USE MODD_DATA_COVER,     ONLY : XDATA_TOWN, XDATA_WALL_O_HOR,           &
                                  XDATA_ALB_ROOF, XDATA_EMIS_ROOF,        &
                                  XDATA_ALB_ROAD, XDATA_EMIS_ROAD,        &
                                  XDATA_ALB_WALL, XDATA_EMIS_WALL,        &
                                  XDATA_HC_ROOF, XDATA_TC_ROOF,           &
                                  XDATA_D_ROOF, XDATA_HC_ROAD,            &
                                  XDATA_TC_ROAD, XDATA_D_ROAD,            &
                                  XDATA_HC_WALL, XDATA_TC_WALL,           &
                                  XDATA_D_WALL, XDATA_CAN_HW_RATIO,       &
                                  XDATA_Z0_TOWN, XDATA_BLD,               &
                                  XDATA_BLD_HEIGHT  


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
 CHARACTER(LEN=6), DIMENSION(8) :: YDATA_TOWN    ! town parameters
 CHARACTER(LEN=6), DIMENSION(9) :: YDATA_MATE    ! materials parameters
 CHARACTER(LEN=6)               :: YSTRING6
!
 CHARACTER(LEN=200):: YFMT  ! fortran format
LOGICAL           :: GLINE ! flag to write an additional horizontal line
!
REAL, DIMENSION(JPCOVER) :: ZDATA_SVF_ROAD,ZDATA_SVF_WALL
REAL, DIMENSION(SIZE(XDATA_HC_ROOF,1),SIZE(XDATA_HC_ROOF,2)) :: ZDATA_HC_ROOF,ZDATA_HC_ROAD,ZDATA_HC_WALL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_TEB',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_TEB',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
GLINE=.FALSE.
!
ZDATA_SVF_ROAD=XUNDEF
ZDATA_SVF_WALL=XUNDEF
WHERE (XDATA_CAN_HW_RATIO.NE.XUNDEF)
  ZDATA_SVF_ROAD = SQRT(XDATA_CAN_HW_RATIO**2+1.) - XDATA_CAN_HW_RATIO
  WHERE (XDATA_CAN_HW_RATIO>0.)
    ZDATA_SVF_WALL = 0.5 * (XDATA_CAN_HW_RATIO + 1. - SQRT(XDATA_CAN_HW_RATIO**2+1.) ) &
                         / XDATA_CAN_HW_RATIO  
  ELSEWHERE
    ZDATA_SVF_WALL = 0.5
  END WHERE
END WHERE

ZDATA_HC_ROOF=XDATA_HC_ROOF
ZDATA_HC_ROAD=XDATA_HC_ROAD
ZDATA_HC_WALL=XDATA_HC_WALL
WHERE (ZDATA_HC_ROOF.EQ.XUNDEF)
  ZDATA_HC_ROOF=XUNDEF*1.E+6
END WHERE
WHERE (ZDATA_HC_ROAD.EQ.XUNDEF)
  ZDATA_HC_ROAD=XUNDEF*1.E+6
END WHERE
WHERE (ZDATA_HC_WALL.EQ.XUNDEF)
  ZDATA_HC_WALL=XUNDEF*1.E+6
END WHERE
!
!-------------------------------------------------------------------------------
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf town parameters (1)}\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres de ville (1)}\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$z_{0_{town}}$&bld frac.&$h$&h/L&{h/W} $^\star$&'
  WRITE(NTEX,*) '$\alpha_{_{roof}}$&'
  WRITE(NTEX,*) '$\epsilon_{_{roof}}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO 
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_TOWN(I)>0. .AND. XDATA_Z0_TOWN(I).NE.XUNDEF) THEN
      IP=IP+1
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_Z0_TOWN(I)),'.',DEC(XDATA_Z0_TOWN(I)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_Z0_TOWN(I)
      YDATA_TOWN(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_BLD(I)),'.',DEC(XDATA_BLD(I)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_BLD(I)
      YDATA_TOWN(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_BLD_HEIGHT(I)),'.',DEC(XDATA_BLD_HEIGHT(I)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_BLD_HEIGHT(I)
      YDATA_TOWN(3) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_WALL_O_HOR(I)),'.',DEC(XDATA_WALL_O_HOR(I)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_WALL_O_HOR(I)
      YDATA_TOWN(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_CAN_HW_RATIO(I)),'.',DEC(XDATA_CAN_HW_RATIO(I)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_CAN_HW_RATIO(I)
      YDATA_TOWN(5) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_ALB_ROOF(I)
      YDATA_TOWN(6) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_EMIS_ROOF(I)
      YDATA_TOWN(7) = YSTRING6

      WRITE(NTEX, FMT=*)                                             &
          I,' &',CNAME(I),' &', YDATA_TOWN(1),' &',YDATA_TOWN(2),' &', &
            YDATA_TOWN(3),' &', YDATA_TOWN(4),' &',                    &
            YDATA_TOWN(5),' &', YDATA_TOWN(6),' &',                    &
            YDATA_TOWN(7),' \\'  
      GLINE=.TRUE.
      WRITE(NTEX,*) '\hline'
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
    END DO
    WRITE(NTEX,*) '\end{tabular}'
   WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'

ENDDO
!
 
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf town parameters (2)}\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres de ville (2)}\'
  END IF
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$\alpha_{_{road}}$&'
  WRITE(NTEX,*) '$\epsilon_{_{road}}$&$\psi_{s_{road}}$ $^\dagger$&'
  WRITE(NTEX,*) '$\alpha_{_{wall}}$&'
  WRITE(NTEX,*) '$\epsilon_{_{wall}}$&$\psi_{s_{wall}}$ $^\ddagger$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_TOWN(I)>0. .AND. XDATA_Z0_TOWN(I).NE.XUNDEF) THEN
      IP=IP+1
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_ALB_ROAD(I)
      YDATA_TOWN(1) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_EMIS_ROAD(I)
      YDATA_TOWN(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_SVF_ROAD(I)),'.',DEC(ZDATA_SVF_ROAD(I)),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_SVF_ROAD(I)
      YDATA_TOWN(3) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_ALB_WALL(I)
      YDATA_TOWN(4) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') XDATA_EMIS_WALL(I)
      YDATA_TOWN(5) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_SVF_WALL(I)),'.',DEC(ZDATA_SVF_WALL(I)),')'
      WRITE(YSTRING6, FMT='(F3.2)') ZDATA_SVF_WALL(I)
      YDATA_TOWN(6) = YSTRING6

      WRITE(NTEX, FMT=*)                                             &
          I,' &',CNAME(I),' &', YDATA_TOWN(1),' &',YDATA_TOWN(2),' &', &
            YDATA_TOWN(3),' &', YDATA_TOWN(4),' &',                    &
            YDATA_TOWN(5),' &', YDATA_TOWN(6),' \\'  
      GLINE=.TRUE.
      WRITE(NTEX,*) '\hline'
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'
  WRITE(NTEX,*) ' '
ENDDO
!
!
!-------------------------------------------------------------------------------
!
I=0
DO 
  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '$^\star$ {h/W} is computed from the vegetation fraction, the building '
    WRITE(NTEX,*) 'fraction of artificial area, and h/L'
  ELSE
    WRITE(NTEX,*) "$^\star$ {h/W} est calcul\'e \`a partir de la fraction de v\'eg\'etation,"
    WRITE(NTEX,*) 'de la fraction de batiments dans la surface construite, et de h/L'
  END IF
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '$^\dagger$ $\psi_{s_{road}} = \sqrt{\frac{h^2}{W^2}+1}-\frac{h}{W}$ '
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '$^\ddagger$ $\psi_{s_{wall}} = \frac{1}{2}\frac{W}{h}\left(\frac{h}{W}+1-\sqrt{\frac{h^2}{W^2}+1}\right)$'


  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf roof thermal parameters }\\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres thermiques des toits}\\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$C_{_{roof}}^{(1)}$&$C_{_{roof}}^{(2)}$&$C_{_{roof}}^{(3)}$&'
  WRITE(NTEX,*) '$\lambda_{_{roof}}^{(1)}$&$\lambda_{_{roof}}^{(2)}$&$\lambda_{_{roof}}^{(3)}$&'
  WRITE(NTEX,*) '$D_{_{roof}}^{(1)}$&$D_{_{roof}}^{(2)}$&$D_{_{roof}}^{(3)}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1

    IF (XDATA_TOWN(I)>0. .AND. XDATA_Z0_TOWN(I).NE.XUNDEF) THEN
      IP=IP+1

      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROOF(I,1)*1.E-6),'.',DEC(ZDATA_HC_ROOF(I,1)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROOF(I,1)*1.E-6
      YDATA_MATE(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROOF(I,2)*1.E-6),'.',DEC(ZDATA_HC_ROOF(I,2)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROOF(I,2)*1.E-6
      YDATA_MATE(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROOF(I,3)*1.E-6),'.',DEC(ZDATA_HC_ROOF(I,3)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROOF(I,3)*1.E-6
      YDATA_MATE(3) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROOF(I,1)),'.',DEC(XDATA_TC_ROOF(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROOF(I,1)
      YDATA_MATE(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROOF(I,2)),'.',DEC(XDATA_TC_ROOF(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROOF(I,2)
      YDATA_MATE(5) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROOF(I,3)),'.',DEC(XDATA_TC_ROOF(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROOF(I,3)
      YDATA_MATE(6) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROOF(I,1)),'.',DEC(XDATA_D_ROOF(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROOF(I,1)
      YDATA_MATE(7) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROOF(I,2)),'.',DEC(XDATA_D_ROOF(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROOF(I,2)
      YDATA_MATE(8) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROOF(I,3)),'.',DEC(XDATA_D_ROOF(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROOF(I,3)
      YDATA_MATE(9) = YSTRING6


      WRITE(NTEX, FMT=*)                                             &
          I,' &',CNAME(I),' &', YDATA_MATE(1),' &',YDATA_MATE(2),' &', &
            YDATA_MATE(3),' &', YDATA_MATE(4),' &',                    &
            YDATA_MATE(5),' &', YDATA_MATE(6),' &',                    &
            YDATA_MATE(7),' &', YDATA_MATE(8),' &',YDATA_MATE(9),' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'
ENDDO

I=0
DO 
  IF (I==JPCOVER) EXIT
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf road thermal parameters}\\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres thermiques des rues}\\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$C_{_{road}}^{(1)}$&$C_{_{road}}^{(2)}$&$C_{_{road}}^{(3)}$&'
  WRITE(NTEX,*) '$\lambda_{_{road}}^{(1)}$&$\lambda_{_{road}}^{(2)}$&$\lambda_{_{road}}^{(3)}$&'
  WRITE(NTEX,*) '$D_{_{road}}^{(1)}$&$D_{_{road}}^{(2)}$&$D_{_{road}}^{(3)}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_TOWN(I)>0. .AND. XDATA_Z0_TOWN(I).NE.XUNDEF) THEN
      IP=IP+1
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROAD(I,1)*1.E-6),'.',DEC(ZDATA_HC_ROAD(I,1)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROAD(I,1)*1.E-6
      YDATA_MATE(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROAD(I,2)*1.E-6),'.',DEC(ZDATA_HC_ROAD(I,2)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROAD(I,2)*1.E-6
      YDATA_MATE(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_ROAD(I,3)*1.E-6),'.',DEC(ZDATA_HC_ROAD(I,3)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_ROAD(I,3)*1.E-6
      YDATA_MATE(3) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROAD(I,1)),'.',DEC(XDATA_TC_ROAD(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROAD(I,1)
      YDATA_MATE(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROAD(I,2)),'.',DEC(XDATA_TC_ROAD(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROAD(I,2)
      YDATA_MATE(5) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_ROAD(I,3)),'.',DEC(XDATA_TC_ROAD(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_ROAD(I,3)
      YDATA_MATE(6) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROAD(I,1)),'.',DEC(XDATA_D_ROAD(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROAD(I,1)
      YDATA_MATE(7) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROAD(I,2)),'.',DEC(XDATA_D_ROAD(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROAD(I,2)
      YDATA_MATE(8) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_ROAD(I,3)),'.',DEC(XDATA_D_ROAD(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_ROAD(I,3)
      YDATA_MATE(9) = YSTRING6


      WRITE(NTEX, FMT=*)                                             &
          I,' &',CNAME(I),' &', YDATA_MATE(1),' &',YDATA_MATE(2),' &', &
            YDATA_MATE(3),' &', YDATA_MATE(4),' &',                    &
            YDATA_MATE(5),' &', YDATA_MATE(6),' &',                    &
            YDATA_MATE(7),' &', YDATA_MATE(8),' &',YDATA_MATE(9),' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!
  WRITE(NTEX,*) ' '
  WRITE(NTEX,*) '\bigskip'
ENDDO

I=0
DO 
  IF (I==JPCOVER) EXIT
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf wall thermal parameters}\\'
  ELSE
    WRITE(NTEX,*) '{\bf param\`etres thermiques des murs}\\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$C_{_{wall}}^{(1)}$&$C_{_{wall}}^{(2)}$&$C_{_{wall}}^{(3)}$&'
  WRITE(NTEX,*) '$\lambda_{_{wall}}^{(1)}$&$\lambda_{_{wall}}^{(2)}$&$\lambda_{_{wall}}^{(3)}$&'
  WRITE(NTEX,*) '$D_{_{wall}}^{(1)}$&$D_{_{wall}}^{(2)}$&$D_{_{wall}}^{(3)}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I+1
    IF (XDATA_TOWN(I)>0. .AND. XDATA_Z0_TOWN(I).NE.XUNDEF) THEN
      IP=IP+1
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_WALL(I,1)*1.E-6),'.',DEC(ZDATA_HC_WALL(I,1)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_WALL(I,1)*1.E-6
      YDATA_MATE(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_WALL(I,2)*1.E-6),'.',DEC(ZDATA_HC_WALL(I,2)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_WALL(I,2)*1.E-6
      YDATA_MATE(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDATA_HC_WALL(I,3)*1.E-6),'.',DEC(ZDATA_HC_WALL(I,3)*1.E-6),')'
      WRITE(YSTRING6, FMT=YFMT) ZDATA_HC_WALL(I,3)*1.E-6
      YDATA_MATE(3) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_WALL(I,1)),'.',DEC(XDATA_TC_WALL(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_WALL(I,1)
      YDATA_MATE(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_WALL(I,2)),'.',DEC(XDATA_TC_WALL(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_WALL(I,2)
      YDATA_MATE(5) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_TC_WALL(I,3)),'.',DEC(XDATA_TC_WALL(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_TC_WALL(I,3)
      YDATA_MATE(6) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_WALL(I,1)),'.',DEC(XDATA_D_WALL(I,1)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_WALL(I,1)
      YDATA_MATE(7) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_WALL(I,2)),'.',DEC(XDATA_D_WALL(I,2)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_WALL(I,2)
      YDATA_MATE(8) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_D_WALL(I,3)),'.',DEC(XDATA_D_WALL(I,3)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_D_WALL(I,3)
      YDATA_MATE(9) = YSTRING6


      WRITE(NTEX, FMT=*)                                             &
          I,' &',CNAME(I),' &', YDATA_MATE(1),' &',YDATA_MATE(2),' &', &
            YDATA_MATE(3),' &', YDATA_MATE(4),' &',                    &
            YDATA_MATE(5),' &', YDATA_MATE(6),' &',                    &
            YDATA_MATE(7),' &', YDATA_MATE(8),' &',YDATA_MATE(9),' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_COVER_TEX_TEB
