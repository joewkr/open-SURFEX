!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
    SUBROUTINE MAKE_MASK_TOPD_TO_ISBA (HGRID, PGRID_PAR, KI)
!     #######################
!
!!****  *MAKE_MASK_TOPD_TO_ISBA(*  
!!
!!    PURPOSE
!!    -------
!
!     Create a mask for each catchment. 
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!                     
!!    REFERENCE
!!    ---------
     
!!    AUTHOR
!!    ------
!!
!!      L. Labatut & K. Chancibault     * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   16/03/2005
!!                 03/2014 (E. Artinian) manages the option CGRID='IGN'
!!                 07/2015 (E. Artinian) corrections  for option CGRID='IGN'
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TOPODYN,       ONLY: NNCAT, NNYC, XY0, XDXT, NNXC,&
                                XX0, XTOPD, XNUL, NLINE, NMESHT
USE MODD_COUPLING_TOPD, ONLY: NMASKT
USE MODD_SURF_PAR,        ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_WRITE_FILE_MAP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HGRID
REAL, DIMENSION(:), INTENT(IN) :: PGRID_PAR
!
INTEGER, INTENT(IN) :: KI    ! Grid dimensions
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=30)  :: YVAR        ! name of results file
INTEGER            :: JCAT, JJ, JI, IDX     ! loop control  
INTEGER            :: II,ILINE ! work integer variables
INTEGER            :: IDXM   ! indexes of Isba grid meshes and nodes
INTEGER            :: ILUOUT   ! unit
REAL :: ZXT, ZYT ! catchment grid nodes Lambert II coordinates 
REAL, DIMENSION(MAX(1,KI-1)) :: ZX1, ZX2, ZX3, ZX4, ZY1, ZY2, ZY3, ZY4  ! Isba mesh Lambert II coordinates
REAL :: ZXA, ZXB, ZYA, ZYB
REAL, DIMENSION(NNCAT,NMESHT):: ZWRK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT('OFFLIN',ILUOUT)
!
DO IDXM=1,MAX(KI-1,1)
  CALL INIT_4POINTS(IDXM,ZX1(IDXM),ZX2(IDXM),ZX3(IDXM),ZX4(IDXM),&
                         ZY1(IDXM),ZY2(IDXM),ZY3(IDXM),ZY4(IDXM))
ENDDO
!
!WRITE(*,*) 'Y0=', XY0(1), XY0(2)
!WRITE(*,*) 'X0=', XX0(1), XX0(2)
!WRITE(*,*) 'X et Y isba=', XXI(1), XXI(180), XYI(1), XYI(180)
!loop on catchments
DO JCAT=1,NNCAT
  !
  DO JJ=1,NNYC(JCAT)   ! number of topographic points on the y axis
    !
    ZYT = XY0(JCAT) + (JJ-1) * XDXT(JCAT)
    ZYT = ZYT + 0.5 * XDXT(JCAT)
    !
    DO JI=1,NNXC(JCAT)
      !
      ZXT = XX0(JCAT) + (JI-1) * XDXT(JCAT)
      ZXT = ZXT + 0.5 * XDXT(JCAT)
      !
      IDX = (JJ-1) * NNXC(JCAT) + JI   ! index of the point among all in the catchment
      !
      !* on vérifie que le pixel du MNT appartient au BV
      IF ( XTOPD(JCAT,IDX).NE.XNUL(JCAT) ) THEN
        !* calcule des coordonées X et Y inf et sup de la maille ISBA considérée
        CALL GET_COORD(ZXT,ZYT,ZX1(1),ZX2(1),ZX3(1),ZX4(1),ZY1(1),ZY2(1),ZY3(1),ZY4(1),ZXA,ZYA,ZXB,ZYB)
        !* si on se trouve sur le premier pixel du MNT ou si le pixel du MNT n'est pas 
        !dans la maille Isba considérée (qui est celle dans laquelle se trouve le pixel précédent)
        IF (ZXT.LT.ZXA.OR.ZXT.GE.ZXB.OR.ZYT.LT.ZYA.OR.ZYT.GE.ZYB) THEN
          !* on repart de la première maille de la grille Isba
          IDXM = 1
          CALL GET_COORD(ZXT,ZYT,ZX1(IDXM),ZX2(IDXM),ZX3(IDXM),ZX4(IDXM),&
                                 ZY1(IDXM),ZY2(IDXM),ZY3(IDXM),ZY4(IDXM),ZXA,ZYA,ZXB,ZYB)
          !* on parcours les mailles de la grille Isba, jusqu'à ce qu'on trouve la maille à laquelle appartient le pixel du MNT
          DO WHILE (ZXT.LT.ZXA.OR.ZXT.GE.ZXB.OR.ZYT.LT.ZYA.OR.ZYT.GE.ZYB)
            IDXM = IDXM + 1
            IF (IDXM.GE.KI) THEN
              WRITE(*,*) 'ZXT', ZXT,'ZYT',ZYT
              WRITE(*,*) 'indices Isba:',IDXM,'>=',KI
              CALL ABOR1_SFX("MAKE_MASK_TOPD_TO_ISBA: PROBLEM")
            ENDIF
            CALL GET_COORD(ZXT,ZYT,ZX1(IDXM),ZX2(IDXM),ZX3(IDXM),ZX4(IDXM),&
                                   ZY1(IDXM),ZY2(IDXM),ZY3(IDXM),ZY4(IDXM),ZXA,ZYA,ZXB,ZYB)
          ENDDO
        ENDIF
        IF (NLINE(JCAT,IDX)/=0) NMASKT(JCAT,NLINE(JCAT,IDX)) = IDXM
      ENDIF
    ENDDO
  ENDDO
  !
ENDDO
!
YVAR='.mask_topd'
WHERE (NMASKT(:,:)/=NUNDEF)
  ZWRK(:,:)=REAL(NMASKT)
ELSEWHERE
  ZWRK(:,:)=XUNDEF
ENDWHERE
 CALL WRITE_FILE_MAP(ZWRK,YVAR)
!CALL WRITE_FILE_MAP(REAL(NMASKT),YVAR)
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE INIT_4POINTS(KDXM,PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4)
!
USE MODD_COUPLING_TOPD, ONLY: NIMAX, XXI, XYI
USE MODE_GRIDTYPE_IGN
!
INTEGER, INTENT(IN) :: KDXM
REAL, INTENT(OUT) :: PX1, PX2, PX3, PX4
REAL, INTENT(OUT) :: PY1, PY2, PY3, PY4
REAL, DIMENSION(KI)    :: ZDX, ZDY
!
INTEGER :: ILINE, II, IDXN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:INIT_4POINTS',0,ZHOOK_HANDLE)
!
IF (HGRID=='IGN') THEN 
  CALL GET_GRIDTYPE_IGN(PGRID_PAR,PDX=ZDX,PDY=ZDY)
  IDXN=KDXM
  !on va juste retourner les quatre coins de la maille, les XXI et XYI etant les coordonees du centre
  !on se contente de verifier si la maille TOP est dans la maille SURFEX
  !la grille est reguliere dans les deux sens mais pas forcement rectangulaire
  PX1=XXI(IDXN)-ZDX(IDXN)/2.0
  PX2=XXI(IDXN)+ZDX(IDXN)/2.0
  PX3=XXI(IDXN)-ZDX(IDXN)/2.0
  PX4=XXI(IDXN)+ZDX(IDXN)/2.0
  PY1=XYI(IDXN)-ZDY(IDXN)/2.0
  PY2=XYI(IDXN)-ZDY(IDXN)/2.0
  PY3=XYI(IDXN)+ZDY(IDXN)/2.0
  PY4=XYI(IDXN)+ZDY(IDXN)/2.0
ELSE
 ILINE = INT(KDXM/(NIMAX))+1      ! number of the current line
 II    = KDXM-((ILINE-1)*NIMAX)   ! index of point in the line
 IDXN  = (ILINE-1)*(NIMAX+1)+II   ! indice du point dans la grille isba
!
 PX1 = XXI(IDXN)              ! coordonnée X du point courant
 PX2 = XXI(IDXN+1)            ! coordonnée X du point suivant
 PX3 = XXI(IDXN+(NIMAX+1))    ! coordonnée X du point aligné sur la ligne suivante 
 PX4 = XXI(IDXN+1+(NIMAX+1))  ! coordonnée X du point suivant sur la ligne suivante
!
 PY1 = XYI(IDXN)              ! coordonnée Y du point courant
 PY2 = XYI(IDXN+1)            ! coordonnée Y du point suivant
 PY3 = XYI(IDXN+(NIMAX+1))    ! coordonnée Y du point aligné sur la ligne suivante
 PY4 = XYI(IDXN+1+(NIMAX+1))  ! coordonnée Y du point suivant sur la ligne suivante
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:INIT_4POINTS',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_4POINTS
!
SUBROUTINE GET_COORD(PXT,PYT,PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4,&
                     PXA,PYA,PXB,PYB)
!
REAL, INTENT(IN) :: PXT, PYT
REAL, INTENT(IN) :: PX1, PX2, PX3, PX4
REAL, INTENT(IN) :: PY1, PY2, PY3, PY4
REAL, INTENT(OUT) :: PXA, PYA, PXB, PYB
REAL :: ZFA, ZFB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:GET_COORD',0,ZHOOK_HANDLE)
!
IF((PX3-PX1).EQ.0.0) THEN
  PXA = PX1
ELSE
  CALL GET_LINE_PARAM(PX1,PY1,PX3,PY3,ZFA,ZFB)
  PXA = (PYT-ZFB)/ZFA
ENDIF
!
IF ((PX4-PX2).EQ.0.0) THEN
  PXB = PX2
ELSE
  CALL GET_LINE_PARAM(PX2,PY2,PX4,PY4,ZFA,ZFB)
  PXB = (PYT-ZFB)/ZFA
ENDIF
!
IF ((PY2-PY1).EQ.0.0) THEN
  PYA = PY2
ELSE
  CALL GET_LINE_PARAM(PX1,PY1,PX2,PY2,ZFA,ZFB)
  PYA = ZFA*PXT+ZFB
ENDIF
!
IF ((PY4-PY3).EQ.0.0) THEN
  PYB = PY4
ELSE
  CALL GET_LINE_PARAM(PX3,PY3,PX4,PY4,ZFA,ZFB)
  PYB = ZFA*PXT+ZFB
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:GET_COORD',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_COORD
!
SUBROUTINE GET_LINE_PARAM(PX1,PY1,PX2,PY2,PFA,PFB)
!
REAL, INTENT(IN) :: PX1, PX2, PY1, PY2
REAL, INTENT (OUT) :: PFA, PFB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:GET_LINE_PARAM',0,ZHOOK_HANDLE)
!
PFA = (PY2 - PY1) / (PX2 - PX1)  ! slope
PFB = PY1 - PFA * PX1 ! offset
!
IF (LHOOK) CALL DR_HOOK('MAKE_MASK_TOPD_TO_ISBA:GET_LINE_PARAM',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_LINE_PARAM
!
END SUBROUTINE MAKE_MASK_TOPD_TO_ISBA
