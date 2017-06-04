!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_COVER_TEX_ISBA_PAR (DTCO, HALBEDO, OTR_ML, &
                                           KPATCH,KLAYER,HISBA,HPHOTO,PSOILGRID)
!     ##########################
!
!!**** *WRITE_COVER_TEX* writes the ISBA data arrays into a tex file
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
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      B. Decharme    2008 Bug if ZDMAX = XUNDEF
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODE_WRITE_COVER_TEX

USE MODI_CONVERT_COVER_ISBA

USE MODD_TYPE_DATE_SURF
USE MODD_WRITE_COVER_TEX,ONLY : NTEX, CNAME, CLANG, NLINES
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XDAY

USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVEGTYPE
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
!
 CHARACTER(LEN=*), INTENT(IN) :: HALBEDO
LOGICAL, INTENT(IN) :: OTR_ML
!
INTEGER,          INTENT(IN) :: KPATCH! number of patch
INTEGER,          INTENT(IN) :: KLAYER! number of soil layers
 CHARACTER(LEN=*), INTENT(IN) :: HISBA ! type of soil (Force-Restore OR Diffusion)
 CHARACTER(LEN=*), INTENT(IN) :: HPHOTO! type of photosynthesis
REAL, DIMENSION(:),INTENT(IN) :: PSOILGRID ! reference grid for DIF
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: JI,JJ,JIP
!
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_VEGPARAM! vegetation parameters
 CHARACTER(LEN=6)                      :: YSTRING6
 CHARACTER(LEN=24)                     :: YDATA_CV      ! Cv
 CHARACTER(LEN=24)                     :: YDATA_RE25    ! Re25
 CHARACTER(LEN=4)                      :: YDATA_STRESS  ! Stress def.
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_MONTH   ! monthly veg. parameters
 CHARACTER(LEN=6), DIMENSION(10      ) :: YDATA_LAYER   ! soil layer parameters
!
 CHARACTER(LEN=20) :: YFMT  ! fortran format
!
INTEGER           :: JPAGE ! current page when the number of classes
                           ! is too long to stand on one page only
INTEGER           :: JPATCH   ! loop counter
!
!
LOGICAL           :: GLINE ! flag to write an additional horizontal line
!
REAL, DIMENSION(JPCOVER,JPCOVER     ) :: ZCOVER
LOGICAL, DIMENSION(JPCOVER          ) :: GCOVER
REAL, DIMENSION(JPCOVER,KPATCH, 12  ) :: ZVEG, ZLAI, ZZ0VEG, ZEMIS_ECO, ZF2I
REAL, DIMENSION(JPCOVER,KPATCH      ) :: ZRSMIN,ZGAMMA,ZRGL,ZCV,             &
                                           ZALBNIR_VEG,ZALBVIS_VEG,ZALBUV_VEG, &
                                           ZGMES,ZBSLAI,ZLAIMIN,ZSEFOLD,       &
                                           ZH_TREE, ZGC, ZZ0_O_Z0H,            &
                                           ZWRMAX_CF, ZDMAX, ZRE25  
REAL, DIMENSION(JPCOVER,KLAYER,KPATCH):: ZDG, ZROOTFRAC
REAL, DIMENSION(JPCOVER,KPATCH)       :: ZDROOT
REAL, DIMENSION(JPCOVER,KPATCH)       :: ZDG2
INTEGER, DIMENSION(JPCOVER,KPATCH)    :: IWG_LAYER
LOGICAL, DIMENSION(JPCOVER,KPATCH, 12   ) :: GSTRESS
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
!
GLINE=.FALSE.
!
ZCOVER(:,:) = 0.
DO JI=1,JPCOVER
  ZCOVER(JI,JI) = 1.
END DO
!
GCOVER(:) = .TRUE.
!
!ocl scalar
!
DO JJ=1,12
  CALL CONVERT_COVER_ISBA(DTCO, HALBEDO, &
                          HISBA,OTR_ML,3*JJ-1,ZCOVER,GCOVER,HPHOTO, 'NAT',         &
                            PVEG=ZVEG(:,:,JJ), PLAI=ZLAI(:,:,JJ),            &
                            PZ0=ZZ0VEG(:,:,JJ), PEMIS_ECO=ZEMIS_ECO(:,:,JJ), &
                            PF2I=ZF2I(:,:,JJ),OSTRESS=GSTRESS(:,:,JJ)        )  
END DO

 CALL CONVERT_COVER_ISBA(DTCO, HALBEDO, &
                          HISBA,OTR_ML,2,ZCOVER,GCOVER,HPHOTO, 'NAT',            &
                        PRSMIN=ZRSMIN,PGAMMA=ZGAMMA,PWRMAX_CF=ZWRMAX_CF, &
                        PRGL=ZRGL,PCV=ZCV,PSOILGRID=PSOILGRID,           &
                        PDG=ZDG,KWG_LAYER=IWG_LAYER,PDROOT=ZDROOT,       &
                        PDG2=ZDG2,PZ0_O_Z0H=ZZ0_O_Z0H,                   &
                        PALBNIR_VEG=ZALBNIR_VEG,PALBVIS_VEG=ZALBVIS_VEG, &
                        PALBUV_VEG=ZALBUV_VEG,                           &
                        PROOTFRAC=ZROOTFRAC,                             &
                        PGMES=ZGMES,PBSLAI=ZBSLAI,PLAIMIN=ZLAIMIN,       &
                        PSEFOLD=ZSEFOLD,PGC=ZGC,PDMAX=ZDMAX,             &
                        PH_TREE=ZH_TREE,PRE25=ZRE25                       )  
!
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
DO JPATCH=1,KPATCH
!
 IF (       (KPATCH>=2  .AND. JPATCH==1) &
         .OR. (KPATCH>=7  .AND. JPATCH==2) &
         .OR. (KPATCH>=10 .AND. JPATCH==3) ) CYCLE  
!
!
!
 JI=0
 DO 

  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf averaged leaf area index} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf indice foliaire moyenn\'e} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||c|c||c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12&$d_2$&$d_3$&$h$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    IF (ZLAI(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,12
        IF (ZLAI(JI,JPATCH,JJ)==0. .OR. ZLAI(JI,JPATCH,JJ)==XUNDEF) THEN
          YDATA_MONTH(JJ) = ' -  '
        ELSE
          WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZLAI(JI,JPATCH,JJ),1),'.',DEC(ZLAI(JI,JPATCH,JJ),1),')'
          WRITE(YSTRING6, FMT=YFMT) ZLAI(JI,JPATCH,JJ)
          YDATA_MONTH(JJ) = YSTRING6
        END IF
      END DO

      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDG(JI,2,JPATCH),1),'.',DEC(ZDG(JI,2,JPATCH),1),')'
      WRITE(YSTRING6, FMT=YFMT) ZDG(JI,2,JPATCH)
      YDATA_VEGPARAM(1) = YSTRING6
      IF (KLAYER>=3) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZDG(JI,3,JPATCH),1),'.',DEC(ZDG(JI,3,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZDG(JI,3,JPATCH)
        YDATA_VEGPARAM(2) = YSTRING6
      ELSE
        YDATA_VEGPARAM(2) = ' -    '
      END IF

      IF (ZH_TREE(JI,JPATCH)==XUNDEF .OR. ZH_TREE(JI,JPATCH)==0.) THEN
        YDATA_VEGPARAM(3) = ' -    '
      ELSE
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZH_TREE(JI,JPATCH)),'.',DEC(ZH_TREE(JI,JPATCH)),')'
        WRITE(YSTRING6, FMT=YFMT) ZH_TREE(JI,JPATCH)
        YDATA_VEGPARAM(3) = YSTRING6
      END IF

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' & ',YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' & ',YDATA_VEGPARAM(3),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT  
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf ground layer depth} (from surface, patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf profondeur des couche de sol} (depuis la surface, partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    YDATA_LAYER(:) = ' -  '
    IF (ZDG(JI,1,JPATCH)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,MIN(KLAYER,10)
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZDG(JI,JJ,JPATCH),1),'.',DEC(ZDG(JI,JJ,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZDG(JI,JJ,JPATCH)
        YDATA_LAYER(JJ) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_LAYER(1), ' & ',YDATA_LAYER(2), ' & ', &
            YDATA_LAYER(3),' & ',YDATA_LAYER(4), ' & ',YDATA_LAYER(5), ' & ', &
            YDATA_LAYER(6),' & ',YDATA_LAYER(7), ' & ',YDATA_LAYER(8), ' & ', &
            YDATA_LAYER(9),' & ',YDATA_LAYER(10),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
IF (HISBA=='DIF') THEN
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT  

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf cumulative root fraction} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf fraction de racines cumul\'ee} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    YDATA_LAYER(:) = ' -  '
    IF (ZROOTFRAC(JI,1,JPATCH)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,MIN(KLAYER,10)
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZROOTFRAC(JI,JJ,JPATCH),1),'.',DEC(ZROOTFRAC(JI,JJ,JPATCH),1),')'
        WRITE(YSTRING6, FMT=YFMT) ZROOTFRAC(JI,JJ,JPATCH)
        YDATA_LAYER(JJ) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_LAYER(1), ' & ',YDATA_LAYER(2), ' & ', &
            YDATA_LAYER(3),' & ',YDATA_LAYER(4), ' & ',YDATA_LAYER(5), ' & ', &
            YDATA_LAYER(6),' & ',YDATA_LAYER(7), ' & ',YDATA_LAYER(8), ' & ', &
            YDATA_LAYER(9),' & ',YDATA_LAYER(10),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'

ENDDO
END IF
!
!-------------------------------------------------------------------------------
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf vegetation fraction (over natural or agricultural areas)} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf fraction de v\'eg\'etation (sur les surfaces naturelles ou cultiv\'ees)} (partition ", &
  JPATCH,'/',KPATCH,') \\'  
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    IF (ZLAI(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,12
        IF (ZVEG(JI,JPATCH,JJ)==0.) THEN
          YDATA_MONTH(JJ) = ' -  '
        ELSE
          WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZVEG(JI,JPATCH,JJ),2),'.',DEC(ZVEG(JI,JPATCH,JJ),2),')'
          WRITE(YSTRING6, FMT=YFMT) ZVEG(JI,JPATCH,JJ)

          YDATA_MONTH(JJ) = YSTRING6
        END IF
      END DO

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf surface roughness length for momentum: $z_{0}$} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf longueur de rugosit\'e de la surface (qdm): $z_{0}$} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1

    IF (ZLAI(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,12
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZZ0VEG(JI,JPATCH,JJ)),'.',DEC(ZZ0VEG(JI,JPATCH,JJ)),')'
        WRITE(YSTRING6, FMT=YFMT) ZZ0VEG(JI,JPATCH,JJ)
        YDATA_MONTH(JJ) = YSTRING6
      END DO

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
   END DO

  WRITE(NTEX,*) '\end{tabular}'

!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf emissivity of natural continental surfaces} (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf \'emissivit\'e des surfaces continentales naturelles} (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    IF (ZLAI(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      DO JJ=1,12
        IF (ZEMIS_ECO(JI,JPATCH,JJ)==1.) THEN
          YDATA_MONTH(JJ) = ' 1.  '
        ELSE
          WRITE(YSTRING6, FMT='(F3.2)') ZEMIS_ECO(JI,JPATCH,JJ)
          YDATA_MONTH(JJ) = YSTRING6
        END IF
      END DO

      WRITE(NTEX, FMT=*) &
          JI,' & ',CNAME(JI),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
            YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
            YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
            YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
            YDATA_MONTH(12),' \\'  

      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf other vegetation parameters} (1) (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf autres param\`etres de v\'eg\'etation} (1) (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) &
    '&&$\alpha_{nir}$&$\alpha_{vis}$&$\alpha_{UV}$&$r_{s_{min}}$&$\gamma$&$rgl$&$C_{w_{r_{max}}}$&$z_0$/$z_{0_h}$&$C_v$\\'  
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1
    IF (ZLAI(JI,JPATCH,1)>0. .AND. ZVEG(JI,JPATCH,1)>0. .AND. ZVEG(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      WRITE(YSTRING6, FMT='(F3.2)') ZALBNIR_VEG(JI,JPATCH)
      YDATA_VEGPARAM(4) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') ZALBVIS_VEG(JI,JPATCH)
      YDATA_VEGPARAM(5) = YSTRING6
      WRITE(YSTRING6, FMT='(F3.2)') ZALBUV_VEG (JI,JPATCH)
      YDATA_VEGPARAM(6) = YSTRING6
      ZRSMIN(JI,JPATCH) = NINT(ZRSMIN(JI,JPATCH))
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZRSMIN(JI,JPATCH)),'.',DEC(ZRSMIN(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZRSMIN(JI,JPATCH)
      YDATA_VEGPARAM(7) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZGAMMA(JI,JPATCH)),'.',DEC(ZGAMMA(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZGAMMA(JI,JPATCH)
      YDATA_VEGPARAM(8) = YSTRING6
      ZRGL(JI,JPATCH) = NINT(ZRGL(JI,JPATCH))
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZRGL(JI,JPATCH)),'.',DEC(ZRGL(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZRGL(JI,JPATCH)
      YDATA_VEGPARAM(9) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZWRMAX_CF(JI,JPATCH)),'.',DEC(ZWRMAX_CF(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZWRMAX_CF(JI,JPATCH)
      YDATA_VEGPARAM(10) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZZ0_O_Z0H(JI,JPATCH)),'.',DEC(ZZ0_O_Z0H(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZZ0_O_Z0H(JI,JPATCH)
      YDATA_VEGPARAM(11) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000. *ZCV(JI,JPATCH)),'.',DEC(10000. *ZCV(JI,JPATCH)),',A10)'
      WRITE(YDATA_CV, FMT=YFMT) 10000. *ZCV(JI,JPATCH),' $10^{-4}$'

      WRITE(NTEX, FMT=*) &
             JI,' & ',CNAME(JI),' & ', &
                     YDATA_VEGPARAM(4),' & ',YDATA_VEGPARAM(5),' & ', &
                     YDATA_VEGPARAM(6),' & ',YDATA_VEGPARAM(7),' & ', &
                     YDATA_VEGPARAM(8),' & ',YDATA_VEGPARAM(9),' & ', &
                     YDATA_VEGPARAM(10),' & ',YDATA_VEGPARAM(11),' & ', &
                     YDATA_CV,' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
ENDDO
!-------------------------------------------------------------------------------
!
!
JI=0
DO 
  IF (JI==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf other vegetation parameters} (2) (patch ',JPATCH,'/',KPATCH,') \\'
  ELSE
    WRITE(NTEX,*) "{\bf autres param\`etres de v\'eg\'etation} (2) (partition ",JPATCH,'/',KPATCH,') \\'
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '&&$gm$&$B/lai$&$lai_{_{m}}$&$e_{_{fold}}$&$G_c$&$D_{max}$&$f_{2i}$&stress&Re$_{25}$\\'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  JIP=0
  DO
    IF (JI==JPCOVER) EXIT
    JI=JI+1

    IF (ZLAI(JI,JPATCH,1)>0. .AND. ZVEG(JI,JPATCH,1)>0. .AND. ZVEG(JI,JPATCH,1)/=XUNDEF) THEN
      JIP=JIP+1
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZGMES(JI,JPATCH)),'.',DEC(ZGMES(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZGMES(JI,JPATCH)
      YDATA_VEGPARAM(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZBSLAI(JI,JPATCH)),'.',DEC(ZBSLAI(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZBSLAI(JI,JPATCH)
      YDATA_VEGPARAM(2) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZLAIMIN(JI,JPATCH)),'.',DEC(ZLAIMIN(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZLAIMIN(JI,JPATCH)
      YDATA_VEGPARAM(3) = YSTRING6
      ZSEFOLD(JI,JPATCH) = NINT(ZSEFOLD(JI,JPATCH)/XDAY)
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(ZSEFOLD(JI,JPATCH)),'.',DEC(ZSEFOLD(JI,JPATCH)),')'
      WRITE(YSTRING6, FMT=YFMT) ZSEFOLD(JI,JPATCH)
      YDATA_VEGPARAM(4) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000. *ZGC(JI,JPATCH)),'.',DEC(10000. *ZGC(JI,JPATCH)),',A10)'
      WRITE(YDATA_CV, FMT=YFMT) 10000. *ZGC(JI,JPATCH),' $10^{-4}$'
      IF (ZDMAX(JI,JPATCH) /= XUNDEF)THEN
         WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZDMAX(JI,JPATCH)),'.',DEC(ZDMAX(JI,JPATCH)),')'
         WRITE(YSTRING6, FMT=YFMT) ZDMAX(JI,JPATCH)
      ELSE
         YSTRING6 ='  -   '
      ENDIF
      YDATA_VEGPARAM(5) = YSTRING6
      IF (GSTRESS(JI,JPATCH,1)) THEN
        YDATA_STRESS='def.'
      ELSE
        YDATA_STRESS='off.'
      END IF
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NBT(ZF2I(JI,JPATCH,1)),'.',DEC(ZF2I(JI,JPATCH,1)),')'
      WRITE(YSTRING6, FMT=YFMT) ZF2I(JI,JPATCH,1)
      YDATA_VEGPARAM(6) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A5)') '(F',NB(10000000. *ZRE25(JI,JPATCH)),'.',DEC(10000000. *ZRE25(JI,JPATCH)),',A10)'
      WRITE(YDATA_RE25, FMT=YFMT) 10000000. *ZRE25(JI,JPATCH),' $10^{-7}$'
      WRITE(NTEX, FMT=*) &
             JI,' & ',CNAME(JI),' & ', &
                     YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' & ', &
                     YDATA_VEGPARAM(3),' & ',YDATA_VEGPARAM(4),' & ', &
                     YDATA_CV,         ' & ',YDATA_VEGPARAM(5),' & ', &
                     YDATA_VEGPARAM(6),' & ',                         &
                     YDATA_STRESS ,' & ',YDATA_RE25,' \\'  
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,JI)
    IF (JIP==NLINES) EXIT
  END DO
  WRITE(NTEX,*) '\end{tabular}'
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
  
 END DO
!
END DO 
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE WRITE_COVER_TEX_ISBA_PAR
