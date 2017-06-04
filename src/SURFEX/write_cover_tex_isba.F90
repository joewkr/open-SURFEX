!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_COVER_TEX_ISBA(KPATCH,KLAYER,HISBA)
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
!!    R. Alkama    05/2012 : from 12 to 19 vegtype 
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODE_WRITE_COVER_TEX
!
USE MODD_WRITE_COVER_TEX,ONLY : NTEX, CNAME, CLANG, NLINES
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_NATURE,                            &
                                  XDATA_VEGTYPE, XDATA_H_TREE, XDATA_LAI,  &
                                  XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH  
                                  
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVEGTYPE, NVT_NO, NVT_ROCK,      & 
                                  NVT_SNOW, NVT_TEBD, NVT_BONE, NVT_TRBE, &
                                  NVT_C3, NVT_C4, NVT_IRR, NVT_GRAS,      &
                                  NVT_TROG,NVT_PARK, NVT_TRBD, NVT_TEBE,  &
                                  NVT_TENE, NVT_BOBD, NVT_BOND, NVT_BOGR, &
                                  NVT_SHRB, NVT_C3W, NVT_C3S, NVT_FLTR,   &
                                  NVT_FLGR 
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG, XEVERG_RSMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,          INTENT(IN) :: KPATCH! number of patch
INTEGER,          INTENT(IN) :: KLAYER! number of soil layers
 CHARACTER(LEN=*), INTENT(IN) :: HISBA ! type of soil (Force-Restore OR Diffusion)
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: I,J
!
 CHARACTER(LEN=5), DIMENSION(NVEGTYPE) :: YDATA_VEGTYPE ! vegetation type
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_VEGPARAM! vegetation parameters
 CHARACTER(LEN=6)                      :: YSTRING6
 CHARACTER(LEN=6), DIMENSION(12      ) :: YDATA_MONTH   ! monthly veg. parameters
!
 CHARACTER(LEN=40),DIMENSION(2)        :: YPATCH        ! titles for each vegtype
!
 CHARACTER(LEN=20) :: YFMT  ! fortran format
!
INTEGER           :: JPAGE ! current page when the number of classes
                           ! is too long to stand on one page only
INTEGER           :: IP    ! number of cover types on the current page
INTEGER           :: JVEGTYPE ! loop counter
!
!
LOGICAL           :: GLINE ! flag to write an additional horizontal line
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA',0,ZHOOK_HANDLE)
IF (NTEX==0 .AND. LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA',1,ZHOOK_HANDLE)
IF (NTEX==0) RETURN
GLINE=.FALSE.
!
!
I=0

DO 

  IF (I==JPCOVER) EXIT

  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf vegetation type and main ISBA parameters$^\star$}\\'
  ELSE
    WRITE(NTEX,*) "{\bf type de v\'eg\'etation et param\`etres principaux d'ISBA $^\star$}\\"
  END IF
  WRITE(NTEX,*) '\medskip\'
  WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c||}'
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
!
!* WARNING: check the cover type order in ini_data_cover routine
!
  IF (CLANG=='EN') THEN
    WRITE(NTEX, '("&&bare&rocks&snow&broad.d&needl.e&ever-&C3&C4&irr.&grass&grass&irr. ")', ADVANCE='NO') 
    WRITE(NTEX, '("broad.d&broad.e&needl.e&broad.d&needl.d&grass&shrubs\\")')
    WRITE(NTEX, '("&&land&&&temp.&boreal&green&crops&crops&crops&C3&C4&grass&trop. ")', ADVANCE='NO')
    WRITE(NTEX, '("temp.&temp.&boreal&boreal&boreal&broad.\\")')
  ELSE
    WRITE(NTEX, '("&&sol&roc&neige&feuillu.d&coni.p&persis-&C3&C4&cult.&prairie&prairie ")', ADVANCE='NO')
    WRITE(NTEX, '("pelouse&feuillu.d&feuillu.p&aigui.p&feuillu.d&aigui.d&prairie&arbuste\\")')
    WRITE(NTEX, '("&&nu&&&temp.&boreale&tants&cult.&cult.&irr.&&tropicale&&tropi.&temp. ")', ADVANCE='NO')
    WRITE(NTEX, '("temp.&boreale&boreale&boreale\\")')
  END IF
  WRITE(NTEX,*) '\hline'
  WRITE(NTEX,*) '\hline'
  IP=0
  DO 

    IF (I==JPCOVER) EXIT 
    I=I+1
    IF (XDATA_NATURE(I)>0.) THEN
      IP=IP+1
      DO J=1,NVEGTYPE
        IF (XDATA_VEGTYPE(I,J)==0.) THEN
          YDATA_VEGTYPE(J) = '    '
        ELSE
          WRITE(YFMT,FMT='(A2,I1,A1,I1,A1)') '(F',NB(XDATA_VEGTYPE(I,J)),'.',DEC(XDATA_VEGTYPE(I,J)),')'
          WRITE(YSTRING6, FMT=YFMT) XDATA_VEGTYPE(I,J)
          YDATA_VEGTYPE(J) = YSTRING6
        END IF
      END DO

      WRITE(NTEX, FMT=*) &
          I,' & ',CNAME(I ),' & ',YDATA_VEGTYPE(1 ),' & ',YDATA_VEGTYPE(2 ),' & ',&
          YDATA_VEGTYPE(3 ),' & ',YDATA_VEGTYPE(4 ),' & ',YDATA_VEGTYPE(5 ),' & ',&
          YDATA_VEGTYPE(6 ),' & ',YDATA_VEGTYPE(7 ),' & ',YDATA_VEGTYPE(8 ),' & ',&
          YDATA_VEGTYPE(9 ),' & ',YDATA_VEGTYPE(10),' & ',YDATA_VEGTYPE(11),' & ',&
          YDATA_VEGTYPE(12),' & ',YDATA_VEGTYPE(13),' & ',YDATA_VEGTYPE(14),' & ',&
          YDATA_VEGTYPE(15),' & ',YDATA_VEGTYPE(16),' & ',YDATA_VEGTYPE(17),' & ',&          
          YDATA_VEGTYPE(18),' & ',YDATA_VEGTYPE(19),' \\'  
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
END DO
!
!-------------------------------------------------------------------------------
!
  IF (CLANG=='EN') THEN
    WRITE(NTEX,*) '{\bf computation of other ISBA parameters}\'
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "Vegetation type, Leaf area index and height of vegetation (for trees)"
    WRITE(NTEX,*) "are used in these computations.\"
    WRITE(NTEX,*) '\bigskip\'
    WRITE(NTEX,*) '\underline{vegetation fraction}\'
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) '\begin{tabular}{rll}'
    WRITE(NTEX,*) 'veg = & $1-e^{-0.6 lai}$ & for C3, C4 and irrigated crops\\'
    WRITE(NTEX,*) 'veg = & 0.95 & for boreale, temperate, tropical and irrigated grass\\'
    WRITE(NTEX,*) 'veg = & 0.95 & for broadleaf and coniferous trees\\'
    IF(XEVERG_VEG==0.99)THEN
      WRITE(NTEX,*) 'veg = & 0.99 & for evergreen broadleaf trees\\'
    ELSE
      WRITE(NTEX,*) 'veg = & 1.00 & for evergreen broadleaf trees\\'
    ENDIF
    WRITE(NTEX,*) 'veg = & 0.   & for bare soil, snow and rocks'
    WRITE(NTEX,*) '\end{tabular}'
    WRITE(NTEX,*) '\smallskip\'
    WRITE(NTEX,*) 'When averaging is needed, it is performed arithmetically'
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) '\underline{roughness length for momentum}\'
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) 'The height of the vegetation (or obstacles over bare soils) is computed as:\'
    WRITE(NTEX,*) '\begin{tabular}{rll}'
    WRITE(NTEX,*) '$h_{veg}$ = & min $(1. , h_{allen})$ & for C3 crops\\'
    WRITE(NTEX,*) '$h_{veg}$ = & min $(2.5, h_{allen})$ & for C4 and irrigated crops\\'
    WRITE(NTEX,*) '$h_{veg}$ = & $h$ & for broadleaf, coniferous and evergreen  trees\\'
    WRITE(NTEX,*) '$h_{veg}$ = & $lai/6$ & for grassland, tropical grassland and irrigated grass\\'
    WRITE(NTEX,*) '$h_{veg}$ = & 0.1  m   & for bare soil\\'
    WRITE(NTEX,*) '$h_{veg}$ = & 1.   m   & for rocks\\'
    WRITE(NTEX,*) '$h_{veg}$ = & 0.01 m   & for permanent snow and ice'
    WRITE(NTEX,*) '\end{tabular}'
    WRITE(NTEX,*) '\smallskip\'
    WRITE(NTEX,*) 'where $h_{allen} = e^{(lai-3.5)/1.3}$\'
    WRITE(NTEX,*) 'The roughness length is deduced: $z_{0} = 0.13 h_{veg}$\' 
    WRITE(NTEX,*) 'When averaging is needed, it is performed according to the'
    WRITE(NTEX,*) '$1/{\rm ln}^2(\frac{z_{0}}{10})$ quantities.'
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) '\underline{emissivity}\'
    WRITE(NTEX,*) "\medskip\"
    WRITE(NTEX,*) "Emissivity is equal to 0.97 on the vegetated part (veg), "
    WRITE(NTEX,*) "to 0.94 on bare soil and rocks, and to 1. on snow."
    WRITE(NTEX,*) "Averaging is linear."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) '\underline{cumulative root fraction}\'
    WRITE(NTEX,*) "\smallskip\"
    WRITE(NTEX,*) "\begin{displaymath}"
    WRITE(NTEX,*) "F_{root} =(1-\delta) \left[ \frac{1-A^{100\times z}}"
    WRITE(NTEX,*) "{1 - A^{100\times d_2}} \right] + \delta \frac{z}{d_2}"
    WRITE(NTEX,*) "\end{displaymath}"
    WRITE(NTEX,*) "where $d_2$ is root depth, given for each 215 ecosystem (meters),"
    WRITE(NTEX,*) "$z$ is depth (meters, "
    WRITE(NTEX,*) "positive downwards), $\delta=0.05$ and $A$ is the extinction"
    WRITE(NTEX,*) "coefficient after Jackson (1996) (see below).\"
    WRITE(NTEX,*) "Averaging is linear."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) '\underline{other vegetation parameters}\'
    WRITE(NTEX,*) "\medskip\"
    WRITE(NTEX,*) 'Other vegetation parameters are computed from the vegetation types.'
    WRITE(NTEX,*) "The 'bare soil', 'rocks' and 'snow' vegetation types are not pertinent."
    WRITE(NTEX,*) 'When averaging is needed,'
    WRITE(NTEX,*) 'it is performed linearly, except for the $C_v$ parameter, where it is harmonic.'
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) '\begin{tabular}{||l||c|c|c|c|c|c|c|c|c|c||}'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '&$\alpha_{nir}$&$\alpha_{vis}$&$r_{s_{min}}$&$\gamma$&$rgl$&$gm$&$B/lai$&$e_{_{fold}}$&$C_v$&$A$\\'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) 'brodleaf trees      & .25 & .05 & 150 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$ & 0.966 \\'
    WRITE(NTEX,*) 'coniferous trees    & .15 & .05 & 150 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$ & 0.943 \\'
    IF(XEVERG_RSMIN==250.)THEN
      WRITE(NTEX,*) 'evergreen broadleaf trees & .21 & .05 & 250 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$ & 0.962 \\'
    ELSE
      WRITE(NTEX,*) 'evergreen broadleaf trees & .21 & .05 & 175 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$ & 0.962 \\'
    ENDIF
    WRITE(NTEX,*) 'C3 crops   & .30 & .10 &  40 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$& 0.961 \\'
    WRITE(NTEX,*) 'C4 crops   & .30 & .10 &  120 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$ & 0.972 \\'
    WRITE(NTEX,*) 'irr. crops & .30 & .10 &  120 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$ & 0.961 \\'
    WRITE(NTEX,*) 'grassland  & .30 & .10 &  40 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$& 0.943 \\'
    WRITE(NTEX,*) 'trop. grass & .30 & .10 &  120 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$ & 0.972 \\'
    WRITE(NTEX,*) 'irr. grass & .30 & .10 &  40 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$& 0.943 \\'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '\end{tabular}'
  ELSE
    WRITE(NTEX,*) "{\bf calcul des autres param\`etres d'ISBA}\"
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "Les types de v\'eg\'etation, l'indice foliaire et la hauteur de la v\'eg\'etation (pour les arbres)"
    WRITE(NTEX,*) "sont utilis\'es dans ces calculs.\"
    WRITE(NTEX,*) '\bigskip\'
    WRITE(NTEX,*) "\underline{fraction de v\'eg\'etation}\"
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "\begin{tabular}{rll}"
    WRITE(NTEX,*) "veg = & $1-e^{-0.6 lai}$ & pour les cultures (C3, C4 et irrigu\'ees)\\"
    WRITE(NTEX,*) "veg = & 0.95 & pour les arbres (feuillus, conif\`eres)\\"
    IF(XEVERG_VEG==0.99)THEN
      WRITE(NTEX,*) "veg = & 0.99 & pour les feuillus persistants\\"
    ELSE
      WRITE(NTEX,*) "veg = & 1.00 & pour les feuillus persistants\\"
    ENDIF
    WRITE(NTEX,*) "veg = & 0.95 & pour la prairie, la prairie tropicale et la pelouse irrigu\'ee\\"
    WRITE(NTEX,*) "veg = & 0.   & pour les sols nus, la neige \'eternelle et les rochers"
    WRITE(NTEX,*) "\end{tabular}"
    WRITE(NTEX,*) "\smallskip\"
    WRITE(NTEX,*) "Quand une moyenne est n\'ecessaire, elle est arithm\'etique."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) "\underline{longueur de rugosit\'e pour la quantit\'e de mouvement}\"
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "La hauteur de la v\'eg\'etation (ou des obstacles sur les sols nus) est calcul\'ee ainsi:\"
    WRITE(NTEX,*) "\begin{tabular}{rll}"
    WRITE(NTEX,*) "$h_{veg}$ = & min $(1. , h_{allen})$ & pour les cultures (C3)\\"
    WRITE(NTEX,*) "$h_{veg}$ = & min $(2.5, h_{allen})$ & pour les cultures (C4 et irrigu\'ees)\\"
    WRITE(NTEX,*) "$h_{veg}$ = & $h$ & pour les arbres (feuillus, conif\`eres et persistants)\\"
    WRITE(NTEX,*) "$h_{veg}$ = & $lai/6$ & pour la prairie, la prairie tropicale et la pelouse irrigu\'ee\\"
    WRITE(NTEX,*) "$h_{veg}$ = & 0.1  m   & pour le sol nu\\"
    WRITE(NTEX,*) "$h_{veg}$ = & 1.   m   & pour les rochers\\"
    WRITE(NTEX,*) "$h_{veg}$ = & 0.01 m   & pour les neiges \'eternelles et glaciers"
    WRITE(NTEX,*) "\end{tabular}"
    WRITE(NTEX,*) "\smallskip\"
    WRITE(NTEX,*) "avec $h_{allen} = e^{(lai-3.5)/1.3}$\"
    WRITE(NTEX,*) "La longueur de rugosit\'e de la surface en est d\'eduite:"
    WRITE(NTEX,*) "$z_{0} = 0.13 h_{veg}$\"
    WRITE(NTEX,*) "Quand une moyenne est n\'ecessaire, elle s'effectue sur les"
    WRITE(NTEX,*) "$1/{\rm ln}^2(\frac{z_{0}}{10})$."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) "\underline{\'emissivit\'e}\"
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "L'\'emissivit\'e est \'egale a 0.97 sur la partie v\'eg\'etale (veg), "
    WRITE(NTEX,*) "0.94 sur la partie sol nu et rochers, et 1 sur la neige."
    WRITE(NTEX,*) "Les moyennes sont lin\'eaires."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) "\underline{fraction cumul\'ee de racines}\"
    WRITE(NTEX,*) "\medskip\"
    WRITE(NTEX,*) "\begin{displaymath}"
    WRITE(NTEX,*) "F_{root} =(1-\delta) \left[ \frac{1-A^{100\times z}}"
    WRITE(NTEX,*) "{1 - A^{100\times d}} \right] + \delta \frac{z}{d_2}"
    WRITE(NTEX,*) "\end{displaymath}"
    WRITE(NTEX,*) "o\`u $d_2$ est la profondeur racinaire, "
    WRITE(NTEX,*) "donn\'ee pour chacun des 215 ecosyst\`emes (m\`etres),"
    WRITE(NTEX,*) "$z$ est la profondeur (m\`etres, "
    WRITE(NTEX,*) "positive vers le bas), $\delta=0.05$ et $A$ est le"
    WRITE(NTEX,*) "coefficient d'extinction d'apr\`es Jackson (1996) (voir ci-dessous).\"
    WRITE(NTEX,*) "Les moyennes sont lin\'eaires."
    WRITE(NTEX,*) "\bigskip\"
    WRITE(NTEX,*) "\underline{autres param\`etres de v\'eg\'etation}\"
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) "Les autres param\`etres de v\'eg\'etation sont d\'eduits des types de v\'eg\'etation."
    WRITE(NTEX,*) "Les types 'sol nu', 'rochers' et 'neige' ne sont pas pertinents."
    WRITE(NTEX,*) "Quand une moyenne est n\'ecessaire, elle est lin\'eaire, sauf sur le param\`etre"
    WRITE(NTEX,*) "$C_v$, o\`u elle est harmonique."
    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) '\begin{tabular}{||l||c|c|c|c|c|c|c|c|c|c||}'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '&$\alpha_{nir}$&$\alpha_{vis}$&$r_{s_{min}}$&$\gamma$&$rgl$&$gm$&$B/lai$&$e_{_{fold}}$&$C_v$&$A$\\'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) 'feuillus     & .25 & .05 & 150 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$& 0.966 \\'
    WRITE(NTEX,*) 'conif\`eres & .15 & .05 & 150 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$& 0.943 \\'
    IF(XEVERG_RSMIN==250.)THEN
      WRITE(NTEX,*) 'feuillus persistans & .21 & .05 & 250 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$& 0.962 \\'
    ELSE
      WRITE(NTEX,*) 'feuillus persistans & .21 & .05 & 175 & .04 & 30 & .001 & .25 & 365. & 1. $10^{-5}$& 0.962 \\'
    ENDIF
    WRITE(NTEX,*) 'cultures C3  & .30 & .10 &  40 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$& 0.961 \\'
    WRITE(NTEX,*) 'cultures C4  & .30 & .10 &  120 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$& 0.972 \\'
    WRITE(NTEX,*) 'cultures irr.  & .30 & .10 & 120 & 0. & 100 & .003 & .06 &  60. & 2. $10^{-5}$& 0.961 \\'
    WRITE(NTEX,*) 'prairies     & .30 & .10 &  40 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$&0.943 \\'
    WRITE(NTEX,*) 'prairies trop. & .30 & .10 &  120 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$& 0.972 \\'
    WRITE(NTEX,*) 'pelouse      & .30 & .10 &  40 & 0. & 100 & .020 & .36 &  90. & 2. $10^{-5}$& 0.943 \\'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '\end{tabular}'
  END IF
!
!-------------------------------------------------------------------------------
!
  WRITE(NTEX,*) '\clearpage'
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
DO JVEGTYPE=1,NVEGTYPE
 !
 I=0
 !
 DO
  !
  IF (I==JPCOVER) EXIT
  !
  IF (JVEGTYPE==NVT_ROCK) YPATCH = (/ 'rocks                        ',   &
                                        'les rochers                  ' /)  
  IF (JVEGTYPE==NVT_SNOW) THEN
     YPATCH(1) = 'permanent snow and ice       '
     YPATCH(2) = "les neiges \'eternelles     "
  END IF
  IF (JVEGTYPE==NVT_NO  ) YPATCH = (/ 'bare soil                    ',   &
                                        'le sol nu                    ' /)  
  IF (JVEGTYPE==NVT_GRAS) YPATCH = (/ 'grasslands                   ',   &
                                        'les prairies                 ' /)
  IF (JVEGTYPE==NVT_BOGR) YPATCH = (/ 'tundra and boreal grass      ',   &
                                        'les prairies boreale         ' /)
  IF (JVEGTYPE==NVT_TROG) YPATCH = (/ 'tropical grasslands          ',   &
                                        'les prairies tropicales      ' /)  
  IF (JVEGTYPE==NVT_PARK) THEN
     YPATCH(1) = 'irrigated grass              '
     YPATCH(2) = "les pelouses irrigu\'ees    "
  END IF
  IF (JVEGTYPE==NVT_FLTR) THEN
     YPATCH(1) = 'flooded trees             '
     YPATCH(2) = "les arbres inondes    "
  END IF
  IF (JVEGTYPE==NVT_FLGR) THEN
     YPATCH(1) = 'flooded grassland              '
     YPATCH(2) = "les prairies inondees    "
  END IF
  IF (JVEGTYPE==NVT_C3)   YPATCH = (/ 'C3 crops                     ',   &
                                        'les cultures C3              ' /)  
  IF (JVEGTYPE==NVT_C3W)  YPATCH = (/ 'C3W crops                    ',   &
                                        'les cultures C3 dhiver       ' /)  
  IF (JVEGTYPE==NVT_C3S)  YPATCH = (/ 'C3S crops                    ',   &
                                        'les cultures C3 dete         ' /)  
  IF (JVEGTYPE==NVT_C4)   YPATCH = (/ 'C4 crops                     ',   &
                                        'les cultures C4              ' /)  
  IF (JVEGTYPE==NVT_IRR ) THEN
    YPATCH(1) = 'irrigated crops              '
    YPATCH(2) = "les cultures irrigu\'ees    "
  END IF
  IF (JVEGTYPE==NVT_TEBD) YPATCH = (/ 'temperate broadleaf deciduous',   &
                                      'les feuillus decidus tempere ' /)
  IF (JVEGTYPE==NVT_BONE) YPATCH = (/ 'boreal needleleaf evergreen  ',   &
                                      'coniferes boreale persistant ' /)
  IF (JVEGTYPE==NVT_TRBE) THEN
    YPATCH(1) = 'equatorial evergreen forest  '
    YPATCH(2) = "les for\^ets \'equatoriales"
  END IF
  IF (JVEGTYPE==NVT_TRBD) YPATCH = (/ 'tropical broadleaf deciduous ',   &
                                      'feuillus decidus tropical    ' /)
  IF (JVEGTYPE==NVT_TEBE) YPATCH = (/ 'temperate broadleaf evergreen',   &
                                      'feuillus tempere persistant  ' /)
  IF (JVEGTYPE==NVT_TENE) YPATCH = (/ 'temperate needle. evergreen  ',   &
                                      'coniferes tempere persistant ' /)
  IF (JVEGTYPE==NVT_BOBD) YPATCH = (/ 'boreal broadleaf deciduous   ',   &
                                      'coniferes decidus boreale    ' /)
  IF (JVEGTYPE==NVT_BOND) YPATCH = (/ 'boreal needleleaf deciduous  ',   &
                                      'coniferes decidus boreale    ' /)
  IF (JVEGTYPE==NVT_SHRB) YPATCH = (/ 'broadleaf shrub              ',   &
                                      'arbustes feuillus            ' /)
  !
  IF (JVEGTYPE==NVT_ROCK .OR. JVEGTYPE==NVT_SNOW .OR. JVEGTYPE==NVT_NO) THEN
    IF (CLANG=='EN') THEN
      WRITE(NTEX,*) '{\bf Ground depths for : ',YPATCH(1),'}\\'
    ELSE
      WRITE(NTEX,*) "{\bf Profondeurs de sol pour : ",YPATCH(2),'}\\'
    END IF

    WRITE(NTEX,*) '\medskip\'
    WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c||}'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '&&$d_2$&$d_3$\\'
    WRITE(NTEX,*) '\hline'
    WRITE(NTEX,*) '\hline'
  ELSE
    IF (CLANG=='EN') THEN
      WRITE(NTEX,*) '{\bf Leaf Area Index and ground depths for : ',YPATCH(1),'}\\'
    ELSE
      WRITE(NTEX,*) "{\bf Indice foliaire et profondeurs de sol pour : ",YPATCH(2),'}\\'
    END IF
    !
    IF (JVEGTYPE==NVT_TEBD .OR. JVEGTYPE==NVT_BONE .OR. JVEGTYPE==NVT_TRBE .OR. &
        JVEGTYPE==NVT_TRBD .OR. JVEGTYPE==NVT_TEBE .OR. JVEGTYPE==NVT_TENE .OR. &
        JVEGTYPE==NVT_BOBD .OR. JVEGTYPE==NVT_BOND .OR. JVEGTYPE==NVT_SHRB ) THEN
      WRITE(NTEX,*) '\medskip\'
      WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||c|c||c||}'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12&$d_2$&$d_3$&$h$\\'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '\hline'
    ELSE
      WRITE(NTEX,*) '\medskip\'
      WRITE(NTEX,*) '\begin{tabular}{||r|l||c|c|c|c|c|c|c|c|c|c|c|c||c|c||}'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '&&01&02&03&04&05&06&07&08&09&10&11&12&$d_2$&$d_3$\\'
      WRITE(NTEX,*) '\hline'
      WRITE(NTEX,*) '\hline'
    END IF
  END IF

  IP=0
  DO
    IF (I==JPCOVER) EXIT
    I=I +1
    !
    IF (XDATA_VEGTYPE(I,JVEGTYPE)>0.) THEN
      IP=IP+1
      !
      DO J=1,12
        IF (XDATA_LAI(I,J,JVEGTYPE)==0.) THEN
          YDATA_MONTH(J) = ' -  '
        ELSE
          WRITE(YFMT,FMT='(A2,I1,A1,I1,A1)') '(F',NB(XDATA_LAI(I,3*J-1,JVEGTYPE)),'.',DEC(XDATA_LAI(I,3*J-1,JVEGTYPE)),')'
          WRITE(YSTRING6, FMT=YFMT) XDATA_LAI(I,3*J-1,JVEGTYPE)
          YDATA_MONTH(J) = YSTRING6
        END IF
      END DO
      !
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_ROOT_DEPTH(I,JVEGTYPE)),'.',DEC(XDATA_ROOT_DEPTH(I,JVEGTYPE)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_ROOT_DEPTH(I,JVEGTYPE)
      YDATA_VEGPARAM(1) = YSTRING6
      WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_GROUND_DEPTH(I,JVEGTYPE)),'.',DEC(XDATA_GROUND_DEPTH(I,JVEGTYPE)),')'
      WRITE(YSTRING6, FMT=YFMT) XDATA_GROUND_DEPTH(I,JVEGTYPE)
      YDATA_VEGPARAM(2) = YSTRING6
      !
      IF (JVEGTYPE==NVT_TEBD) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_TEBD)),'.',DEC(XDATA_H_TREE(I,NVT_TEBD)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_TEBD)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_BONE) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_BONE)),'.',DEC(XDATA_H_TREE(I,NVT_BONE)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_BONE)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_TRBE) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_TRBE)),'.',DEC(XDATA_H_TREE(I,NVT_TRBE)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_TRBE)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_TRBD) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_TRBD)),'.',DEC(XDATA_H_TREE(I,NVT_TRBD)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_TRBD)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_TEBE) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_TEBE)),'.',DEC(XDATA_H_TREE(I,NVT_TEBE)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_TEBE)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_TENE) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_TENE)),'.',DEC(XDATA_H_TREE(I,NVT_TENE)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_TENE)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_BOBD) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_BOBD)),'.',DEC(XDATA_H_TREE(I,NVT_BOBD)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_BOBD)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_BOND) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_BOND)),'.',DEC(XDATA_H_TREE(I,NVT_BOND)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_BOND)
        YDATA_VEGPARAM(3) = YSTRING6
      ELSE IF (JVEGTYPE==NVT_SHRB) THEN
        WRITE(YFMT,'(A2,I1,A1,I1,A1)') '(F',NB(XDATA_H_TREE(I,NVT_SHRB)),'.',DEC(XDATA_H_TREE(I,NVT_SHRB)),')'
        WRITE(YSTRING6, FMT=YFMT) XDATA_H_TREE(I,NVT_SHRB)
        YDATA_VEGPARAM(3) = YSTRING6        
      ELSE
        YDATA_VEGPARAM(3) = '       '
      END IF
      !
      IF (JVEGTYPE==NVT_TEBD .OR. JVEGTYPE==NVT_BONE .OR. JVEGTYPE==NVT_TRBE .OR. &
          JVEGTYPE==NVT_TRBD .OR. JVEGTYPE==NVT_TEBE .OR. JVEGTYPE==NVT_TENE .OR. &
          JVEGTYPE==NVT_BOBD .OR. JVEGTYPE==NVT_BOND .OR. JVEGTYPE==NVT_SHRB ) THEN
        WRITE(NTEX, FMT=*) &
            I,' & ',CNAME(I ),' & ',YDATA_MONTH(1 ), ' & ',YDATA_MONTH(2),' & ', &
              YDATA_MONTH(3 ),' & ',YDATA_MONTH(4 ), ' & ',YDATA_MONTH(5),' & ', &
              YDATA_MONTH(6 ),' & ',YDATA_MONTH(7 ), ' & ',YDATA_MONTH(8),' & ', &
              YDATA_MONTH(9 ),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
              YDATA_MONTH(12),' & ',YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' & ',YDATA_VEGPARAM(3),' \\'  
      ELSE IF (JVEGTYPE==NVT_ROCK .OR. JVEGTYPE==NVT_SNOW .OR. JVEGTYPE==NVT_NO) THEN
        WRITE(NTEX, FMT=*) &
            I,' & ',CNAME(I),' & ',YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' \\'  
      ELSE
        WRITE(NTEX, FMT=*) &
            I,' & ',CNAME(I),' & ',YDATA_MONTH(1), ' & ',YDATA_MONTH(2), ' & ', &
              YDATA_MONTH(3),' & ',YDATA_MONTH(4), ' & ',YDATA_MONTH(5), ' & ', &
              YDATA_MONTH(6),' & ',YDATA_MONTH(7), ' & ',YDATA_MONTH(8), ' & ', &
              YDATA_MONTH(9),' & ',YDATA_MONTH(10),' & ',YDATA_MONTH(11),' & ', &
              YDATA_MONTH(12),' & ',YDATA_VEGPARAM(1),' & ',YDATA_VEGPARAM(2),' \\'  
      END IF
      WRITE(NTEX,*) '\hline'
      GLINE=.TRUE.
    END IF
    CALL HLINE(NTEX,GLINE,I)
    IF (IP==NLINES) EXIT
  END DO
  !
  WRITE(NTEX,*) '\end{tabular}'
  !
  WRITE(NTEX,*) '\clearpage'
 END DO
 !
 !
!-------------------------------------------------------------------------------
END DO
IF (LHOOK) CALL DR_HOOK('WRITE_COVER_TEX_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_COVER_TEX_ISBA

