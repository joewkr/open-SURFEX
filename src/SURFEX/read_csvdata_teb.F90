!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE READ_CSVDATA_TEB (BDD, &
                                   HPROGRAM,HFILE)
!     #########################
!
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
!!    Original    05/2012 
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
!
USE MODD_CSTS,     ONLY : XTT
USE MODD_SURF_PAR, ONLY : XUNDEF
!
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
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
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to read
!
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
INTEGER            :: ILUNAM       ! logical unit of the file
INTEGER            :: ILUOUT       ! logical unit of the output listing file
 CHARACTER(LEN=400) :: YSTRING
 CHARACTER(LEN=80)  :: YSTRING1, YSTRING2, YSTRING3, YSTRING4, &
                      YSTRING5, YSTRING6, YSTRING7, YSTRING8, YSTRING9
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YUSE_NAME ! building's use name
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YBLD_NAME ! building name
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YLAYER    ! name of layer
INTEGER            :: I1
INTEGER            :: I2
INTEGER            :: JBLD                ! loop counter on buildings
INTEGER            :: JAGE                ! loop counter on building's ages
INTEGER            :: JUSE                ! loop counter on building's uses
INTEGER            :: JLAYER              ! loop counter on layers
INTEGER            :: IINDEX              ! index in descriptive data arrays
!
INTEGER            :: IALL_HYP            ! number of hypotheses for equipment
INTEGER            :: IHYP                ! kept hypothese for equipment
INTEGER            :: IRES                ! index for residential use
 CHARACTER(LEN=10)  :: YTYPE_OF_DATA       ! 'STRUCTURE', 'EQUIPMENT'
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
IF (LEN_TRIM(HFILE)==0) THEN
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',1,ZHOOK_HANDLE)
  RETURN
END IF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    1.     Opens the file
!      --------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
!
!-------------------------------------------------------------------------------
!
!*    2.1    Reads the number of building types and of construction dates
!            ------------------------------------------------------------
!
 CALL READ_CONF_IN_CSVFILE("Nb de types de batiments",BDD%NDESC_BLD)
 CALL READ_CONF_IN_CSVFILE("Nb de plages de dates",BDD%NDESC_AGE)
 CALL READ_CONF_IN_CSVFILE("Nb de types d_usages",BDD%NDESC_USE)
!
BDD%NDESC_CODE = BDD%NDESC_BLD * BDD%NDESC_AGE
!
!*    2.2    Reads the number of layers for description of the surfaces
!            ----------------------------------------------------------
!
 CALL READ_CONF_IN_CSVFILE("Nb de couches MUR",BDD%NDESC_WALL_LAYER)
 CALL READ_CONF_IN_CSVFILE("Nb de couches TOITURE",BDD%NDESC_ROOF_LAYER)
 CALL READ_CONF_IN_CSVFILE("Nb de couches PLANCHER",BDD%NDESC_FLOOR_LAYER)
!
!-------------------------------------------------------------------------------
!
!*    3.     Reads the codes of the building construction (or renovation) dates
!            ------------------------------------------------------------------
!
ALLOCATE(BDD%NDESC_AGE_LIST(BDD%NDESC_AGE))
ALLOCATE(BDD%NDESC_AGE_DATE(BDD%NDESC_AGE))
!
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
!* reads the record
  READ(ILUNAM,END=98,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6
  IF (YSTRING1=='DATE' .AND. YSTRING2(4:)=='plage de date') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, BDD%NDESC_AGE_LIST(:)
  END IF
  IF (YSTRING1=='DATE' .AND. YSTRING2(:)=='Date maximum') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, BDD%NDESC_AGE_DATE(:)
  END IF
END DO
!
98 CONTINUE
REWIND(ILUNAM)
!
!
!
!-------------------------------------------------------------------------------
!
!*    3.     Reads the codes of the building and building's use types
!            --------------------------------------------------------
!
ALLOCATE(YBLD_NAME(BDD%NDESC_BLD))
ALLOCATE(YUSE_NAME(BDD%NDESC_USE))
!
ALLOCATE(BDD%NDESC_BLD_LIST(BDD%NDESC_BLD))
ALLOCATE(BDD%NDESC_CODE_LIST(BDD%NDESC_CODE))
ALLOCATE(BDD%NDESC_USE_LIST(BDD%NDESC_USE))
!
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
!* reads the record
  READ(ILUNAM,END=99,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6

  IF (YSTRING1=='TYPES USAGES' .AND. YSTRING4=='TYPES BATIMENTS') THEN
    ! reads both use and building types
    DO JBLD=1,MAX(BDD%NDESC_BLD,BDD%NDESC_USE)
      READ(ILUNAM,FMT='(A400)') YSTRING
      CALL FRENCH_TO_ENGLISH(YSTRING)
      READ(YSTRING,FMT=*) YSTRING1, I1, YSTRING3, YSTRING4, I2, YSTRING6
    ! updates building types
      IF (JBLD<=BDD%NDESC_BLD) THEN
        YBLD_NAME(JBLD) = YSTRING4
        BDD%NDESC_BLD_LIST(JBLD) = I2
        DO JAGE=1,BDD%NDESC_AGE
          IINDEX = (JBLD-1)*BDD%NDESC_AGE + JAGE
          BDD%NDESC_CODE_LIST(IINDEX) = BLD_CODE(BDD%NDESC_BLD_LIST(JBLD),BDD%NDESC_AGE_LIST(JAGE))
        END DO
      END IF
    ! updates building's use types
      IF (JBLD<=BDD%NDESC_USE) THEN
        YUSE_NAME     (JBLD) = YSTRING1
        BDD%NDESC_USE_LIST(JBLD) = I1
      END IF
    END DO
    EXIT
  END IF
END DO
!
99 CONTINUE
REWIND(ILUNAM)
!
!------------------------------------------------------------------------------
!
!*    4.     town parameters depending on building structure descriptions
!      ------------------------------------------------------------------
!
YTYPE_OF_DATA = 'STRUCTURE'
!
!* radiative properties
!
ALLOCATE(BDD%XDESC_ALB_ROOF(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,"Exterieur",'Albedo',BDD%XDESC_ALB_ROOF)

ALLOCATE(BDD%XDESC_ALB_WALL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('MUR',YBLD_NAME,"Couche 1 (Ext)",'Albedo',BDD%XDESC_ALB_WALL)

ALLOCATE(BDD%XDESC_EMIS_ROOF(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,"Exterieur",'Emissivite',BDD%XDESC_EMIS_ROOF)

ALLOCATE(BDD%XDESC_EMIS_WALL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('MUR',YBLD_NAME,"Couche 1 (Ext)",'Emissivite',BDD%XDESC_EMIS_WALL)
!
!* thermal properties for roof
!
ALLOCATE(YLAYER(BDD%NDESC_ROOF_LAYER))
DO JLAYER=1,BDD%NDESC_ROOF_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Exterieur '
  ELSEIF (JLAYER==BDD%NDESC_ROOF_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Interieur '
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Milieu '
  END IF
END DO
!
ALLOCATE(BDD%XDESC_HC_ROOF(BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
ALLOCATE(BDD%XDESC_TC_ROOF(BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
ALLOCATE(BDD%XDESC_D_ROOF (BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
DO JLAYER=1,BDD%NDESC_ROOF_LAYER
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',BDD%XDESC_HC_ROOF(:,JLAYER))
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Conductivite',BDD%XDESC_TC_ROOF(:,JLAYER))
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',BDD%XDESC_D_ROOF(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
BDD%XDESC_HC_ROOF = BDD%XDESC_HC_ROOF * 1000.
DEALLOCATE(YLAYER)
!
!* thermal properties for wall
!
ALLOCATE(YLAYER(BDD%NDESC_WALL_LAYER))
DO JLAYER=1,BDD%NDESC_WALL_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Ext)'
  ELSEIF (JLAYER==BDD%NDESC_WALL_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Int)'
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Milieu)'
  END IF
END DO
!
ALLOCATE(BDD%XDESC_HC_WALL(BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
ALLOCATE(BDD%XDESC_TC_WALL(BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
ALLOCATE(BDD%XDESC_D_WALL (BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
DO JLAYER=1,BDD%NDESC_WALL_LAYER
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',BDD%XDESC_HC_WALL(:,JLAYER))
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Conductivite',BDD%XDESC_TC_WALL(:,JLAYER))
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',BDD%XDESC_D_WALL(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
BDD%XDESC_HC_WALL = BDD%XDESC_HC_WALL * 1000.
DEALLOCATE(YLAYER)
!
!
!* thermal properties for floor
!
ALLOCATE(YLAYER(BDD%NDESC_FLOOR_LAYER))
DO JLAYER=1,BDD%NDESC_FLOOR_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Superieur '
  ELSEIF (JLAYER==BDD%NDESC_FLOOR_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Inferieur '
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Milieu '
  END IF
END DO
!
ALLOCATE(BDD%XDESC_HC_FLOOR(BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
ALLOCATE(BDD%XDESC_TC_FLOOR(BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
ALLOCATE(BDD%XDESC_D_FLOOR (BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
DO JLAYER=1,BDD%NDESC_FLOOR_LAYER
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',BDD%XDESC_HC_FLOOR(:,JLAYER))
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Conductivite',BDD%XDESC_TC_FLOOR(:,JLAYER))
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',BDD%XDESC_D_FLOOR(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
BDD%XDESC_HC_FLOOR = BDD%XDESC_HC_FLOOR * 1000.
DEALLOCATE(YLAYER)
!
!* windows
!  -------
!
ALLOCATE(BDD%XDESC_SHGC(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'Facteur solaire m',BDD%XDESC_SHGC)

ALLOCATE(BDD%XDESC_U_WIN(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'U-factor',BDD%XDESC_U_WIN)

ALLOCATE(BDD%XDESC_GR(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'Surface fenetre /surface facade',BDD%XDESC_GR)
!
!------------------------------------------------------------------------------
!
!*    5.     town parameters depending on building equipment descriptions
!      ------------------------------------------------------------------
!
YTYPE_OF_DATA = 'EQUIPMENT'
!
 CALL READ_CONF_IN_CSVFILE("Nb d_hypotheses",IALL_HYP)
!
!* Air conditionning systems
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Taux de rejets en toitures",IHYP)
ALLOCATE(BDD%XDESC_F_WASTE_CAN(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Taux de rejets en toitures',BDD%XDESC_F_WASTE_CAN)
BDD%XDESC_F_WASTE_CAN = BDD%XDESC_F_WASTE_CAN / 100. ! % => fraction
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Taux de rejets secs",IHYP)
ALLOCATE(BDD%XDESC_F_WATER_COND(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Taux de rejets secs',BDD%XDESC_F_WATER_COND)
BDD%XDESC_F_WATER_COND = 1. - BDD%XDESC_F_WATER_COND / 100. ! % => fraction    and dry waste => humid waste
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Performance (COP)",IHYP)
ALLOCATE(BDD%XDESC_COP_RAT(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Performance (COP)',BDD%XDESC_COP_RAT)
!
!
!* Heating systems
!
!CALL READ_HYP_IN_CSVFILE("Chauffage","Efficacite energetique",IHYP)
ALLOCATE(BDD%XDESC_EFF_HEAT(BDD%NDESC_CODE))
!CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Chauffage",'Efficacite energetique',XDESC_EFF_HEAT)
BDD%XDESC_EFF_HEAT = 0.9
!
!
!* Sanitary ventilation
 CALL READ_HYP_IN_CSVFILE("Infiltration","Taux de renouvellement d_air",IHYP)
ALLOCATE(BDD%XDESC_INF(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Infiltration","Taux de renouvellement d_air",BDD%XDESC_INF)
!
 CALL READ_HYP_IN_CSVFILE("Ventilation Mecanique Controlee","Taux de renouvellement d_air",IHYP)
ALLOCATE(BDD%XDESC_V_VENT(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Ventilation Mecanique Controlee","Taux de renouvellement d_air",BDD%XDESC_V_VENT)
!
!* Greenroof fraction
 CALL READ_HYP_IN_CSVFILE("Toits vegetalises","Implantation",IHYP)
ALLOCATE(BDD%XDESC_GREENROOF(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Toits vegetalises","Implantation",BDD%XDESC_GREENROOF)
!
!* solar panels
 CALL READ_HYP_IN_CSVFILE("Panneau solaire","Emissivite",IHYP)
ALLOCATE(BDD%XDESC_EMIS_PANEL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Panneau solaire","Emissivite",BDD%XDESC_EMIS_PANEL)
!
 CALL READ_HYP_IN_CSVFILE("Panneau solaire","Coefficient d_absorption",IHYP)
ALLOCATE(BDD%XDESC_ALB_PANEL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Panneau solaire","Coefficient d_absorption",BDD%XDESC_ALB_PANEL)
BDD%XDESC_ALB_PANEL = 1. - BDD%XDESC_ALB_PANEL  ! absorption ==> albedo
!
 CALL READ_HYP_IN_CSVFILE("Panneau solaire","Rendement",IHYP)
ALLOCATE(BDD%XDESC_EFF_PANEL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Panneau solaire","Rendement",BDD%XDESC_EFF_PANEL)
BDD%XDESC_EFF_PANEL = BDD%XDESC_EFF_PANEL /100.  ! % ==> fraction
!
 CALL READ_HYP_IN_CSVFILE("Panneau solaire","Surface des panneaux / surface du toit",IHYP)
ALLOCATE(BDD%XDESC_FRAC_PANEL(BDD%NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Panneau solaire","Surface des panneaux / surface du toit",BDD%XDESC_FRAC_PANEL)
BDD%XDESC_FRAC_PANEL = BDD%XDESC_FRAC_PANEL/100. ! % ==> fraction

!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
!*    7.     town parameters depending on building's use descriptions
!      --------------------------------------------------------------
!
!
YTYPE_OF_DATA = 'USE'
!
!* Temperature target for air conditionning
 CALL READ_HYP_IN_CSVFILE("Climatisation","Temp. de consigne",IHYP)
ALLOCATE(BDD%XDESC_TCOOL_TARGET(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Climatisation","Temp. de consigne",BDD%XDESC_TCOOL_TARGET)
BDD%XDESC_TCOOL_TARGET = BDD%XDESC_TCOOL_TARGET + XTT ! C => K
!
!* Temperature target for domestic heating
 CALL READ_HYP_IN_CSVFILE("Chauffage","Temp. de consigne",IHYP)
ALLOCATE(BDD%XDESC_THEAT_TARGET(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Chauffage","Temp. de consigne",BDD%XDESC_THEAT_TARGET)
BDD%XDESC_THEAT_TARGET = BDD%XDESC_THEAT_TARGET + XTT ! C => K
!
!* Internal gains
 CALL READ_HYP_IN_CSVFILE("Apports internes","Flux",IHYP)
ALLOCATE(BDD%XDESC_QIN(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Apports internes","Flux",BDD%XDESC_QIN)
!
!* Latent fraction for internal gains
 CALL READ_HYP_IN_CSVFILE("Apports internes","Fraction latente",IHYP)
ALLOCATE(BDD%XDESC_QIN_FLAT(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Apports internes","Fraction latente",BDD%XDESC_QIN_FLAT)
BDD%XDESC_QIN_FLAT = BDD%XDESC_QIN_FLAT / 100. ! % => fraction
!
!* Solar protections
 CALL READ_HYP_IN_CSVFILE("Protection solaire","Facteur solaire m",IHYP)
ALLOCATE(BDD%XDESC_SHGC_SH(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Protection solaire","Facteur solaire m",BDD%XDESC_SHGC_SH)
!
 CALL READ_HYP_IN_CSVFILE("Protection solaire","Active",IHYP)
ALLOCATE(BDD%XDESC_SHADE(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Protection solaire","Active",BDD%XDESC_SHADE)
!
!* Extra Natural ventilation (windows open or extra mechanical ventilation)
 CALL READ_HYP_IN_CSVFILE("Sur-ventilation","Type d_ouverture",IHYP)
ALLOCATE(BDD%XDESC_NATVENT(BDD%NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Sur-ventilation","Type d_ouverture",BDD%XDESC_NATVENT)
!
!* fraction of residential use for the buildings
ALLOCATE(BDD%XDESC_RESIDENTIAL(BDD%NDESC_USE))
CALL READ_CONF_IN_CSVFILE("Residentiel",IRES)
BDD%XDESC_RESIDENTIAL(:) = 0.
DO JUSE=1,BDD%NDESC_USE
  IF (JUSE==IRES) BDD%XDESC_RESIDENTIAL(JUSE) = 1.
END DO

!------------------------------------------------------------------------------
!
!*    8.     town parameters depending on urban structure
!      --------------------------------------------------
!
BDD%NDESC_ROAD_LAYER = 3
!
ALLOCATE(BDD%XDESC_ALB_ROAD(BDD%NDESC_CODE))
BDD%XDESC_ALB_ROAD = 0.08
ALLOCATE(BDD%XDESC_EMIS_ROAD(BDD%NDESC_CODE))
BDD%XDESC_EMIS_ROAD = 0.94
ALLOCATE(BDD%XDESC_HC_ROAD(BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
BDD%XDESC_HC_ROAD(:,1)  = 1940000.
BDD%XDESC_HC_ROAD(:,2:) = 1280000.
ALLOCATE(BDD%XDESC_TC_ROAD(BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
BDD%XDESC_TC_ROAD(:,1)  = 0.74
BDD%XDESC_TC_ROAD(:,2:) = 0.25
ALLOCATE(BDD%XDESC_D_ROAD(BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
BDD%XDESC_D_ROAD(:,1)  = 0.05
BDD%XDESC_D_ROAD(:,2)  = 0.1
BDD%XDESC_D_ROAD(:,3:) = 1.
!
!-------------------------------------------------------------------------------
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
FUNCTION BLD_CODE(KBLD,KAGE)
INTEGER, INTENT(IN) :: KBLD     ! building type number
INTEGER, INTENT(IN) :: KAGE     ! building construction period number
INTEGER             :: BLD_CODE ! building code combining type and age
BLD_CODE = 100*KBLD+KAGE
END FUNCTION BLD_CODE
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
SUBROUTINE READ_CONF_IN_CSVFILE(HCODE1,KDATA)

 CHARACTER(LEN=*), INTENT(IN) :: HCODE1
INTEGER,          INTENT(OUT):: KDATA
 CHARACTER(LEN=80) :: YERROR
!
REWIND(ILUNAM)
DO
  YSTRING1 = ''
  YSTRING2 = ''
!* reads the record
 READ(ILUNAM,END=101,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2

  IF (TRIM(YSTRING1)==TRIM(HCODE1)) THEN
    READ(YSTRING,*) YSTRING1, KDATA
    REWIND(ILUNAM)
    RETURN
  END IF
END DO
!
101 CONTINUE
 YERROR=TRIM(HCODE1)//' not found in file : '//TRIM(HFILE)
 CALL ABOR1_SFX(YERROR)
!
END SUBROUTINE READ_CONF_IN_CSVFILE
!
SUBROUTINE READ_HYP_IN_CSVFILE(HCODE1,HCODE2,KDATA)

 CHARACTER(LEN=*), INTENT(IN) :: HCODE1
 CHARACTER(LEN=*), INTENT(IN) :: HCODE2
INTEGER,          INTENT(OUT):: KDATA
 CHARACTER(LEN=80) :: YERROR
LOGICAL           :: GCODE2
!
REWIND(ILUNAM)
DO
  YSTRING1 = ''
  YSTRING2 = ''
!* reads the record
  READ(ILUNAM,END=101,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2

  GCODE2 = TRIM(YSTRING2)==TRIM(HCODE2)
  IF (TRIM(YSTRING1)==TRIM(HCODE1) .AND. GCODE2) THEN
    READ(YSTRING,*) YSTRING1, YSTRING2, KDATA
    REWIND(ILUNAM)
    RETURN
  END IF
END DO
!
101 CONTINUE
 YERROR=TRIM(HCODE1)//' '//TRIM(HCODE2)//' not found in file : '//TRIM(HFILE)
 CALL ABOR1_SFX(YERROR)
!
END SUBROUTINE READ_HYP_IN_CSVFILE
!
SUBROUTINE READ_IN_CSVFILE(HCODE_ELEMENT,HCODE_TYPE,HCODE_ELEMENT2,HCODE_PARAM,PDATA)
!
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_ELEMENT  ! type of element
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HCODE_TYPE     ! building type or
                                                             ! building's use type
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_ELEMENT2 ! description of element
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_PARAM    ! name of Parameter
REAL, DIMENSION(:),             INTENT(OUT):: PDATA          ! data read in the csv file
!
REAL, DIMENSION(:), ALLOCATABLE      :: ZDATA        ! data array read in the file
LOGICAL, DIMENSION(SIZE(HCODE_TYPE)) :: GINITIALIZED ! Flag to know if parameter
!                                                    ! has been initialized correctly
LOGICAL                          :: GFOUND ! correct record has been found
 CHARACTER(LEN=80)                :: YTYPE  ! type of building or building's use
!                                          ! in the csv file record
 CHARACTER(LEN=100)               :: YERROR ! Character string for error message
INTEGER                          :: IN1 ! number of building type or use
INTEGER                          :: IN2 ! number of construction dates
!
IF (YTYPE_OF_DATA=='STRUCTURE') THEN
  ALLOCATE(ZDATA(BDD%NDESC_AGE))
  IN1=BDD%NDESC_BLD
  IN2=BDD%NDESC_AGE
ELSE IF (YTYPE_OF_DATA=='EQUIPMENT') THEN
  ALLOCATE(ZDATA(IALL_HYP))
  IN1=BDD%NDESC_BLD
  IN2=BDD%NDESC_AGE
ELSE IF (YTYPE_OF_DATA=='USE') THEN
  ALLOCATE(ZDATA(IALL_HYP))
  IN1=BDD%NDESC_USE
  IN2=1
END IF
!
PDATA = XUNDEF
GINITIALIZED(:)=.FALSE.
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
  YSTRING7=' '
  YSTRING8=' '
!* reads the record
  READ(ILUNAM,END=100,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6, YSTRING7
  !
  IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') THEN
    GFOUND = TRIM(YSTRING1)==TRIM(HCODE_ELEMENT) .AND. TRIM(YSTRING6)==TRIM(HCODE_ELEMENT2) &
                                                 .AND. TRIM(YSTRING7)==TRIM(HCODE_PARAM)
  ELSE IF (YTYPE_OF_DATA=='STRUCTURE') THEN
    GFOUND = TRIM(YSTRING1)==TRIM(HCODE_ELEMENT) .AND. TRIM(YSTRING4)==TRIM(HCODE_ELEMENT2) &
                                                 .AND. TRIM(YSTRING5)==TRIM(HCODE_PARAM)
  ELSE
    GFOUND = .FALSE.
  END IF

  IF (GFOUND) THEN
!* reads the data in the record
  IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, &
                        YSTRING6, YSTRING7, YSTRING8, YSTRING9, ZDATA(:)
  ELSE IF (YTYPE_OF_DATA=='STRUCTURE') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, &
                        YSTRING6, YSTRING7, ZDATA(:)
  END IF
!* in case of EQUIPMENT or USE data, one keeps the chosen hypothesis
    IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') ZDATA(:) = ZDATA(IHYP)
!* one finds for which building type or building's use type the data is for
    IF (YTYPE_OF_DATA=='EQUIPMENT') YTYPE = YSTRING2
    IF (YTYPE_OF_DATA=='STRUCTURE') YTYPE = YSTRING2
    IF (YTYPE_OF_DATA=='USE      ') YTYPE = YSTRING4
    !
    DO JBLD=1,IN1
      IF (TRIM(HCODE_TYPE(JBLD))==TRIM(YTYPE) .OR. TRIM(YTYPE)=='Tous batiments') THEN
!* one affects the data for this type of building for each construction dates
        DO JAGE=1,IN2
          IINDEX = (JBLD-1)*IN2 + JAGE
          PDATA(IINDEX) = ZDATA(MIN(JAGE,SIZE(ZDATA)))
        END DO
        GINITIALIZED(JBLD) = .TRUE.
      END IF
    END DO
  END IF
  IF (ALL(GINITIALIZED)) EXIT
END DO
!
100 CONTINUE
REWIND(ILUNAM)
DEALLOCATE(ZDATA)
!
!* one checks if the data is available for all building's types
IF (ANY(.NOT. GINITIALIZED)) THEN
  WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
  WRITE(ILUOUT,*) 'While reading the csv data file for building parameters specification'
  WRITE(ILUOUT,*) '(file ',TRIM(HFILE),')'
  WRITE(ILUOUT,*) 'The field corresponding to the following '
  WRITE(ILUOUT,*) 'identifiers:',TRIM(HCODE_ELEMENT),' ',TRIM(HCODE_ELEMENT2),' ',TRIM(HCODE_PARAM)
  WRITE(ILUOUT,*) 'has not been completely initialized.'
  WRITE(ILUOUT,*) 'The data for the following building types were not found:'
  IF (YTYPE_OF_DATA=='USE') THEN
    DO JBLD=1,IN1
      IF (.NOT. GINITIALIZED(JBLD)) WRITE(ILUOUT,*) '"',TRIM(YUSE_NAME(JBLD)),'"'
    END DO
  ELSE
    DO JBLD=1,IN1
      IF (.NOT. GINITIALIZED(JBLD)) WRITE(ILUOUT,*) '"',TRIM(YBLD_NAME(JBLD)),'"'
    END DO
  END IF
  WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'

  YERROR='Initialization not complete for: '//TRIM(HCODE_ELEMENT)//' '//TRIM(HCODE_ELEMENT2)//' '//TRIM(HCODE_PARAM)
  CALL ABOR1_SFX(YERROR)
END IF
!
END SUBROUTINE READ_IN_CSVFILE
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
SUBROUTINE FRENCH_TO_ENGLISH(HSTRING)
 CHARACTER(LEN=400), INTENT(INOUT) :: HSTRING ! csv record
INTEGER :: JL
LOGICAL :: GFRENCH
!
GFRENCH = .FALSE.
!* analyses if the record has been written in French convention 
!     French  convention (separator is ;  decimal symbol is ,) 
!  or English convention (separator is ,  decimal symbol is .)
DO JL=1,400
  IF (HSTRING(JL:JL)==';') GFRENCH=.TRUE.
END DO
!
! If French convention is used in the file, transforms it in English convention
IF (GFRENCH) THEN
  DO JL=1,400
    IF (HSTRING(JL:JL)==',') HSTRING(JL:JL)='.'
    IF (HSTRING(JL:JL)==';') HSTRING(JL:JL)=','
  END DO
END IF
!
END SUBROUTINE FRENCH_TO_ENGLISH
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_CSVDATA_TEB
