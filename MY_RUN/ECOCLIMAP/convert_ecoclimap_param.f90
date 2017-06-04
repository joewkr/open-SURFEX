PROGRAM CONVERT_ECOCLIMAP_PARAM
!
IMPLICIT NONE
!
!CALL BIN_TO_ASC(1)
!CALL BIN_TO_ASC(2)
!CALL BIN_TO_ASC(3)
!
CALL ASC_TO_BIN(1)
CALL ASC_TO_BIN(2)
CALL ASC_TO_BIN(3)
!
CONTAINS
!
SUBROUTINE BIN_TO_ASC(KFILE)
!
INTEGER, INTENT(IN) :: KFILE
!
INTEGER :: NBCOVERS,NBAN,IAN, JAN
INTEGER :: JCOVER, ICOVER, JVEGTYPE, JLAI, J, IREC
REAL*8 :: ZTOWN, ZNATURE, ZWATER, ZSEA, ZDG2, ZDG3, ZDICE, ZHT
REAL*8 :: ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD, ZWATSUP, ZIRRIG
REAL*8, DIMENSION(3) :: ZALB, ZEMIS
REAL*8, DIMENSION(3,3) :: ZHC,ZTC,ZD
REAL*8, DIMENSION(2) :: ZTRAFFIC, ZINDUSTRY
INTEGER*4, DIMENSION(2):: ISEED, IREAP
REAL*8, DIMENSION(19) :: ZVEGTYPE
REAL*8, DIMENSION(36) :: ZLAI
REAL*8, DIMENSION(36) :: ZALB_SOIL_NIR, ZALB_SOIL_VIS, ZALB_VEG_NIR, ZALB_VEG_VIS
CHARACTER(LEN=4),DIMENSION(19) :: CVEGTYPE
CHARACTER(LEN=1), PARAMETER :: CSEP=' '

DATA CVEGTYPE/'  No','Rock','Snow','Tebd','Bone','Trbe','  C3','  C4',' Irr',&
       'Gras','Trog','Park','Trbd','Tebe','Tene','Bobd','Bond','Bogr','Shrb'/              
!
IF (KFILE==1) THEN
  OPEN(11,FILE='ecoclimapI_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
  NBCOVERS = 255
  NBAN = 1
  IAN = 1992
  OPEN(12,FILE='ecoclimapI_covers_param2.dat',FORM='FORMATTED')
ELSEIF (KFILE==2) THEN
  OPEN(11,FILE='ecoclimapII_eu_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
  NBCOVERS = 273
  NBAN = 5
  IAN = 2002
  OPEN(12,FILE='ecoclimapII_eu_covers_param2.dat',FORM='FORMATTED')
ELSEIF (KFILE==3) THEN
  OPEN(11,FILE='ecoclimapII_af_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
  NBCOVERS = 38
  NBAN = 8
  IAN = 2000
  OPEN(12,FILE='ecoclimapII_af_covers_param2.dat',FORM='FORMATTED')
ENDIF
!
IREC = 0
DO ICOVER=1,NBCOVERS
  !
  IREC = IREC+1
  READ(11,REC=IREC) JCOVER
  IREC = IREC+1
  READ(11,REC=IREC) ZTOWN,ZNATURE,ZWATER,ZSEA
  !  
  WRITE(12,FMT="(A6,I3,'"//CSEP//"')") 'COVER ',JCOVER
  !
  WRITE(12,FMT="(A)") ' FRACTION'//CSEP//'  Town'//CSEP//'Nature'//CSEP//' Water'//CSEP//'   Sea'
  WRITE(12,FMT="(I9,4('"//CSEP//"',F6.2))") JCOVER,ZTOWN,ZNATURE,ZWATER,ZSEA
  !
  IF (ZTOWN+ZNATURE+ZWATER+ZSEA==0.) CYCLE
  !
  !WRITE(12,FMT=*) ''
  !
  IF (ZNATURE.GT.0.) THEN
    IREC = IREC+1
    READ(11,REC=IREC) ZVEGTYPE(:)
    WRITE(12,FMT="(A)") ' FRACTION'//CSEP//'    No'//CSEP//'  Rock'//CSEP//'  Snow'//CSEP//'  Tebd'//CSEP//'  Bone'//CSEP//&
        '  Trbe'//CSEP//'    C3'//CSEP//'    C4'//CSEP//'   Irr'//CSEP//'  Gras'//CSEP//'  Trog'//CSEP//'  Park'//CSEP//&
        '  Trbd'//CSEP//'  Tebe'//CSEP//'  Tene'//CSEP//'  Bobd'//CSEP//'  Bond'//CSEP//'  Bogr'//CSEP//'  Shrb'
    WRITE (12,FMT="(I9,19('"//CSEP//"',F6.2))") JCOVER,ZVEGTYPE(:)
    !
    IF (KFILE<=2) THEN
      DO J=1,3
        IREC = IREC+1
        READ(11,REC=IREC) ZALB_SOIL_NIR((J-1)*12+1:J*12)
      ENDDO
      DO J=1,3
        IREC = IREC+1
        READ(11,REC=IREC) ZALB_SOIL_VIS((J-1)*12+1:J*12)
      ENDDO 
      WRITE (12,FMT="(A)") ' mean ALB_SOIL_NIR (36 10-day periods)'
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' Jan  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Feb '//CSEP//'      '//CSEP//'      '//CSEP//'  Mar '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Apr '//CSEP//'      '
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_NIR(1:12)
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' May  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Jun '//CSEP//'      '//CSEP//'      '//CSEP//'  Jul '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Aug '//CSEP//'      '
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_NIR(13:24)
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' Sep  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Oct '//CSEP//'      '//CSEP//'      '//CSEP//'  Nov '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Dec '//CSEP//'      '
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_NIR(25:36)  
          !
      WRITE (12,FMT="(A)") ' mean ALB_SOIL_VIS (36 10-day periods)'
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' Jan  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Feb '//CSEP//'      '//CSEP//'      '//CSEP//'  Mar '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Apr '//CSEP//'      '        
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_VIS(1:12)
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' May  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Jun '//CSEP//'      '//CSEP//'      '//CSEP//'  Jul '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Aug '//CSEP//'      '        
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_VIS(13:24)
      WRITE(12,FMT="(A)") '          '//CSEP//'      '//CSEP//' Sep  '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Oct '//CSEP//'      '//CSEP//'      '//CSEP//'  Nov '//CSEP//'      '&
          //CSEP//'      '//CSEP//'  Dec '//CSEP//'      '        
      WRITE(12,FMT="(I9,'"//CSEP//"',12('"//CSEP//"',F6.4))") JCOVER,ZALB_SOIL_VIS(25:36)  
    ENDIF   
    !
    DO JVEGTYPE=1,SIZE(ZVEGTYPE)
      IF (ZVEGTYPE(JVEGTYPE).NE.0.) THEN
        JAN = IAN 
        WRITE(12,FMT="(A10,A6,'"//CSEP//"')") '  VEGTYPE'//CSEP//'',CVEGTYPE(JVEGTYPE)
        IF (KFILE<=2) THEN
          IREC = IREC+1      
          READ(11,REC=IREC) ZDG2,ZDG3,ZDICE
        ELSE
          IREC = IREC+1      
          READ(11,REC=IREC) ZDG2,ZDG3
          ZDICE=ZDG2
        ENDIF
        WRITE(12,FMT="(A)") '         '//CSEP//' DEPTH'//CSEP//'  Root'//CSEP//'  Soil'//CSEP//'   Ice'
        WRITE(12,FMT="(I9,'"//CSEP//"',A6,3('"//CSEP//"',F6.2))") JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3,ZDICE
        IF (JVEGTYPE.GT.3) THEN
          DO JLAI=1,NBAN
            WRITE(12,FMT="(A,I4,A)") '         '//CSEP//'   LAI'//CSEP//' YEAR ',JAN,' (36 10-day periods)'
            DO J=1,3
              IREC = IREC+1
              READ(11,REC=IREC) ZLAI((J-1)*12+1:J*12)
            ENDDO
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'Jan'//CSEP//'   '//CSEP//'   '&
                //CSEP//'Feb'//CSEP//'   '//CSEP//'   '//CSEP//'Mar'//CSEP//'   '//CSEP//'   '//CSEP//'Apr'//CSEP//'   '
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(1:12)
            !
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'May'//CSEP//'   '//CSEP//'   '&
                //CSEP//'Jun'//CSEP//'   '//CSEP//'   '//CSEP//'Jul'//CSEP//'   '//CSEP//'   '//CSEP//'Aug'//CSEP//'   '
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(13:24)
            !
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'Sep'//CSEP//'   '//CSEP//'   '&
                //CSEP//'Oct'//CSEP//'   '//CSEP//'   '//CSEP//'Nov'//CSEP//'   '//CSEP//'   '//CSEP//'Dec'//CSEP//'   '
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(25:36)            
            JAN=JAN+1
          ENDDO
          IF ((JVEGTYPE.LT.7) .OR. (JVEGTYPE.GT.12 .AND. JVEGTYPE.NE.18)) THEN 
            IREC = IREC+1      
            READ(11,REC=IREC) ZHT
            WRITE(12,FMT="(A)") '         |    HT|'
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,'"//CSEP//"',F3.0)") JCOVER,CVEGTYPE(JVEGTYPE),ZHT
          ENDIF
          IF (KFILE<=2) THEN
            DO J=1,3
              IREC = IREC+1
              READ(11,REC=IREC) ZALB_VEG_NIR((J-1)*12+1:J*12)
            ENDDO
            DO J=1,3
              IREC = IREC+1
              READ(11,REC=IREC) ZALB_VEG_VIS((J-1)*12+1:J*12)
            ENDDO  
            WRITE (12,FMT="(A)") '         '//CSEP//' mean ALB_VEG_NIR (36 10-day periods)'
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' Jan  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Feb '//CSEP//'      '//CSEP//'      '//CSEP//'  Mar '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Apr '//CSEP//'      '            
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_NIR(1:12)
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' May  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Jun '//CSEP//'      '//CSEP//'      '//CSEP//'  Jul '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Aug '//CSEP//'      '            
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_NIR(13:24)
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' Sep  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Oct '//CSEP//'      '//CSEP//'      '//CSEP//'  Nov '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Dec '//CSEP//'      '            
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_NIR(25:36)   
            !
            WRITE (12,FMT="(A)") '         '//CSEP//' mean ALB_VEG_VIS (36 10-day periods)'
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' Jan  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Feb '//CSEP//'      '//CSEP//'      '//CSEP//'  Mar '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Apr '//CSEP//'      '                 
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_VIS(1:12)
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' May  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Jun '//CSEP//'      '//CSEP//'      '//CSEP//'  Jul '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Aug '//CSEP//'      '                   
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_VIS(13:24)
            WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'      '//CSEP//' Sep  '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Oct '//CSEP//'      '//CSEP//'      '//CSEP//'  Nov '//CSEP//'      '&
            //CSEP//'      '//CSEP//'  Dec '//CSEP//'      '     
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F6.4))") JCOVER,CVEGTYPE(JVEGTYPE),&
                  ZALB_VEG_VIS(25:36) 
          ENDIF 
        ENDIF          
        IF (JVEGTYPE.EQ.8 .AND. KFILE.EQ.1 .OR. JVEGTYPE.EQ.9 .AND. KFILE.EQ.2) THEN
          IREC = IREC+1
          READ(11,REC=IREC) ISEED, IREAP, ZWATSUP, ZIRRIG
          WRITE(12,FMT="(A)") '         | IRRIG|Seed_M|Seed_D|Reap_M|Reap_D|Watsup| Irrig'          
          IF (ISEED(1).NE.1E+9) THEN
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,4('"//CSEP//"',I6),2('"//CSEP//"',F6.0))")  &
                  JCOVER,CVEGTYPE(JVEGTYPE),ISEED,IREAP,ZWATSUP,ZIRRIG 
          ELSE
            WRITE(12,FMT="(I9,'"//CSEP//"',A6,4('"//CSEP//"',I6),2('"//CSEP//"',F6.0))")  &
                  JCOVER,CVEGTYPE(JVEGTYPE),999,999,999,999,0.,0.
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    !WRITE(12,FMT=*) ''
  ENDIF
  !
  IF (ZTOWN.GT.0) THEN
    IREC = IREC+1      
    READ(11,REC=IREC) ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//'    Z0'//CSEP//'BldHgt'//CSEP//' WOHor'//CSEP//'   Bld'//CSEP//'Garden'
    WRITE(12,FMT="(I9,'"//CSEP//"',F6.2,'"//CSEP//"',F6.0,3('"//CSEP//"',F6.2))") JCOVER,ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD
    !
    IREC = IREC+1
    READ(11,REC=IREC) ZALB(:)
    IREC = IREC+1
    READ(11,REC=IREC) ZEMIS(:)
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//' AlbRf'//CSEP//' AlbRd'//CSEP//' AlbWl'//CSEP//'EmisRf'//CSEP//'EmisRd'//CSEP//'EmisWl'
    WRITE(12,FMT="(I9,6('"//CSEP//"',F6.2))") JCOVER,ZALB(:),ZEMIS(:)
    !
    IREC = IREC+1
    READ(11,REC=IREC) ZHC(1,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZHC(2,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZHC(3,:)
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//'Hc_Roof1'//CSEP//'Hc_Roof2'//CSEP//'Hc_Roof3'//CSEP//'Hc_Road1'//CSEP//'Hc_Road2'&
        //CSEP//'Hc_Road3'//CSEP//'Hc_Wall1'//CSEP//'Hc_Wall2'//CSEP//'Hc_Wall3'
    WRITE(12,FMT="(I9,9('"//CSEP//"',ES8.2))") JCOVER,ZHC(1,:),ZHC(2,:),ZHC(3,:)
    !
    IREC = IREC+1
    READ(11,REC=IREC) ZTC(1,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZTC(2,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZTC(3,:)
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//'Tc_Rf1'//CSEP//'Tc_Rf2'//CSEP//'Tc_Rf3'//CSEP//'Tc_Rd1'//CSEP//'Tc_Rd2'//CSEP//&
        'Tc_Rd3'//CSEP//'Tc_Wl1'//CSEP//'Tc_Wl2'//CSEP//'Tc_Wl3'
    WRITE(12,FMT="(I9,9('"//CSEP//"',F6.4))") JCOVER,ZTC(1,:),ZTC(2,:),ZTC(3,:)
    !
    IREC = IREC+1
    READ(11,REC=IREC) ZD(1,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZD(2,:)
    IREC = IREC+1
    READ(11,REC=IREC) ZD(3,:)
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//' D_Rf1'//CSEP//' D_Rf2'//CSEP//' D_Rf3'//CSEP//' D_Rd1'//CSEP//' D_Rd2'//CSEP//&
        ' D_Rd3'//CSEP//' D_Wl1'//CSEP//' D_Wl2'//CSEP//' D_Wl3'
    WRITE(12,FMT="(I9,9('"//CSEP//"',F6.3))") JCOVER,ZD(1,:),ZD(2,:),ZD(3,:)
    !
    IREC = IREC+1
    READ(11,REC=IREC) ZTRAFFIC(:),ZINDUSTRY(:)
    WRITE(12,FMT="(A)") '     TOWN'//CSEP//' H_Trf'//CSEP//'Le_Trf'//CSEP//' H_Ind'//CSEP//'Le_Ind'
    WRITE(12,FMT="(I9,4('"//CSEP//"',F6.0))") JCOVER,ZTRAFFIC(:),ZINDUSTRY(:)
    !
    !WRITE(12,FMT=*) ''
    IF (ZGRD.NE.0. .AND. ZNATURE.EQ.0.) THEN
      IREC = IREC+1
      READ(11,REC=IREC) ZVEGTYPE(:)
      WRITE(12,FMT="(A)") ' FRACTION'//CSEP//'    No'//CSEP//'  Rock'//CSEP//'  Snow'//CSEP//'  Tebd'//CSEP//'  Bone'//CSEP//&
        '  Trbe'//CSEP//'    C3'//CSEP//'    C4'//CSEP//'   Irr'//CSEP//'  Gras'//CSEP//'  Trog'//CSEP//'  Park'//CSEP//&
        '  Trbd'//CSEP//'  Tebe'//CSEP//'  Tene'//CSEP//'  Bobd'//CSEP//'  Bond'//CSEP//'  Bogr'//CSEP//'  Shrb'
      WRITE(12,FMT="(I9,19('"//CSEP//"',F6.2))") JCOVER,ZVEGTYPE(:)
      !
      DO JVEGTYPE=1,SIZE(ZVEGTYPE)
        IF (ZVEGTYPE(JVEGTYPE).NE.0.) THEN
          JAN = IAN 
          WRITE(12,FMT="(A10,A6,'"//CSEP//"')") '  VEGTYPE'//CSEP//'',CVEGTYPE(JVEGTYPE)
          IF (KFILE<=2) THEN
            IREC = IREC+1      
            READ(11,REC=IREC) ZDG2,ZDG3,ZDICE
          ELSE
            IREC = IREC+1      
            READ(11,REC=IREC) ZDG2,ZDG3
            ZDICE=ZDG2
          ENDIF
          WRITE(12,FMT="(A)") '         '//CSEP//' DEPTH'//CSEP//'  Root'//CSEP//'  Soil'//CSEP//'   Ice'
          WRITE(12,FMT="(I9,'"//CSEP//"',A6,3('"//CSEP//"',F6.2))") JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3,ZDICE
          IF (JVEGTYPE.GT.3) THEN
            DO JLAI=1,NBAN
              WRITE(12,FMT="(A,I4,A)") '         '//CSEP//'   LAI'//CSEP//' YEAR ',JAN,' (36 10-day periods)'
              DO J=1,3
                IREC = IREC+1
                READ(11,REC=IREC) ZLAI((J-1)*12+1:J*12)
              ENDDO
              WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'Jan'//CSEP//'   '//CSEP//'   '&
                  //CSEP//'Feb'//CSEP//'   '//CSEP//'   '//CSEP//'Mar'//CSEP//'   '//CSEP//'   '//CSEP//'Apr'//CSEP//'   '
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(1:12)
              !
              WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'May'//CSEP//'   '//CSEP//'   '&
                  //CSEP//'Jun'//CSEP//'   '//CSEP//'   '//CSEP//'Jul'//CSEP//'   '//CSEP//'   '//CSEP//'Aug'//CSEP//'   '
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(13:24)
              !
              WRITE(12,FMT="(A)") '         '//CSEP//'      '//CSEP//'   '//CSEP//'Sep'//CSEP//'   '//CSEP//'   '&
                  //CSEP//'Oct'//CSEP//'   '//CSEP//'   '//CSEP//'Nov'//CSEP//'   '//CSEP//'   '//CSEP//'Dec'//CSEP//'   '
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,36('"//CSEP//"',F3.1))") JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(25:36)            
              JAN=JAN+1
            ENDDO
            IF ((JVEGTYPE.LT.7) .OR. (JVEGTYPE.GT.12 .AND. JVEGTYPE.NE.18)) THEN 
              IREC = IREC+1      
              READ(11,REC=IREC) ZHT
              WRITE(12,FMT="(A)") '         |    HT|'
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,'"//CSEP//"',F3.0)") JCOVER,CVEGTYPE(JVEGTYPE),ZHT
            ENDIF
          ENDIF
          IF (JVEGTYPE.EQ.8 .AND. KFILE.EQ.1 .OR. JVEGTYPE.EQ.9 .AND. KFILE.EQ.2) THEN
            IREC = IREC+1
            READ(11,REC=IREC) ISEED, IREAP, ZWATSUP, ZIRRIG
            WRITE(12,FMT="(A)") '         | IRRIG|Seed_M|Seed_D|Reap_M|Reap_D|Watsup| Irrig'          
            IF (ISEED(1).NE.1E+9) THEN
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,4('"//CSEP//"',I6),2('"//CSEP//"',F6.0))")  &
                    JCOVER,CVEGTYPE(JVEGTYPE),ISEED,IREAP,ZWATSUP,ZIRRIG 
            ELSE
              WRITE(12,FMT="(I9,'"//CSEP//"',A6,4('"//CSEP//"',I6),2('"//CSEP//"',F6.0))")  &
                    JCOVER,CVEGTYPE(JVEGTYPE),999,999,999,999,0.,0.
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  WRITE(12,FMT=*) '' 
ENDDO
!
CLOSE(11)
CLOSE(12) 
!
END SUBROUTINE BIN_TO_ASC
!
SUBROUTINE ASC_TO_BIN(KFILE)
!
INTEGER, INTENT(IN) :: KFILE
!
INTEGER :: NBCOVERS,NBAN,IAN, JAN
INTEGER :: JCOVER, ICOVER, JVEGTYPE, JLAI, J, IREC
REAL*8 :: ZTOWN, ZNATURE, ZWATER, ZSEA, ZDG2, ZDG3, ZDICE, ZHT
REAL*8 :: ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD, ZWATSUP, ZIRRIG
REAL*8, DIMENSION(3) :: ZALB, ZEMIS
REAL*8, DIMENSION(3,3) :: ZHC,ZTC,ZD
REAL*8, DIMENSION(2) :: ZTRAFFIC, ZINDUSTRY
INTEGER, DIMENSION(2):: ISEED, IREAP
REAL*8, DIMENSION(19) :: ZVEGTYPE
REAL*8, DIMENSION(36) :: ZLAI
REAL*8, DIMENSION(36) :: ZALB_SOIL_NIR, ZALB_SOIL_VIS, ZALB_VEG_NIR, ZALB_VEG_VIS
CHARACTER(LEN=4),DIMENSION(19) :: CVEGTYPE
CHARACTER(LEN=200) :: HPOUB

DATA CVEGTYPE/'  No','Rock','Snow','Tebd','Bone','Trbe','  C3','  C4',' Irr', &
       'Gras','Trog','Park','Trbd','Tebe','Tene','Bobd','Bond','Bogr','Shrb'/
!
IF (KFILE==1) THEN
  OPEN(12,FILE='ecoclimapI_covers_param.dat',FORM='FORMATTED')
  NBCOVERS = 255
  NBAN = 1
  IAN = 1992
  OPEN(11,FILE='ecoclimapI_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
ELSEIF (KFILE==2) THEN
  OPEN(12,FILE='ecoclimapII_eu_covers_param.dat',FORM='FORMATTED')
  NBCOVERS = 273
  NBAN = 5
  IAN = 2002
  OPEN(11,FILE='ecoclimapII_eu_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
ELSEIF (KFILE==3) THEN
  OPEN(12,FILE='ecoclimapII_af_covers_param.dat',FORM='FORMATTED')
  NBCOVERS = 38
  NBAN = 8
  IAN = 2000
  OPEN(11,FILE='ecoclimapII_af_covers_param.bin',FORM='UNFORMATTED',ACCESS='DIRECT',recl=20*8)
ENDIF
!
IREC = 0
DO ICOVER=1,NBCOVERS
  !
  READ(12,FMT=*) HPOUB
  READ(12,FMT=*) HPOUB
  READ(12,FMT=*) JCOVER,ZTOWN,ZNATURE,ZWATER,ZSEA
  !  
  IREC = IREC+1
  WRITE(11,REC=IREC) JCOVER
  IREC = IREC+1
  WRITE(11,REC=IREC) ZTOWN,ZNATURE,ZWATER,ZSEA  
  !
  IF (ZTOWN+ZNATURE+ZWATER+ZSEA==0.) CYCLE
  !
  !WRITE(12,FMT=*) ''
  !
  IF (ZNATURE.GT.0.) THEN
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZVEGTYPE(:)          
    IREC = IREC+1
    WRITE(11,REC=IREC) ZVEGTYPE(:)
    !
    IF (KFILE<=2) THEN
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_NIR(1:12)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_NIR(1:12)
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_NIR(13:24)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_NIR(13:24)      
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_NIR(25:36)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_NIR(25:36)      
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_VIS(1:12)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_VIS(1:12)      
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_VIS(13:24)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_VIS(13:24)        
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER, ZALB_SOIL_VIS(25:36)
      IREC = IREC+1      
      WRITE(11,REC=IREC) ZALB_SOIL_VIS(25:36)       
    ENDIF     
    !
    DO JVEGTYPE=1,SIZE(ZVEGTYPE)
      IF (ZVEGTYPE(JVEGTYPE).NE.0.) THEN
        JAN = IAN
        READ(12,FMT=*) HPOUB
        IF (KFILE<=2) THEN
          READ(12,FMT=*) HPOUB
          READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3,ZDICE
          IREC = IREC+1      
          WRITE(11,REC=IREC) ZDG2,ZDG3,ZDICE
        ELSE
          READ(12,FMT=*) HPOUB
          READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3  
          ZDICE=ZDG2
          IREC = IREC+1      
          WRITE(11,REC=IREC) ZDG2,ZDG3,ZDICE
        ENDIF
        IF (JVEGTYPE.GT.3) THEN
          DO JLAI=1,NBAN
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(1:12)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(13:24)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(25:36)  
            DO J=1,3
              IREC = IREC+1
              WRITE(11,REC=IREC) ZLAI((J-1)*12+1:J*12)
            ENDDO          
            JAN=JAN+1
          ENDDO
          IF ((JVEGTYPE.LT.7) .OR. (JVEGTYPE.GT.12 .AND. JVEGTYPE.NE.18)) THEN   
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZHT   
            IREC = IREC+1              
            WRITE(11,REC=IREC) ZHT
          ENDIF
          !
          IF (KFILE<=2) THEN
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_NIR(1:12)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_NIR(13:24)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_NIR(25:36)  
            DO J=1,3
              IREC = IREC+1
              WRITE(11,REC=IREC) ZALB_VEG_NIR((J-1)*12+1:J*12)
            ENDDO   
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_VIS(1:12)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_VIS(13:24)
            !
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZALB_VEG_VIS(25:36)  
            DO J=1,3
              IREC = IREC+1
              WRITE(11,REC=IREC) ZALB_VEG_VIS((J-1)*12+1:J*12)
            ENDDO     
          ENDIF       
          !
        ENDIF
        IF (JVEGTYPE.EQ.8 .AND. KFILE.EQ.1 .OR. JVEGTYPE.EQ.9 .AND. KFILE.EQ.2) THEN
          READ(12,FMT=*) HPOUB
          READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ISEED,IREAP,ZWATSUP,ZIRRIG      
          IREC = IREC+1
          IF (ISEED(1).NE.999) THEN
            WRITE(11,REC=IREC) ISEED, IREAP, ZWATSUP, ZIRRIG
          ELSE
            WRITE(11,REC=IREC) 1000000000,1000000000,1000000000,1000000000,0.,0.
          ENDIF
        ENDIF        
      ENDIF
    ENDDO
    !WRITE(12,FMT=*) ''
  ENDIF
  !
  IF (ZTOWN.GT.0) THEN
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD          
    IREC = IREC+1      
    WRITE(11,REC=IREC) ZZ0,ZBLDH,ZWOH,ZBLD,ZGRD
    !
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZALB(:),ZEMIS(:)    
    IREC = IREC+1
    WRITE(11,REC=IREC) ZALB(:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZEMIS(:)
    !
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZHC(1,:),ZHC(2,:),ZHC(3,:)    
    IREC = IREC+1
    WRITE(11,REC=IREC) ZHC(1,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZHC(2,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZHC(3,:)
    !
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZTC(1,:),ZTC(2,:),ZTC(3,:)    
    IREC = IREC+1
    WRITE(11,REC=IREC) ZTC(1,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZTC(2,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZTC(3,:)
    !
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZD(1,:),ZD(2,:),ZD(3,:)    
    IREC = IREC+1
    WRITE(11,REC=IREC) ZD(1,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZD(2,:)
    IREC = IREC+1
    WRITE(11,REC=IREC) ZD(3,:)
    !
    READ(12,FMT=*) HPOUB
    READ(12,FMT=*) JCOVER,ZTRAFFIC(:),ZINDUSTRY(:)    
    IREC = IREC+1
    WRITE(11,REC=IREC) ZTRAFFIC(:),ZINDUSTRY(:)
    !
    !WRITE(12,FMT=*) ''
    IF (ZGRD.NE.0 .AND. ZNATURE.EQ.0) THEN
      READ(12,FMT=*) HPOUB
      READ(12,FMT=*) JCOVER,ZVEGTYPE(:)          
      IREC = IREC+1
      WRITE(11,REC=IREC) ZVEGTYPE(:)
      !
      DO JVEGTYPE=1,SIZE(ZVEGTYPE)
        IF (ZVEGTYPE(JVEGTYPE).NE.0.) THEN
          JAN = IAN
          READ(12,FMT=*) HPOUB
          IF (KFILE<=2) THEN
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3,ZDICE                
            IREC = IREC+1      
            WRITE(11,REC=IREC) ZDG2,ZDG3,ZDICE
          ELSE
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZDG2,ZDG3  
            ZDICE=ZDG2
            IREC = IREC+1      
            WRITE(11,REC=IREC) ZDG2,ZDG3,ZDICE
          ENDIF
          IF (JVEGTYPE.GT.3) THEN
            DO JLAI=1,NBAN
              READ(12,FMT=*) HPOUB
              READ(12,FMT=*) HPOUB
              READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(1:12)
              !
              READ(12,FMT=*) HPOUB
              READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(13:24)
              !
              READ(12,FMT=*) HPOUB
              READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZLAI(25:36)  
              DO J=1,3
                IREC = IREC+1
                WRITE(11,REC=IREC) ZLAI((J-1)*12+1:J*12)
              ENDDO          
              JAN=JAN+1
            ENDDO
            IF ((JVEGTYPE.LT.7) .OR. (JVEGTYPE.GT.12 .AND. JVEGTYPE.NE.18)) THEN   
              READ(12,FMT=*) HPOUB
              READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ZHT   
              IREC = IREC+1              
              WRITE(11,REC=IREC) ZHT
            ENDIF
          ENDIF
          IF (JVEGTYPE.EQ.8 .AND. KFILE.EQ.1 .OR. JVEGTYPE.EQ.9 .AND. KFILE.EQ.2) THEN
            READ(12,FMT=*) HPOUB
            READ(12,FMT=*) JCOVER,CVEGTYPE(JVEGTYPE),ISEED,IREAP,ZWATSUP,ZIRRIG
            IREC = IREC+1
            IF (ISEED(1).NE.999) THEN
              WRITE(11,REC=IREC) ISEED, IREAP, ZWATSUP, ZIRRIG
            ELSE
              WRITE(11,REC=IREC) 1000000000,1000000000,1000000000,1000000000,0.,0.
            ENDIF
          ENDIF        
        ENDIF
      ENDDO
    ENDIF
    !
  ENDIF
  !WRITE(12,FMT=*) '' 
ENDDO
!
CLOSE(11)
CLOSE(12) 
!
END SUBROUTINE ASC_TO_BIN
!
END PROGRAM
