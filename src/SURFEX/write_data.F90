!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE WRITE_DATA(HPROGRAM)
!     #########################
!
!!**** *WRITE_DATA* initializes cover-field correspondance arrays
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
!!    R. Alkama    05/2012 : add new vegtypes (from 12 to 19)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------

USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_DATA_COVER,     ONLY : XDATA_TOWN, XDATA_NATURE, XDATA_SEA, XDATA_WATER, &
                                  XDATA_LAI, XDATA_VEGTYPE, XDATA_H_TREE,           &
                                  XDATA_GROUND_DEPTH, XDATA_ROOT_DEPTH,             &
                                  TDATA_SEED, TDATA_REAP, XDATA_WATSUP, XDATA_IRRIG,&
                                  XDATA_LAI_ALL_YEARS  
USE MODD_DATA_COVER_PAR, ONLY : CNAMES, NVEGTYPE
!

USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER, NVT_IRR
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
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER               :: JCOVER,JDEC,JK ! loop counters on covers, decades and vegtypes
INTEGER :: JBEG
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=8), DIMENSION(NVEGTYPE) :: CNVT
 CHARACTER(LEN=2) :: CF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_DATA',0,ZHOOK_HANDLE)
IF (NVT_IRR/=0) THEN
  JBEG = 301
  CNVT(1) =  "NVT_NO  "      ! no vegetation (smooth)
  CNVT(2) =  "NVT_ROCK"      ! no vegetation (rocks)
  CNVT(3) =  "NVT_SNOW"      ! permanent snow and ice
  CNVT(4) =  "NVT_TEBD"      ! temperate broadleaf deciduous trees
  CNVT(5) =  "NVT_BONE"      ! boreal needleleaf evergreen trees 
  CNVT(6) =  "NVT_TRBE"      ! tropical broadleaf evergreen trees
  CNVT(7) =  "NVT_C3  "      ! C3 cultures types
  CNVT(8) =  "NVT_C4  "      ! C4 cultures types
  CNVT(9) =  "NVT_IRR "      ! irrigated crops
  CNVT(10)=  "NVT_GRAS"      ! temperate grassland C3
  CNVT(11)=  "NVT_TROG"      ! tropical  grassland C4
  CNVT(12)=  "NVT_PARK"      ! peat bogs, parks and gardens (irrigated grass)
  CNVT(13)=  "NVT_TRBD"      ! tropical  broadleaf  deciduous trees
  CNVT(14)=  "NVT_TEBE"      ! temperate broadleaf  evergreen trees
  CNVT(15)=  "NVT_TENE"      ! temperate needleleaf evergreen trees
  CNVT(16)=  "NVT_BOBD"      ! boreal    broadleaf  deciduous trees
  CNVT(17)=  "NVT_BOND"      ! boreal    needleleaf deciduous trees
  CNVT(18)=  "NVT_BOGR"      ! boreal grassland C3
  CNVT(19)=  "NVT_SHRB"      ! broadleaf shrub
ELSE
  JBEG = 1
  CNVT(1) =  "NVT_NO  "      ! no vegetation (smooth)
  CNVT(2) =  "NVT_ROCK"      ! no vegetation (rocks)
  CNVT(3) =  "NVT_SNOW"      ! permanent snow and ice
  CNVT(4) =  "NVT_BOBD"      ! boreal    broadleaf  deciduous trees
  CNVT(5) =  "NVT_TEBD"      ! temperate broadleaf deciduous trees
  CNVT(6) =  "NVT_TRBD"      ! tropical  broadleaf  deciduous trees
  CNVT(7) =  "NVT_TEBE"      ! temperate broadleaf  evergreen trees
  CNVT(8) =  "NVT_TRBE"      ! tropical broadleaf evergreen trees
  CNVT(9) =  "NVT_BONE"      ! boreal needleleaf evergreen trees 
  CNVT(10)=  "NVT_TENE"      ! temperate needleleaf evergreen trees
  CNVT(11)=  "NVT_BOND"      ! boreal    needleleaf deciduous trees
  CNVT(12)=  "NVT_GRAS"      ! temperate grassland C3
  CNVT(13)=  "NVT_TROG"      ! tropical  grassland C4
  CNVT(14)=  "NVT_BOGR"      ! boreal grassland C3
  CNVT(15)=  "NVT_SHRB"      ! broadleaf shrub
  CNVT(16)=  "NVT_C3W "      ! winter C3 cultures types
  CNVT(17)=  "NVT_C3E "      ! summer C3 cultures types
  CNVT(18)=  "NVT_C4  "      ! C4 cultures types
  CNVT(19)=  "NVT_FLTR"      ! flooded trees
  CNVT(20)=  "NVT_FLGR"      ! flooded grassland
ENDIF
!
DO JCOVER=JBEG,JPCOVER
WRITE(*,FMT='(A80)') '!-------------------------------------------------------------------------------'
WRITE(*,FMT='(A16,I3.3)') 'SUBROUTINE COVER',JCOVER
WRITE(*,FMT='(A1)') '!'
WRITE(*,FMT='(A10,I3.3)') '!*   cover',JCOVER
WRITE(*,FMT='(A5,A60)')   '!    ',CNAMES(JCOVER,1)
WRITE(*,FMT='(A1)') '!'
WRITE(*,FMT='(A7,I3)') 'ICOVER=',JCOVER
WRITE(*,FMT='(A1)') '!'
WRITE(*,FMT='(A21,F4.2)') 'XDATA_TOWN  (ICOVER)=',XDATA_TOWN(JCOVER)
WRITE(*,FMT='(A21,F4.2)') 'XDATA_NATURE(ICOVER)=',XDATA_NATURE(JCOVER)
WRITE(*,FMT='(A21,F4.2)') 'XDATA_WATER (ICOVER)=',XDATA_WATER(JCOVER)
WRITE(*,FMT='(A21,F4.2)') 'XDATA_SEA   (ICOVER)=',XDATA_SEA(JCOVER)
WRITE(*,FMT='(A1)') '!'
DO JK=1,19
  IF (XDATA_VEGTYPE(JCOVER,JK)==0.) CYCLE
  IF (ALL(XDATA_LAI_ALL_YEARS(JCOVER,:,JK)==0.)) THEN
    WRITE(*,FMT='(A29,A8,A5)') &
           'XDATA_LAI_ALL_YEARS(ICOVER,:,',CNVT(JK),')= 0.'  
    CYCLE
  END IF
  WRITE(*,FMT='(A29,A8,A7)') &
           'XDATA_LAI_ALL_YEARS(ICOVER,:,',CNVT(JK),')= (/ &'  
  DO JDEC=1,18
    CF=', '
    IF (JDEC==18) CF='  '
    WRITE(*,FMT='(A7,12(F4.1,A2),A1)') '       ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+1,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+2,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+3,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+4,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+5,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+6,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+7,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+8,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+9,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+10,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+11,JK),0.1),', ', &
         MAX(XDATA_LAI_ALL_YEARS(JCOVER,(JDEC-1)*12+12,JK),0.1),CF,'&'   
  END DO
  WRITE(*,FMT='(A7)') '     /)'
END DO
WRITE(*,FMT='(A1)') '!'
DO JK=1,19
  IF (XDATA_VEGTYPE(JCOVER,JK)==0.) CYCLE
  WRITE(*,FMT='(A21,A8,A3,F4.2)') &
           'XDATA_VEGTYPE(ICOVER,',CNVT(JK),')= ',XDATA_VEGTYPE(JCOVER,JK)  
END DO
WRITE(*,FMT='(A1)') '!'
DO JK=4,6
  IF (XDATA_VEGTYPE(JCOVER,JK)==0.) CYCLE
  WRITE(*,FMT='(A20,A8,A3,F4.1)') &
           'XDATA_H_TREE(ICOVER,',CNVT(JK),')= ',XDATA_H_TREE(JCOVER,JK)  
END DO
WRITE(*,FMT='(A1)') '!'
DO JK=1,19
  IF (XDATA_VEGTYPE(JCOVER,JK)==0.) CYCLE
  WRITE(*,FMT='(A24,A8,A3,F4.1)') &
           'XDATA_ROOT_DEPTH(ICOVER,',CNVT(JK),')= ',XDATA_ROOT_DEPTH(JCOVER,JK)  
END DO
WRITE(*,FMT='(A1)') '!'
DO JK=1,19
  IF (XDATA_VEGTYPE(JCOVER,JK)==0.) CYCLE
  WRITE(*,FMT='(A26,A8,A3,F4.1)') &
           'XDATA_GROUND_DEPTH(ICOVER,',CNVT(JK),')= ',XDATA_GROUND_DEPTH(JCOVER,JK)  
END DO
WRITE(*,FMT='(A1)') '!'
IF (XDATA_VEGTYPE(JCOVER,9)/=0.) THEN
  WRITE(*,FMT='(A18,A8,A15,I2.2)') &
          'TDATA_SEED(ICOVER,',CNVT(9),')%TDATE%MONTH= ',TDATA_SEED(JCOVER,9)%TDATE%MONTH  
  WRITE(*,FMT='(A18,A8,A15,I2.2)') &
          'TDATA_SEED(ICOVER,',CNVT(9),')%TDATE%DAY  = ',TDATA_SEED(JCOVER,9)%TDATE%DAY  
  WRITE(*,FMT='(A18,A8,A15,I2.2)') &
          'TDATA_REAP(ICOVER,',CNVT(9),')%TDATE%MONTH= ',TDATA_REAP(JCOVER,9)%TDATE%MONTH  
  WRITE(*,FMT='(A18,A8,A15,I2.2)') &
          'TDATA_REAP(ICOVER,',CNVT(9),')%TDATE%DAY  = ',TDATA_REAP(JCOVER,9)%TDATE%DAY  
  WRITE(*,FMT='(A20,A8,A3,F4.1)') &
           'XDATA_WATSUP(ICOVER,',CNVT(9),')= ',XDATA_WATSUP(JCOVER,9)  
  WRITE(*,FMT='(A20,A8,A3,F4.1)') &
           'XDATA_IRRIG (ICOVER,',CNVT(9),')= ',XDATA_IRRIG (JCOVER,9)  
END IF
WRITE(*,FMT='(A20,I3.3)') 'END SUBROUTINE COVER',JCOVER
END DO
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
DO JCOVER=JBEG,JPCOVER
  WRITE(*,FMT='(A10,I3.3)') 'CALL COVER',JCOVER
END DO
IF (LHOOK) CALL DR_HOOK('WRITE_DATA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DATA
