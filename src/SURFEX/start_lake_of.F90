!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######
SUBROUTINE START_LAKE_OF(KDAY, KMONTH, PLON, PLAT, PDEPTH, &   ! IN
                     PT_SNOW, PT_ICE, PT_MNW, PT_WML, PT_BOT, PT_B1, PCT, & ! Lake variables
                     PH_SNOW, PH_ICE, PH_ML, PH_B1, PT_SFC)                 ! OUT
!     ###############
!
! PURPOSE: Extract the climate lake variables from the climate lake dataset 
!          for the given date, the given grid box of the atmopheric model grid in lon-lat
!          and for the given lake depth;
!          this version is for netcdf3 (old-fashioned)
! AUTHOR: Ekaterina Kourzeneva, 
!         Meteo France, 2010
! INPUT:  KDAY - the day number 
!         KMONTH - the month number
!         PLON - longitude of the center of the atmospheric model grid box, deg. dec., -180.0 ... 180.0
!         PLAT - latitude of the center of the atmospheric model grid box, deg. dec., -90.0 ... 90.0
!         PDEPTH - the lake depth in the grid box, m 
! LIBRARIES: NetCDF. Install it and make sure you use right compilation and linkage lines!
!                    (see the example for details)
! READS FILES: data for the lake climatology (LAKE_LTA.nc)
! Be sure that all necessary files are present and not zipped!!!
! OUTPUT: PT_SNOW - the snow temperature, K (no snow at present, so equal to the ice temperature) 
!         PT_ICE - the ice temperature, K
!         PT_MNW - the mean water temperature, K
!         PT_WML - the mixed layer temperature, K
!         PT_BOT - the bottom temperature, K
!         PT_B1 - the temperature on the outer edge of the active layer of the bottom sediments, K
!                (at present the bottom sediments block is not used, 
!                 so eq. to the freshwater maximum density temperature)
!         PCT - the shape factor, dimensioneless
!         PH_SNOW - the snow depth, m (no snow at present, so equal to zero)
!         PH_ICE - the ice depth, m
!         PH_ML - the mixed layer depth, m
!         PH_B1 - the depth of the active layer if the bottom sediments, m
!                (at present the bottom sediments block is not used, 
!                 so eq. to the dummy value)  
!         PT_SFC - the surface temperature, K (the diagnostic value, so just for information)
! WRITES FILES: no
!
!------------------------------------------------------------------------------------------------------------
!         Modified   07/2012, P. Le Moigne : In case there's a lake but no climatic data
!                                    associated then fill with neighbour existing data
!                                    instead of aborting
!------------------------------------------------------------------------------------------------------------
!
USE MODD_DATA_LAKE, ONLY : CLAKELTA, NLONG, NLATG, XFIRSTLAT, &
                           XC_SMALL, NGRADDEPTH_LTA, XCENTRGRADDEPTH_LTA, &
                           XAUXT_SNOW, XAUXT_ICE, XAUXT_MNW, XAUXT_WML, XAUXT_BOT, &
                           XAUXT_B1, XAUXCT, XAUXH_SNOW, XAUXH_ICE, XAUXH_ML, &
                           XAUXH_B1, XAUXT_SFC
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN) :: KDAY,   & ! The day number
                       KMONTH    ! The month number
REAL, INTENT(IN) :: PLON, PLAT   ! Longitude and latitude of the center of the atmospheric model grid box, 
                                 ! deg. dec. (-180.0 ... 180.0), (-90.0 ... 90.0)
REAL, INTENT(IN) :: PDEPTH       ! The depth of the lakes, m
REAL, INTENT(OUT) :: PT_SNOW, &  ! the snow temperature, K (no snow at present, so equal to the ice temperature) 
                     PT_ICE,  &  ! the ice temperature, K
                     PT_MNW,  &  ! the mean water temperature, K
                     PT_WML,  &  ! the mixed layer temperature, K
                     PT_BOT,  &  ! the bottom temperature, K
                     PT_B1,   &  ! the temperature on the outer edge of the active layer of the bottom sediments, K
                                 !  (at present the bottom sediments block is not used, 
                                 !  so eq. to the freshwater maximum density temperature)
                     PCT,     &  ! the shape factor, dimensioneless
                     PH_SNOW, &  ! the snow depth, m (no snow at present, so equal to zero)
                     PH_ICE,  &  ! the ice depth, m
                     PH_ML,   &  ! the mixed layer depth, m
                     PH_B1,   &  ! the depth of the active layer if the bottom sediments, m
                                 !  (at present the bottom sediments block is not used, 
                                 !   so eq. to the dummy value)  
                     PT_SFC      ! the surface temperature, K (the diagnostic value, so just for information) 
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(1,1) :: ZWT_SNOW, ZWT_ICE, ZWT_MNW, ZWT_WML, ZWT_BOT, ZWT_B1, ZWCT, & ! Lake values
                         ZWH_SNOW, ZWH_ICE, ZWH_ML, ZWH_B1, ZWT_SFC                  ! to read from NetCDF                 
REAL :: ZFT_SNOW, ZFT_ICE, ZFT_MNW, ZFT_WML, ZFT_BOT, ZFT_B1, ZFCT, &
        ZFH_SNOW, ZFH_ICE, ZFH_ML, ZFH_B1, ZFT_SFC
REAL, DIMENSION(NGRADDEPTH_LTA) :: ZDISTD
REAL :: ZWLON, ZWLAT, ZWDEPTH
!
LOGICAL :: LEXIST
!
 INTEGER :: ID_LAKELTA, ID_MONTH, &  ! IDs for NetCDF 
            ID_DEC, ID_LON, ID_LAT, ID_DEPTH, &
            ID_T_SNOW, ID_T_ICE, ID_T_MNW, ID_T_WML, ID_T_BOT, ID_T_B1, ID_CT, &
            ID_H_SNOW, ID_H_ICE, ID_H_ML, ID_H_B1, ID_T_SFC
 INTEGER :: IDEC, IMONTH ! Number of the decade and the month
 INTEGER :: ILON, ILAT ! Numbers of the "lake" grid boxes in longitude and latitude
 INTEGER :: IDEPTH ! Number of the lake class in depth
 INTEGER, DIMENSION(1) :: ILOC_DEPTH
 INTEGER :: IRET
 INTEGER :: IMONTHN, IDECN, ILONN, ILATN, IDEPTHN
 INTEGER, DIMENSION(5) :: NINDEX
 REAL(KIND=JPRB) :: ZHOOK_HANDLE
! ----------------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('START_LAKE_OF',0,ZHOOK_HANDLE)
!
!*      0.     Check
!
IF(KDAY.LE.0 .OR. KDAY.GT.31) CALL ABOR1_SFX("START_LAKE_OF: WRONG DAY NUMBER (!!!!)")
IF(KMONTH.LE.0 .OR. KMONTH.GT.12) CALL ABOR1_SFX("START_LAKE_OF: WRONG MONTH NUMBER (!!!!)")
IF(PLON.LT.-180. .OR. PLON.GT.180.) CALL ABOR1_SFX("START_LAKE_OF: WRONG LONGITUDE (!!!!)")
IF(PLAT.LT.-90. .OR. PLAT.GT.90.) CALL ABOR1_SFX("START_LAKE_OF: WRONG LATITUDE (!!!!)")
!
!*      1.     Calculate time indexes 
!
IMONTH = KMONTH
!
SELECT CASE(KDAY)
  CASE(1:10)
    IDEC=1
  CASE(11:20)
    IDEC=2
  CASE(21:31)
    IDEC=3
END SELECT
!
!*      2.     Calculate lon-lat indexes
!
ZWLON = PLON
ZWLAT = MAX(PLAT,XFIRSTLAT)
!
ILON = NINT(ZWLON) + NLONG/2 + 1
ILAT = NINT(ZWLAT) - XFIRSTLAT + 1
!
!*      3.     Check lake depth
!
IF (PDEPTH.LT.XC_SMALL) &
  CALL ABOR1_SFX("START_LAKE_OF: YOUR LAKE DEPTH IS TOO SMALL TO ACTIVATE CLI_LAKE IN SOME POINTS")
!
!*      4.     Calculate the appropriate index
!
ZWDEPTH = MIN(PDEPTH,50.)
ZDISTD = ABS(XCENTRGRADDEPTH_LTA(:) - ZWDEPTH)
ILOC_DEPTH = MINLOC(ZDISTD)
IDEPTH = ILOC_DEPTH(1)
!
!*      5.     Open file for reading
!
IRET = NF90_OPEN(TRIM(ADJUSTL(CLAKELTA)),NF90_NOWRITE,ID_LAKELTA)
IF (IRET.NE.0) CALL ABOR1_SFX("START_LAKE_OF: WRONG OR NO LAKE DATA FILE")
!
!*      6.     Check data in the lake file
!
IRET = NF90_INQ_DIMID(ID_LAKELTA, "NMonth", ID_MONTH)
IRET = NF90_INQUIRE_DIMENSION(ID_LAKELTA, ID_MONTH, LEN=IMONTHN)
IF (IMONTHN.NE.12) CALL ABOR1_SFX("START_LAKE_OF: NUMBER OF MONTHS IN THE LAKE FILE IS NOT 12????") 
!
IRET = NF90_INQ_DIMID(ID_LAKELTA, "NDec", ID_DEC)
IRET = NF90_INQUIRE_DIMENSION(ID_LAKELTA, ID_DEC, LEN=IDECN)
IF (IDECN.NE.3) CALL ABOR1_SFX("START_LAKE_OF: NUMBER OF DECADES IN MONTH IN THE LAKE FILE IS NOT 3????")
!
IRET = NF90_INQ_DIMID(ID_LAKELTA, "NLon", ID_LON)  
IRET = NF90_INQUIRE_DIMENSION(ID_LAKELTA, ID_LON, LEN=ILONN)
IF (ILONN.NE.NLONG) CALL ABOR1_SFX("START_LAKE_OF: WRONG NUMBER OF POINTS IN LONGITUDE IN THE LAKE FILE!")
!
IRET = NF90_INQ_DIMID(ID_LAKELTA, "NLat", ID_LAT)    
IRET = NF90_INQUIRE_DIMENSION(ID_LAKELTA, ID_LAT, LEN=ILATN)
IF (ILATN.NE.NLATG) CALL ABOR1_SFX("START_LAKE_OF: WRONG NUMBER OF POINTS IN LONGITUDE IN THE LAKE FILE!")
!
IRET = NF90_INQ_DIMID(ID_LAKELTA, "NDepth", ID_DEPTH)
IRET = NF90_INQUIRE_DIMENSION(ID_LAKELTA, ID_DEPTH, LEN=IDEPTHN)
IF (IDEPTHN.NE.NGRADDEPTH_LTA) CALL ABOR1_SFX("START_LAKE_OF: WRONG NUMBER OF GRADATIONS IN DEPTH IN THE LAKE FILE!")
!
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_snow", ID_T_SNOW)
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_ice", ID_T_ICE)
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_mnw", ID_T_MNW) 
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_wML", ID_T_WML)
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_bot", ID_T_BOT)
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_B1", ID_T_B1)
IRET = NF90_INQ_VARID(ID_LAKELTA, "C_T", ID_CT)
IRET = NF90_INQ_VARID(ID_LAKELTA, "h_snow", ID_H_SNOW)
IRET = NF90_INQ_VARID(ID_LAKELTA, "h_ice", ID_H_ICE)
IRET = NF90_INQ_VARID(ID_LAKELTA, "h_ML", ID_H_ML)  
IRET = NF90_INQ_VARID(ID_LAKELTA, "H_B1", ID_H_B1)
IRET = NF90_INQ_VARID(ID_LAKELTA, "T_sfc", ID_T_SFC)
!
!*      7.     Reading
!
NINDEX(1) = IMONTH
NINDEX(2) = IDEC
NINDEX(3) = ILON
NINDEX(4) = ILAT
NINDEX(5) = IDEPTH
!
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_SNOW, '_FillValue',ZFT_SNOW)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_ICE, '_FillValue',ZFT_ICE)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_MNW, '_FillValue',ZFT_MNW)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_WML, '_FillValue',ZFT_WML)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_BOT, '_FillValue',ZFT_BOT)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_B1, '_FillValue',ZFT_B1)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_CT, '_FillValue',ZFCT)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_H_SNOW, '_FillValue',ZFH_SNOW)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_H_ICE, '_FillValue',ZFH_ICE)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_H_ML, '_FillValue',ZFH_ML)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_H_B1, '_FillValue',ZFH_B1)
IRET = NF90_GET_ATT(ID_LAKELTA, ID_T_SNOW, '_FillValue',ZFT_SFC)
!
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_SNOW, ZWT_SNOW, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_ICE, ZWT_ICE, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_MNW, ZWT_MNW, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_WML, ZWT_WML, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_BOT, ZWT_BOT, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_B1, ZWT_B1, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_CT, ZWCT, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_H_SNOW, ZWH_SNOW, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_H_ICE, ZWH_ICE, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_H_ML, ZWH_ML, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_H_B1, ZWH_B1, NINDEX)
IRET = NF90_GET_VAR(ID_LAKELTA, ID_T_SFC, ZWT_SFC, NINDEX)
!
!*      8.     Close file
!
IRET = NF90_CLOSE(ID_LAKELTA)   
!
!*      9.     Make output
!

LEXIST=(ZWT_SNOW(1,1).NE.ZFT_SNOW .AND. ZWT_ICE(1,1).NE.ZFT_ICE .AND. ZWT_MNW(1,1).NE.ZFT_MNW .AND. &
        ZWT_WML(1,1).NE.ZFT_WML .AND. ZWT_BOT(1,1).NE.ZFT_BOT .AND. ZWT_B1(1,1).NE.ZFT_B1 .AND. &
        ZWCT(1,1).NE.ZFCT .AND. ZWH_SNOW(1,1).NE.ZFH_SNOW .AND. ZWH_ICE(1,1).NE.ZFH_ICE .AND. &
         ZWH_ML(1,1).NE.ZFH_ML .AND. ZWH_B1(1,1).NE.ZFH_B1 .AND. ZWT_SFC(1,1).NE.ZFT_SFC)

IF (LEXIST) THEN
  !
  XAUXT_SNOW = ZWT_SNOW(1,1)
  XAUXT_ICE = ZWT_ICE(1,1) 
  XAUXT_MNW = ZWT_MNW(1,1) 
  XAUXT_WML = ZWT_WML(1,1)
  XAUXT_BOT = ZWT_BOT(1,1)
  XAUXT_B1 = ZWT_B1(1,1)
  XAUXCT = ZWCT(1,1) 
  XAUXH_SNOW = ZWH_SNOW(1,1)
  XAUXH_ICE = ZWH_ICE(1,1)
  XAUXH_ML = ZWH_ML(1,1)
  XAUXH_B1 = ZWH_B1(1,1)
  XAUXT_SFC = ZWT_SFC(1,1)
  !
ENDIF
!
PT_SNOW = XAUXT_SNOW 
PT_ICE  = XAUXT_ICE 
PT_MNW  = XAUXT_MNW 
PT_WML  = XAUXT_WML 
PT_BOT  = XAUXT_BOT 
PT_B1   = XAUXT_B1  
PCT     = XAUXCT 
PH_SNOW = XAUXH_SNOW 
PH_ICE  = XAUXH_ICE 
PH_ML   = XAUXH_ML 
PH_B1   = XAUXH_B1 
PT_SFC  = XAUXT_SFC
!
IF (LHOOK) CALL DR_HOOK('START_LAKE_OF',1,ZHOOK_HANDLE)
!
END SUBROUTINE START_LAKE_OF
