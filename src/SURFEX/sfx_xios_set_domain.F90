!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_SET_DOMAIN(HGRID, HNAME, KDIM1, KDIM2, KEXT1, KINDEX,&
                         ODMASK, PLON, PLAT, PCORNER_LON, PCORNER_LAT, KMASK) 
#ifdef WXIOS 
!!
!!
!!     PURPOSE
!!     --------
!!
!!
!!     Initialize an XIOS domain, representing Surfex packing for a
!!     tile (or full domain) Declare Surfex grids and masks for XIOS,
!!     accounting for 2D and 1D geometries
!!
!!
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     LXIOS, YXIOS_CONTEXT, TXIOS_CONTEXT, LGAUSS
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 :
!!       svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> ; 
!!       cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2015
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!-------------------------------------------------------------------------------
!
USE XIOS     ,ONLY : XIOS_DOMAIN, XIOS_DOMAINGROUP, XIOS_AXISGROUP, XIOS_AXIS, &
                     XIOS_GET_HANDLE, XIOS_ADD_CHILD, XIOS_SET_DOMAIN_ATTR,    &
                     XIOS_IS_DEFINED_DOMAIN_ATTR
!
USE MODI_GET_SURF_GRID_DIM_n
USE MODI_LATLON_GRIDTYPE_LONLAT_REG
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(IN) :: HGRID
 CHARACTER(LEN=*), INTENT(IN) :: HNAME  ! Name to set in XIOS for the Surfex domain/tile
INTEGER, INTENT(IN)          :: KDIM1  ! Global grid size for dimension 1 (incl. extension zone if any)
                                       ! Can be set to O for offline case
INTEGER, INTENT(IN)          :: KDIM2  ! Global grid size for dimension 2
INTEGER, INTENT(IN)          :: KEXT1  ! Size of extension zone if any (Aladin : NEXTI)
                                       ! Should be O for offline case
!
INTEGER, INTENT(IN), DIMENSION(:)           :: KINDEX ! Index of the MPI-task cells 
                                                      ! in global 1D grid (from 0)
LOGICAL, INTENT(IN), DIMENSION(:)           :: ODMASK ! mask for the MPI-task cells 
REAL   , INTENT(IN), DIMENSION(:),OPTIONAL  :: PLON ! Longitudes for the MPI-task cells
REAL   , INTENT(IN), DIMENSION(:),OPTIONAL  :: PLAT ! Latitudes  for the MPI-task cells
REAL   , INTENT(IN), DIMENSION(:,:),OPTIONAL:: PCORNER_LON, PCORNER_LAT 
INTEGER, INTENT(IN), DIMENSION(:),OPTIONAL  :: KMASK ! Local Surfex packing mask for the tile
!
TYPE(xios_domaingroup) :: domaingroup_hdl
TYPE(xios_domain)      :: domain_hdl
!
INTEGER             :: ISIZE             ! Number of points for the MPI-task (among all tiles)
LOGICAL             :: GRECT             ! T if rectangular grid (inc Aladin)
INTEGER             :: JK
!
INTEGER, DIMENSION(:),ALLOCATABLE :: IINDEX, JINDEX   
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SET_DOMAIN',0,ZHOOK_HANDLE)
!
!$OMP SINGLE
!
CALL XIOS_GET_HANDLE("domain_definition",domaingroup_hdl)
CALL XIOS_ADD_CHILD(domaingroup_hdl,domain_hdl,HNAME)
!
ISIZE = SIZE(KINDEX)
!
!
GRECT = (KDIM2/=1)
!
IF (GRECT) THEN 
   !
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, ni_glo=KDIM1-KEXT1, nj_glo=KDIM2)
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, data_dim=1, ibegin=0)
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, ni=ISIZE)
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, nj=1, data_nj=1)    ! To account for XIOS bugs 
   ! Must build i_index and j_index from kindex
   ALLOCATE(IINDEX(ISIZE), JINDEX(ISIZE))
   !
   DO JK = 1,ISIZE
         IINDEX(JK)=MOD(KINDEX(JK),KDIM1-KEXT1) 
         JINDEX(JK)=KINDEX(JK)/(KDIM1-KEXT1)  
         IF (JINDEX(JK) .GT. KDIM2-1 ) &
              CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : Inconsistent jindex")
   ENDDO
   !
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, i_index=IINDEX, j_index=JINDEX)
   DEALLOCATE(IINDEX, JINDEX)
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, mask_1d=ODMASK)
   !
   IF (PRESENT(KMASK)) THEN 
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, data_i_index=KMASK(:), data_ni=size(KMASK))
   ENDIF
   !
   !
   ! Process lat/lon and their corners
   !
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, lonvalue_1d=PLON,latvalue_1d=PLAT)
   IF (HGRID=="LONLAT REG")  THEN 
     !
     CALL XIOS_SET_DOMAIN_ATTR(HNAME, type='rectilinear')
     CALL XIOS_SET_DOMAIN_ATTR(HNAME, lonvalue_1d=PLON,latvalue_1d=PLAT)
     !
     !CALL XIOS_SET_DOMAIN_ATTR(HNAME, type='regular')
     !IDIM=KDIM1*KDIM2
     !ALLOCATE(ZLAT(IDIM),ZLON(IDIM),ZMESH(IDIM),ZDIR(IDIM))
     !CALL LATLON_GRIDTYPE_LONLAT_REG(SIZE(UG%XGRID_PAR),IDIM,&
     !     UG%XGRID_PAR,ZLAT,ZLON,ZMESH,ZDIR)
     !CALL XIOS_SET_DOMAIN_ATTR(HNAME, lonvalue_1d=ZLON(1:KDIM1-KEXT1))
     !CALL XIOS_SET_DOMAIN_ATTR(HNAME, latvalue_1d=(/(ZLAT(KK),KK=1,IDIM,KDIM1)/)  )
     !DEALLOCATE(ZLAT,ZLON,ZMESH,ZDIR)     
     CALL XIOS_SET_DOMAIN_ATTR(HNAME, type='rectilinear')
     !
   ELSE
      IF (HGRID/="CONF PROJ ") &
           PRINT*,"SFX_XIOS_SET_DOMAIN : Managing "//HGRID//" type grid is not yet tested "
      !
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, type="curvilinear")
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, lonvalue_1d=PLON,latvalue_1d=PLAT)
      IF (PRESENT(PCORNER_LAT) .AND. (HGRID /='CARTESIAN')) THEN 
         CALL XIOS_SET_DOMAIN_ATTR(HNAME, nvertex=4, &
              bounds_lon_1d=PCORNER_LON,bounds_lat_1d=PCORNER_LAT)
      ENDIF      
      !
   ENDIF
   !
ELSE
   ! For 1D global grids (such as Gaussian reduced), just provide
   ! KINDEX, the local array of global cell indices
   !
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, type='unstructured', data_dim=1, ni_glo=KDIM1*KDIM2)
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, ibegin=0)
   if (maxval(kindex) > KDIM1*KDIM2-1 )  CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : maxval(i_index)")
   if (minval(kindex) < 0 )              CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : minval(i_index)")
   CALL XIOS_SET_DOMAIN_ATTR(HNAME, i_index=KINDEX, ni=size(KINDEX) )
   ! CALL XIOS_SET_DOMAIN_ATTR(HNAME, mask_1d=LDMASK)
   IF (PRESENT(KMASK)) THEN 
      !
      ! Use XIOS 'compression' feature to account for Surfex 'packing' 
      !
      !write(0,*) 'declaring '//trim(yname)//' with sizes : ',ISIZE,SIZE(KMASK), minval(kindex),maxval(kindex)&
      !     , minval(kmask),maxval(kmask)
      !call flush(0)
      if ( size(kmask) > 0 ) then 
         if (size(kmask) > size(KINDEX))       CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : size(kmask))")
         if (maxval(kmask) > size(kindex)-1 )  CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : maxval(data_i_index)")
         if (minval(kmask) < 0 )               CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : minval(data_i_index)")
      else
         !write(0,*) 'zero-size domain '//trim(yname)
      endif
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, data_i_index=KMASK, data_ni=size(KMASK))
      !ELSE
      !write(0,*) 'declaring '//trim(yname)//' with sizes : ',ISIZE, minval(kindex),maxval(kindex)
      !call flush(0)
   ENDIF
   !
   ! Process lat/lon and their corners
   !
   IF (PRESENT(PLAT) .AND. PRESENT(PLON)) THEN
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, lonvalue_1d=PLON,latvalue_1d=PLAT)
   ELSE
      CALL ABOR1_SFX("SFX_XIOS_SET_DOMAIN : Must provide lat and lon")
   ENDIF
   IF (PRESENT(PCORNER_LAT) .AND. PRESENT(PCORNER_LON) .AND. &
        (HGRID/='CARTESIAN' )) THEN 
      CALL XIOS_SET_DOMAIN_ATTR(HNAME, nvertex=4, &
           bounds_lat_1d=PCORNER_LAT, bounds_lon_1d=PCORNER_LON )
   ENDIF
ENDIF
!
!$OMP END SINGLE
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SET_DOMAIN',1,ZHOOK_HANDLE)
!
#endif
END SUBROUTINE SFX_XIOS_SET_DOMAIN
