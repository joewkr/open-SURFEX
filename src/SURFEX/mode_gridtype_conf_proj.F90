!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_CONF_PROJ
!     ##############################
!
!############################################################################
!############################################################################
!############################################################################
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_GRIDTYPE_CONF_PROJ(PGRID_PAR,PLAT0,PLON0,PRPK,PBETA,&
                                          PLATOR,PLONOR,KIMAX,KJMAX,       &
                                          PX,PY,PDX,PDY                    )  
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_CONF_PROJ* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!        M.Moge    06/2015 broadcast the space step to all MPI processes (necessary for reproductibility)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
#ifdef MNH_PARALLEL
USE MODD_VAR_ll, ONLY : NPROC, IP, MPI_PRECISION, NMNH_COMM_WORLD, YSPLITTING
USE MODD_MPIF
USE MODE_TOOLS_ll, ONLY : GET_OR_ll
#endif
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL,               INTENT(IN)  :: PLAT0    ! reference latitude
REAL,               INTENT(IN)  :: PLON0    ! reference longitude
REAL,               INTENT(IN)  :: PRPK     ! projection parameter 
!                                           !   K=1 : stereographic north pole
!                                           ! 0<K<1 : Lambert, north hemisphere
!                                           !   K=0 : Mercator
!                                           !-1<K<0 : Lambert, south hemisphere
!                                           !   K=-1: stereographic south pole
REAL,               INTENT(IN)  :: PBETA    ! angle between grid and reference longitude
REAL,               INTENT(IN)  :: PLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL,               INTENT(IN)  :: PLONOR   ! longitude of point of coordinates X=0, Y=0
INTEGER,            INTENT(IN)  :: KIMAX    ! number of points in I direction
INTEGER,            INTENT(IN)  :: KJMAX    ! number of points in J direction
REAL, DIMENSION(:), INTENT(IN)  :: PX       ! X conformal coordinate of left boundary of grid mesh
REAL, DIMENSION(:), INTENT(IN)  :: PY       ! Y conformal coordinate of bottom boundary of grid mesh
REAL, DIMENSION(:), INTENT(IN)  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(IN)  :: PDY      ! Y grid mesh size
REAL, DIMENSION(:), POINTER     :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL                         :: GFULL    ! T : entire grid is stored
INTEGER                         :: IL       ! number of points
INTEGER                         :: JJ       ! loop counter
!
#ifdef SFX_MNH
INTEGER                         :: IINFO_ll
INTEGER                         :: IXOR, IYOR
INTEGER                         :: IXORMIN, IYORMIN
INTEGER                         :: IROOT, IROOTPROC
#endif
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:PUT_GRIDTYPE_CONF_PROJ',0,ZHOOK_HANDLE)
!
GFULL = (KIMAX*KJMAX==SIZE(PX))
!
IL = SIZE(PX)
!
IF (GFULL) THEN
  !* entire grid : one can store only X and Y cooridnate arrays in each direction
  ALLOCATE(PGRID_PAR(12+KIMAX+KJMAX))
ELSE
  !* only some points are present : one store the coordinates of all points
  ALLOCATE(PGRID_PAR(12+2*IL))
END IF
!
PGRID_PAR(1) = PLAT0
PGRID_PAR(2) = PLON0
PGRID_PAR(3) = PRPK
PGRID_PAR(4) = PBETA
PGRID_PAR(5) = PLATOR
PGRID_PAR(6) = PLONOR
PGRID_PAR(7) = FLOAT(KIMAX)
PGRID_PAR(8) = FLOAT(KJMAX)
IF (IL>0) THEN
  PGRID_PAR(9) = PDX(1)
  PGRID_PAR(10)= PDY(1)
ENDIF
!
#ifdef MNH_PARALLEL
!get the index of the process with IL>0 that own the southmost and the westmost point
!then broadcast the value of PDX and PDY at these points
IF ( NPROC > 1 ) THEN
! we need to determine wich processes own the southmost and the westmost point
  CALL GET_OR_ll( YSPLITTING, IXOR, IYOR )
  IF ( IL==0 ) THEN ! we don't consider processes with IL==0
    IXOR = NUNDEF
    IYOR = NUNDEF
  ENDIF
  ! get the processes with IL>0 with the westmost points
  CALL MPI_ALLREDUCE(IXOR, IXORMIN, 1, MPI_INTEGER, MPI_MIN, NMNH_COMM_WORLD, IINFO_ll) 
  IF ( IXOR == IXORMIN ) THEN
    IROOT = IP-1
  ELSE
    IROOT = NPROC
  ENDIF
  CALL MPI_ALLREDUCE(IROOT, IROOTPROC, 1, MPI_INTEGER, MPI_MIN, NMNH_COMM_WORLD, IINFO_ll) 
! Then this process broadcasts the space steps in X direction in order to have the same space steps on all processes
  CALL MPI_BCAST(PGRID_PAR(9), 1, MPI_PRECISION, IROOTPROC, NMNH_COMM_WORLD, IINFO_ll)
  !
  ! get the processes with IL>0 with the southmost points
  CALL MPI_ALLREDUCE(IYOR, IYORMIN, 1, MPI_INTEGER, MPI_MIN, NMNH_COMM_WORLD, IINFO_ll) 
  IF ( IYOR == IYORMIN ) THEN
    IROOT = IP-1
  ELSE
    IROOT = NPROC
  ENDIF
  CALL MPI_ALLREDUCE(IROOT, IROOTPROC, 1, MPI_INTEGER, MPI_MIN, NMNH_COMM_WORLD, IINFO_ll) 
! Then this process broadcasts the space steps in Y direction in order to have the same space steps on all processes
  CALL MPI_BCAST(PGRID_PAR(10), 1, MPI_PRECISION, IROOTPROC, NMNH_COMM_WORLD, IINFO_ll)
ENDIF
#endif
!
IF (IL<=0) THEN
  PGRID_PAR(9) = XUNDEF
  PGRID_PAR(10)= XUNDEF
END IF
PGRID_PAR(11) = SIZE(PX)
IF (GFULL) THEN
  PGRID_PAR(12) = 1
ELSE
  PGRID_PAR(12) = 0
END IF
!
IF (GFULL) THEN
  PGRID_PAR(12     +1:12+KIMAX) = PX(1:KIMAX)
  DO JJ=1,KJMAX
    PGRID_PAR(12+KIMAX+JJ) = PY(1+(JJ-1)*KIMAX)
  END DO
ELSE
  IF (IL>0) THEN
    PGRID_PAR(12     +1:12+  IL) = PX(:)
    PGRID_PAR(12+  IL+1:12+2*IL) = PY(:)
  END IF
END IF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:PUT_GRIDTYPE_CONF_PROJ',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_CONF_PROJ
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_CONF_PROJ(PGRID_PAR,PLAT0,PLON0,PRPK,PBETA,&
                                          PLATOR,PLONOR,KIMAX,KJMAX,       &
                                          PX,PY,PDX,PDY,KL                 )  
!     ####################################################################
!
!!****  *GET_GRIDTYPE_CONF_PROJ* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)            :: PGRID_PAR! parameters defining this grid
REAL,               INTENT(OUT), OPTIONAL :: PLAT0    ! reference latitude
REAL,               INTENT(OUT), OPTIONAL :: PLON0    ! reference longitude
REAL,               INTENT(OUT), OPTIONAL :: PRPK     ! projection parameter 
!                                                     !   K=1 : stereographic north pole
!                                                     ! 0<K<1 : Lambert, north hemisphere
!                                                     !   K=0 : Mercator
!                                                     !-1<K<0 : Lambert, south hemisphere
!                                                     !   K=-1: stereographic south pole
REAL,               INTENT(OUT), OPTIONAL :: PBETA    ! angle between grid and reference longitude
REAL,               INTENT(OUT), OPTIONAL :: PLATOR   ! latitude  of point of coordinates X=0, Y=0
REAL,               INTENT(OUT), OPTIONAL :: PLONOR   ! longitude of point of coordinates X=0, Y=0
INTEGER,            INTENT(OUT), OPTIONAL :: KIMAX    ! number of points in I direction
INTEGER,            INTENT(OUT), OPTIONAL :: KJMAX    ! number of points in J direction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PX       ! X conformal coor. of grid mesh 
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PY       ! Y conformal coor. of grid mesh
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PDY      ! Y grid mesh size
INTEGER,            INTENT(OUT), OPTIONAL :: KL       ! number of points
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL, IIMAX, IJMAX
INTEGER :: JI, JJ ! loop counters
LOGICAL :: GFULL    ! T : entire grid is stored
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:GET_GRIDTYPE_CONF_PROJ',0,ZHOOK_HANDLE)
!
IL    = PGRID_PAR(11)
IIMAX = NINT(PGRID_PAR(7))
IJMAX = NINT(PGRID_PAR(8))
!
IF (PRESENT(PLAT0))  PLAT0 = PGRID_PAR(1)
IF (PRESENT(PLON0))  PLON0 = PGRID_PAR(2)
IF (PRESENT(PRPK ))  PRPK  = PGRID_PAR(3)
IF (PRESENT(PBETA))  PBETA = PGRID_PAR(4)
IF (PRESENT(PLATOR)) PLATOR= PGRID_PAR(5)
IF (PRESENT(PLONOR)) PLONOR= PGRID_PAR(6)
IF (PRESENT(KIMAX))  KIMAX = IIMAX
IF (PRESENT(KJMAX))  KJMAX = IJMAX
IF (PRESENT(PDX))    PDX(:)= PGRID_PAR(9)
IF (PRESENT(PDY))    PDY(:)= PGRID_PAR(10)
IF (PRESENT(KL))     KL    = IL
!
GFULL = (PGRID_PAR(12)==1)
!
IF (PRESENT(PX)) THEN
  IF (GFULL) THEN
    DO JJ=1,IJMAX
      DO JI=1,IIMAX
        PX(JI+(JJ-1)*IIMAX) = PGRID_PAR(12+JI)
      END DO
    END DO
  ELSE
    PX(:) = PGRID_PAR(12+1:12+IL)
  END IF        
END IF
IF (PRESENT(PY)) THEN
  IF (GFULL) THEN
    DO JJ=1,IJMAX
      DO JI=1,IIMAX
        PY(JI+(JJ-1)*IIMAX) = PGRID_PAR(12+IIMAX+JJ)
      END DO
    END DO
  ELSE
    PY(:) = PGRID_PAR(12+IL+1:12+2*IL)
  END IF        
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:GET_GRIDTYPE_CONF_PROJ',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_CONF_PROJ
!############################################################################
!############################################################################
!############################################################################
!      ###################################################
       SUBROUTINE LATLON_CONF_PROJ(PLAT0,PLON0,PRPK,PBETA,PLATOR,PLONOR, &
                                     PX,PY,PLAT,PLON                       )  
!      ###################################################
!
!!****  *LATLON_CONF_PROJ * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!        This routine computes the latitude and longitude of
!      an array given in cartesian conformal coordinates
!        Five map projections are available: 
!      - polar-stereographic from south pole  (PRPK=1),
!      - lambert conformal from south pole  (0<PRPK<1),
!      - mercator                             (PRPK=0),
!      - lambert conformal from north pole (-1<PRPK<0),
!      - polar-stereographic from north pole  (PRPK=-1).
!
!
!!**   METHOD
!!     ------
!!       Spherical earth approximation is used. Longitude origin is 
!!     set in Greenwich, and is positive eastwards. An anticlockwise 
!!     rotation of PBETA degrees is applied to the conformal frame 
!!     with respect to the geographical directions.
!!
!!       WARNING: ALL INPUT AND OUTPUT ANGLES ARE IN DEGREES...
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!      Asencio N. et al., 1994, "Le projet de modele non-hydrostatique
!!            commun CNRM-LA, specifications techniques", 
!!            Note CNRM/GMME, 26, 139p, (Chapter 2).
!!      Ducrocq V., 1994, "Generation de la grille dans le modele",
!!            Note interne MNH, 5 mai, 3p.
!!      Joly A., 1992, "Geographic parameters for ARPEGE/ALADIN",
!!            Internal note ARPEGE/ALADIN, february 27,28p.
!!      Levallois J., 1970, "Geodesie generale", Tome 2, Collection
!!             de l'IGN, Eyrolles, Paris, 408p.
!!       
!!     AUTHOR
!!     ------
!!      P.M.       *LA*
!!
!!     MODIFICATION
!!     ------------
!!       Original  PM  24/05/94
!!       Updated   PM  27/07/94
!!       Updated   VD  23/08/94
!!       Updated   VM  24/10/95 projection from north pole (PRPK<0) and 
!!                              longitudes set between PLON0-180. and PLON0+180.
!!       Update    VM  11/2004  externalized version
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_CSTS,ONLY : XPI, XRADIUS
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,                 INTENT(IN) :: PLAT0  ! Reference latitude
REAL,                 INTENT(IN) :: PLON0  ! Reference longitude
REAL,                 INTENT(IN) :: PRPK   ! projection parameter 
!                                          !   K=1 : stereographic north pole
!                                          ! 0<K<1 : Lambert, north hemisphere
!                                          !   K=0 : Mercator
!                                          !-1<K<0 : Lambert, south hemisphere
!                                          !   K=-1: stereographic south pole
REAL,                 INTENT(IN) :: PBETA  ! angle between grid and reference longitude
REAL,                 INTENT(IN) :: PLATOR ! Latitude of the origine point
REAL,                 INTENT(IN) :: PLONOR ! Longitude of the origine point
REAL, DIMENSION(:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
REAL, DIMENSION(:),   INTENT(OUT):: PLAT,PLON    
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
!*     0.2    Declarations of local variables
!
INTEGER :: JI, ISIZE_OMP
REAL, DIMENSION(SIZE(PX)) :: ZY
REAL                      :: ZRPK,ZBETA,ZLAT0,ZLON0,ZLATOR,ZLONOR
REAL                      :: ZRDSDG,ZCLAT0,ZSLAT0,ZCLATOR,ZSLATOR
REAL                      :: ZXBM0,ZYBM0,ZRO0,ZGA0 
REAL                      :: ZXP,ZYP,ZEPSI,ZT1,ZCGAM,ZSGAM,ZRACLAT0
!
REAL :: ZATA,ZRO2,ZT2,ZXMI0,ZYMI0, ZXI, ZYI, ZIRDSDG
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
!--------------------------------------------------------------------------------
!
!*     1.     Preliminary calculations for all projections
!             --------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_1',0,ZHOOK_HANDLE)
ZRDSDG = XPI/180.         ! Degree to radian conversion factor
ZEPSI  = 10.*EPSILON(1.)      ! A small number
!
! By definition, (PLONOR,PLATOR) are the geographical 
! coordinates, and (ZXBM0,ZYBM0) the conformal cartesian 
! point of coordinates (x=0,y=0).
!
ZXBM0 = 0.
ZYBM0 = 0.
!
!-------------------------------------------------------------------------------
!
!*     2.     POLAR STEREOGRAPHIC AND LAMBERT CONFORMAL CASES
!             -----------------------------------------------
!                   (PRPK=1 P-stereo, 0<PRPK<1 Lambert)
!
IF(PRPK /= 0.) THEN
!
  IF (PRPK<0.) THEN     ! projection from north pole
    ZRPK=-PRPK
    ZBETA=-PBETA
    ZLAT0=-PLAT0
    ZLON0=PLON0+180.
    ZLATOR=-PLATOR
    ZLONOR=PLONOR+180.
    ZY(:)=-PY(:)
    ZYBM0=-ZYBM0
  ELSE                  ! projection from south pole
    ZRPK=PRPK
    ZBETA=PBETA
    ZLAT0=PLAT0
    ZLON0=PLON0
    ZLATOR=PLATOR
    ZLONOR=PLONOR
    ZY(:)=PY(:)
  ENDIF    
!
!*     2.1    Preliminary calculations
!
  ZCLAT0  = COS(ZRDSDG*ZLAT0)
  ZSLAT0  = SIN(ZRDSDG*ZLAT0)
  ZCLATOR = COS(ZRDSDG*ZLATOR)
  ZSLATOR = SIN(ZRDSDG*ZLATOR)
  ZRO0    = (XRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)     &
            * ((1.+ZSLAT0)*ABS(ZCLATOR)/(1.+ZSLATOR))**ZRPK  
  ZGA0    = (ZRPK*(ZLONOR-ZLON0)-ZBETA)*ZRDSDG
  ZXP     = ZXBM0-ZRO0*SIN(ZGA0)
  ZYP     = ZYBM0+ZRO0*COS(ZGA0)
!
!*    2.2    Longitude
!
ZT1     = (XRADIUS*(ABS(ZCLAT0))**(1.-ZRPK))**(2./ZRPK) * (1+ZSLAT0)**2 
!
ZIRDSDG = 1./ZRDSDG
!
ISIZE_OMP = MAX(1,SIZE(ZY)/NBLOCKTOT)
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_21',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(JI,ZXI,ZYI,ZATA,ZRO2,ZT2)
  DO JI = 1,SIZE(ZY)
  
    ZXI = PX(JI)-ZXP
    ZYI = ZY(JI)-ZYP

    IF  (ABS(ZYI) < ZEPSI .AND.ABS(ZXI) < ZEPSI)  THEN
      ZATA = 0.
    ELSE
      ZATA = ATAN2(ZXI,-ZYI)*ZIRDSDG
    ENDIF
    !
    PLON(JI) = (ZBETA+ZATA)/ZRPK+ZLON0
    !
    !*   2.3     Latitude
    !
    ZRO2 = ZXI**2+ZYI**2
    !    
    ZT2  = (ZRPK**2*ZRO2)**(1./ZRPK)
    !
    PLAT(JI) = (XPI/2.-ACOS((ZT1-ZT2)/(ZT1+ZT2)))*ZIRDSDG
    !
    IF (PRPK<0.) THEN     ! projection from north pole
      PLAT(JI)=-PLAT(JI)
      PLON(JI)=PLON(JI)+180.
    ENDIF
    !
  ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_21',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
!
!*  3.        MERCATOR PROJECTION WITH ROTATION
!             ---------------------------------
!                       (PRPK=0)
!
ELSE
!
!*  3.1       Preliminary calculations
!
  ZCGAM    = COS(-ZRDSDG*PBETA)
  ZSGAM    = SIN(-ZRDSDG*PBETA)
  ZRACLAT0 = XRADIUS*COS(ZRDSDG*PLAT0)
!
!*  3.2       Longitude
!
  ZT1     = ALOG(TAN(XPI/4.+PLATOR*ZRDSDG/2.))
  !
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_1',1,ZHOOK_HANDLE)

!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_22',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,ZXMI0,ZYMI0,ZT2)
  DO JI = 1,SIZE(PX)

    ZXMI0 = PX(JI)-ZXBM0
    ZYMI0 = PY(JI)-ZYBM0
   !
     PLON(JI) = (ZXMI0*ZCGAM+ZYMI0*ZSGAM) / (ZRACLAT0*ZRDSDG)+PLONOR  
!
!*  3.3       Latitude
!
    ZT2  = (-ZXMI0*ZSGAM+ZYMI0*ZCGAM)/ZRACLAT0
    !
    PLAT(JI) = (-XPI/2.+2.*ATAN(EXP(ZT1+ZT2)))/ZRDSDG
    !
  ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_22',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!---------------------------------------------------------------------------------
!
!*  4.        EXIT
!             ----
!
END IF
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_3',0,ZHOOK_HANDLE)
PLON(:)=PLON(:)+NINT((PLON0-PLON(:))/360.)*360.
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:LATLON_CONF_PROJ_3',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_CONF_PROJ
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!
!      ################################################
       SUBROUTINE XY_CONF_PROJ(PLAT0,PLON0,PRPK,PBETA,PLATOR,PLONOR, &
                                 PX,PY,PLAT,PLON                       )  
!      ################################################
!
!!****  *XY_CONF_PROJ * - Routine to compute conformal coordinates
!!
!!
!!     PURPOSE
!!     -------
!        This routine computes the cartesian conformal coordinates 
!      of an array given in latitude-longitude coordinates
!        Three map projections are available: 
!      - polar-stereographic (XRPK=1),
!      - lambert conformal  (0<XRPK<1),
!      - mercator (XRPK=0).
!
!
!!**   METHOD
!!     ------
!!       Spherical earth approximation is used. Longitude origin is 
!!     set in Greenwich, and is positive eastwards. An anticlockwise 
!!     rotation of XBETA degrees is applied to the conformal frame 
!!     with respect to the geographical directions.
!!
!!       WARNING: ALL INPUT AND OUTPUT ANGLES ARE IN DEGREES...
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!      Asencio N. et al., 1994, "Le projet de modele non-hydrostatique
!!            commun CNRM-LA, specifications techniques", 
!!            Note CNRM/GMME, 26, 139p, (Chapter 2).
!!      Ducrocq V., 1994, "Generation de la grille dans le modele",
!!            Note interne MNH, 5 mai, 3p.
!!      Joly A., 1992, "Geographic parameters for ARPEGE/ALADIN",
!!            Internal note ARPEGE/ALADIN, february 27,28p.
!!      Levallois J., 1970, "Geodesie generale", Tome 2, Collection
!!             de l'IGN, Eyrolles, Paris, 408p.
!!       
!!     AUTHOR
!!     ------
!!      P.M.       *LA*
!!
!!     MODIFICATION
!!     ------------
!!       Original PM  24/05/94
!!       Updated  PM  27/07/94
!!       Updated  VD  23/08/94
!!       Updated  VM  24/10/95 projection from north pole (XRPK<0) and 
!!                             longitudes set between XLON0-180. and XLON0+180.
!!       Updated  VM  03/2004  externalized version
!!
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_CSTS, ONLY : XPI, XRADIUS
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,                 INTENT(IN) :: PLAT0  ! Reference latitude
REAL,                 INTENT(IN) :: PLON0  ! Reference longitude
REAL,                 INTENT(IN) :: PRPK   ! projection parameter 
!                                          !   K=1 : stereographic north pole
!                                          ! 0<K<1 : Lambert, north hemisphere
!                                          !   K=0 : Mercator
!                                          !-1<K<0 : Lambert, south hemisphere
!                                          !   K=-1: stereographic south pole
REAL,                 INTENT(IN) :: PBETA  ! angle between grid and reference longitude
REAL,                 INTENT(IN) :: PLATOR ! Latitude of the origine point
REAL,                 INTENT(IN) :: PLONOR ! Longitude of the origine point
REAL, DIMENSION(:),   INTENT(OUT):: PX,PY
                                           ! returned conformal coordinates of the 
                                           ! processed points (meters);
REAL, DIMENSION(:),   INTENT(IN) :: PLAT,PLON    
                                           ! given geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
!*     0.2    Declarations of local variables
! 
REAL :: ZLAT,ZLON
REAL :: ZRPK,ZBETA,ZLAT0,ZLON0,ZLATOR,ZLONOR
REAL :: ZRDSDG,ZCLAT0,ZSLAT0,ZCLATOR,ZSLATOR
REAL :: ZXBM0,ZYBM0,ZRO0,ZGA0 
REAL :: ZXP,ZYP,ZCGAM,ZSGAM,ZRACLAT0,ZXE,ZYE
!
REAL :: ZCLAT,ZSLAT,ZRO,ZGA,ZXPR,ZYPR
!
INTEGER :: J, ISIZE_OMP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
!
!-------------------------------------------------------------------------------
!
!*     1.     PRELIMINARY CALCULATION FOR ALL PROJECTIONS
!             -------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_1',0,ZHOOK_HANDLE)
ZRDSDG = XPI/180.         ! Degree to radian conversion factor
!
! By definition, (PLONOR,PLATOR) are the geographical 
! coordinates, and (ZXBM0,ZYBM0) the conformal cartesian 
! coordinates of the x=0, y=0 point.
!
ZXBM0 = 0.
ZYBM0 = 0.
!
ZLONOR=PLONOR
ZLONOR=ZLONOR+NINT((PLON0-ZLONOR)/360.)*360.
!------------------------------------------------------------------------------
!
!*     2.     POLAR SEREOGRAPHIC AND LAMBERT CONFORMAL CASES
!             ----------------------------------------------
!                   (PRPK=1 P-stereo, 0<PRPK<1 Lambert)
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_2',0,ZHOOK_HANDLE)
!
ISIZE_OMP = MAX(1,SIZE(PLAT)/NBLOCKTOT)
!
IF(PRPK  /=  0.) THEN
!
  IF (PRPK<0.) THEN     ! projection from north pole
    ZRPK=-PRPK
    ZBETA=-PBETA
    ZLAT0=-PLAT0
    ZLON0=PLON0+180.
    ZLATOR=-PLATOR
    ZLONOR=ZLONOR+180.
    ZYBM0=-ZYBM0
  ELSE                  ! projection from south pole
    ZRPK=PRPK
    ZBETA=PBETA
    ZLAT0=PLAT0
    ZLON0=PLON0
    ZLATOR=PLATOR
    ZLONOR=ZLONOR
  ENDIF    
!
!*     2.1    Preliminary calculations
!
  ZCLAT0  = COS(ZRDSDG*ZLAT0)
  ZSLAT0  = SIN(ZRDSDG*ZLAT0)
  ZCLATOR = COS(ZRDSDG*ZLATOR)
  ZSLATOR = SIN(ZRDSDG*ZLATOR)
  ZRO0    = (XRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)    &
            * ((1.+ZSLAT0)*ABS(ZCLATOR)/(1.+ZSLATOR))**ZRPK  
  ZGA0    = (ZRPK*(ZLONOR-ZLON0)-ZBETA)*ZRDSDG
  ZXP     = ZXBM0-ZRO0*SIN(ZGA0)
  ZYP     = ZYBM0+ZRO0*COS(ZGA0)
!
!*    2.2    Conformal coordinates in meters
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_2',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_3',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(J,ZLON,ZLAT,ZCLAT,ZSLAT,ZRO,ZGA)
  DO J=1,SIZE(PLAT)
    !
    ZLON = PLON(J)+NINT((PLON0-PLON(J))/360.)*360.
    !
    IF (PRPK<0.) THEN     ! projection from north pole
      ZLAT =-PLAT(J)
      ZLON = ZLON+180.
    ELSE                  ! projection from south pole
      ZLAT = PLAT(J)
    ENDIF    
    !
    ZCLAT = COS(ZRDSDG*ZLAT)
    ZSLAT = SIN(ZRDSDG*ZLAT)
    ZRO   = (XRADIUS/ZRPK)*(ABS(ZCLAT0))**(1.-ZRPK)    &
              * ((1.+ZSLAT0)*ABS(ZCLAT)/(1.+ZSLAT))**ZRPK  
    ZGA   = (ZRPK*(ZLON-ZLON0)-ZBETA)*ZRDSDG
    !
    PX(J) = ZXP+ZRO*SIN(ZGA)
    PY(J) = ZYP-ZRO*COS(ZGA)
    !
    IF (PRPK<0.) THEN     ! projection from north pole
      PY(J)=-PY(J)
    ENDIF
    !
  ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_3',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
  !
!-------------------------------------------------------------------------------
!
!*  3.        MERCATOR PROJECTION WITH ROTATION
!             ---------------------------------
!                       (PRPK=0)
!
ELSE
!
!*  3.1       Preliminary calculations
!
  ZCGAM    = COS(-ZRDSDG*PBETA)
  ZSGAM    = SIN(-ZRDSDG*PBETA)
  ZRACLAT0 = XRADIUS*COS(ZRDSDG*PLAT0)
  ZXE      = ZXBM0*ZCGAM+ZYBM0*ZSGAM            &
           - ZRACLAT0*(PLONOR-PLON0)*ZRDSDG    
  ZYE      =-ZXBM0*ZSGAM+ZYBM0*ZCGAM            &
           - ZRACLAT0*LOG(TAN(XPI/4.+PLATOR*ZRDSDG/2.))  
!
!*  3.2       Conformal coordinates
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_2',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_31',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(J,ZLON,ZXPR,ZYPR)
  DO J=1,SIZE(PLAT)
    !
    ZLON = PLON(J)+NINT((PLON0-PLON(J))/360.)*360.
    !
    ZXPR = ZRACLAT0*(ZLON-PLON0)*ZRDSDG+ZXE
    ZYPR = ZRACLAT0*LOG(TAN(XPI/4.+PLAT(J)*ZRDSDG/2.))+ZYE
    !
    PX(J) = ZXPR*ZCGAM-ZYPR*ZSGAM
    PY(J) = ZXPR*ZSGAM+ZYPR*ZCGAM
    !
  ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:XY_CONF_PROJ_31',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
  !
!-------------------------------------------------------------------------------
!
!*  4.        EXIT
!             ----
!
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE XY_CONF_PROJ
!-------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!
!      ################################################
       SUBROUTINE MAP_FACTOR_CONF_PROJ(PLAT0,PRPK,PLAT,PMAP)
!      ################################################
!
!!****  *MAP_FACTOR_CONF_PROJ * - Routine to compute conformal coordinates
!!
!!
!!     PURPOSE
!!     -------
!
!!     REFERENCE
!!     ---------
!!      Asencio N. et al., 1994, "Le projet de modele non-hydrostatique
!!            commun CNRM-LA, specifications techniques", 
!!            Note CNRM/GMME, 26, 139p, (Chapter 2).
!!      Ducrocq V., 1994, "Generation de la grille dans le modele",
!!            Note interne MNH, 5 mai, 3p.
!!      Joly A., 1992, "Geographic parameters for ARPEGE/ALADIN",
!!            Internal note ARPEGE/ALADIN, february 27,28p.
!!      Levallois J., 1970, "Geodesie generale", Tome 2, Collection
!!             de l'IGN, Eyrolles, Paris, 408p.
!!       
!!     AUTHOR
!!     ------
!!      P.M.       *LA*
!!
!!     MODIFICATION
!!     ------------
!!       Original PM  24/05/94
!!       Updated  PM  27/07/94
!!       Updated  VD  23/08/94
!!       Updated  VM  24/10/95 projection from north pole (XRPK<0) and 
!!                             longitudes set between XLON0-180. and XLON0+180.
!!       Updated  VM  03/2004  externalized version
!!
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_CSTS, ONLY : XPI, XRADIUS
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
REAL,                 INTENT(IN) :: PLAT0  ! Reference latitude
REAL,                 INTENT(IN) :: PRPK   ! projection parameter 
!                                          !   K=1 : stereographic north pole
!                                          ! 0<K<1 : Lambert, north hemisphere
!                                          !   K=0 : Mercator
!                                          !-1<K<0 : Lambert, south hemisphere
!                                          !   K=-1: stereographic south pole
REAL, DIMENSION(:),   INTENT(IN) :: PLAT   ! given geographic latitudes
                                           ! of the processed points (degrees).
REAL, DIMENSION(:),   INTENT(OUT):: PMAP   ! map factor
!
!*     0.2    Declarations of local variables
! 
!
REAL                              :: ZLAT0    ! reference latitude
REAL                              :: ZRPK     ! projection parameter 
!                                             !   K=1 : stereographic north pole
!                                             ! 0<K<1 : Lambert, north hemisphere
!                                             !   K=0 : Mercator
!                                             !-1<K<0 : Lambert, south hemisphere
!                                             !   K=-1: stereographic south pole
REAL, DIMENSION(SIZE(PLAT))       :: ZLAT     ! latitude
!
REAL                              :: ZCLAT0   ! cos(lat0)
REAL                              :: ZSLAT0   ! sin(lat0)
REAL                              :: ZRDSDG   ! pi/180
LOGICAL                           :: GNORTHPROJ! T: projection from north pole
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
INTEGER :: J, ISIZE_OMP
!
!-------------------------------------------------------------------------------
!
!*     1.     PRELIMINARY CALCULATION FOR ALL PROJECTIONS
!             -------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_1',0,ZHOOK_HANDLE)
ZRDSDG = XPI/180.         ! Degree to radian conversion factor
!
GNORTHPROJ = PRPK < 0.
IF (GNORTHPROJ) THEN     ! projection from north pole 
  ZRPK=-PRPK
  ZLAT0=-PLAT0
  ZLAT(:)=-PLAT(:)
ELSE
  ZRPK=PRPK
  ZLAT0=PLAT0
  ZLAT(:)=PLAT(:)
ENDIF    
!
ZRDSDG = XPI/180.
ZCLAT0 = COS(ZRDSDG*ZLAT0)
ZSLAT0 = SIN(ZRDSDG*ZLAT0)
!
ISIZE_OMP = MAX(1,SIZE(PMAP)/NBLOCKTOT)
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_1',1,ZHOOK_HANDLE)
!
IF (ABS(ZCLAT0)<1.E-10 .AND. (ABS(ZRPK-1.)<1.E-10)) THEN
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_2a',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(J)
  DO J=1,SIZE(PMAP)
    PMAP(J) = (1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(J)))
  ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_2a',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
ELSE
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_2b',0,ZHOOK_HANDLE_OMP)
!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(J)
  DO J=1,SIZE(PMAP)
    IF (ABS(COS(ZRDSDG*ZLAT(J)))>1.E-10) THEN
      PMAP(J) = ((ZCLAT0/COS(ZRDSDG*ZLAT(J)))**(1.-ZRPK))      &
              * ((1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(J))))**ZRPK  
    ELSE
      PMAP(J) = (1.+ZSLAT0)/(1.+SIN(ZRDSDG*ZLAT(J)))
    ENDIF
  ENDDO
!$OMP ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_CONF_PROJ:MAP_FACTOR_CONF_PROJ_2b',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
END IF
!
!-------------------------------------------------------------------------------
END SUBROUTINE MAP_FACTOR_CONF_PROJ
!-------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################

END MODULE MODE_GRIDTYPE_CONF_PROJ
