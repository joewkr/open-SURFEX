!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##############################
      MODULE MODE_GRIDTYPE_IGN
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
      SUBROUTINE PUT_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT,PX,PY,PDX,PDY, &
                                  KDIMX,KDIMY,PXALL,PYALL           )
!     ####################################################################
!
!!****  *PUT_GRIDTYPE_IGN* - routine to store in PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      E. Martin   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!!      02/2011     Correction de la longitude d'origine pourle cas L93 (A. Lemonsu)
!!      07/2011     add maximum domain dimension for output (B. Decharme)
!       01/2016     Correction de la valeur de l'excentricite pour L93 (V. Masson)
!       01/2016     Correction de la valeur du rayon terrestre pour Lamberts 1 a 4 (V. Masson)
!       01/2016     Correction d'une boucle pour parallelisation (V. Masson)
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
INTEGER,            INTENT(IN)  :: KLAMBERT ! Lambert type
                                            ! 1 : Lambert I
                                            ! 2 : Lambert II
                                            ! 3 : Lambert III
                                            ! 4 : Lambert IV
                                            ! 5 : Extended Lambert II
                                            ! 6 : Lambert 93
REAL, DIMENSION(:), INTENT(IN)  :: PX       ! X coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(IN)  :: PY       ! Y coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(IN)  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(IN)  :: PDY      ! Y grid mesh size
INTEGER,            INTENT(IN)  :: KDIMX    ! maximum domain length in X
INTEGER,            INTENT(IN)  :: KDIMY    ! maximum domain length in Y
REAL, DIMENSION(KDIMX), INTENT(IN)  :: PXALL! maximum domain X coordinate of grid mesh
REAL, DIMENSION(KDIMY), INTENT(IN)  :: PYALL! maximum domain Y coordinate of grid mesh
REAL, DIMENSION(:), POINTER     :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL ! number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:PUT_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
IL = SIZE(PX)
ALLOCATE(PGRID_PAR(4*IL+4+KDIMX+KDIMY))
PGRID_PAR(1) = FLOAT(KLAMBERT)
PGRID_PAR(2) = FLOAT(IL)
PGRID_PAR(3:IL+2)        = PX(:)
PGRID_PAR(IL+3:2*IL+2)   = PY(:)
PGRID_PAR(2*IL+3:3*IL+2) = PDX(:)
PGRID_PAR(3*IL+3:4*IL+2) = PDY(:)
PGRID_PAR(4*IL+3)        = FLOAT(KDIMX)
PGRID_PAR(4*IL+4)        = FLOAT(KDIMY)
PGRID_PAR(4*IL+5:4*IL+4+KDIMX) = PXALL(:)
PGRID_PAR(4*IL+5+KDIMX:4*IL+4+KDIMX+KDIMY) = PYALL(:)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:PUT_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_GRIDTYPE_IGN
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_GRIDTYPE_IGN(PGRID_PAR,KLAMBERT,KL,PX,PY,PDX,PDY,&
                                  KDIMX,KDIMY,PXALL,PYALL             )
!     ####################################################################
!
!!****  *GET_GRIDTYPE_IGN* - routine to get from PGRID_PAR the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      E. Martin   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007 
!!      07/2011     add maximum domain dimension for output (B. Decharme)
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
INTEGER,            INTENT(OUT), OPTIONAL  :: KL ! number of points
INTEGER,            INTENT(OUT), OPTIONAL  :: KLAMBERT ! Lambert type
                                            ! 1 : Lambert I
                                            ! 2 : Lambert II
                                            ! 3 : Lambert III
                                            ! 4 : Lambert IV
                                            ! 5 : Extended Lambert II
                                            ! 6 : Lambert 93
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PX       ! X coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PY       ! Y coordinate of grid mesh center
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PDX      ! X grid mesh size
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PDY      ! Y grid mesh size
INTEGER,            INTENT(OUT), OPTIONAL  :: KDIMX    ! maximum domain length in X
INTEGER,            INTENT(OUT), OPTIONAL  :: KDIMY    ! maximum domain length in Y
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PXALL    ! maximum domain X coordinate of grid mesh
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL  :: PYALL    ! maximum domain Y coordinate of grid mesh

REAL, DIMENSION(:), INTENT(IN)            :: PGRID_PAR! parameters defining this grid
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IL, IDIMX, IDIMY
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:GET_GRIDTYPE_IGN',0,ZHOOK_HANDLE)
IF (PRESENT(KLAMBERT))  KLAMBERT = NINT(PGRID_PAR(1))
IF (PRESENT(KL))        KL       = NINT(PGRID_PAR(2))
!
IL = NINT(PGRID_PAR(2))
!
IF (PRESENT(PX)) PX(:) = PGRID_PAR(2+1:2+IL)
!
IF (PRESENT(PY)) PY(:) = PGRID_PAR(2+IL+1:2+2*IL)
!
IF (PRESENT(PDX)) PDX(:) = PGRID_PAR(2+2*IL+1:2+3*IL)
!
IF (PRESENT(PDY)) PDY(:) = PGRID_PAR(2+3*IL+1:2+4*IL)
!
IF (PRESENT(KDIMX)) KDIMX = NINT(PGRID_PAR(3+4*IL))
!
IF (PRESENT(KDIMY)) KDIMY = NINT(PGRID_PAR(4+4*IL))
!
IF (PRESENT(PXALL)) THEN
  IDIMX= NINT(PGRID_PAR(3+4*IL))
  PXALL(:)= PGRID_PAR(5+4*IL:4+4*IL+IDIMX)
END IF
!
IF (PRESENT(PYALL)) THEN
  IDIMX= NINT(PGRID_PAR(3+4*IL))
  IDIMY= NINT(PGRID_PAR(4+4*IL))
  PYALL(:)= PGRID_PAR(5+4*IL+IDIMX:4+4*IL+IDIMX+IDIMY)
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:GET_GRIDTYPE_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_GRIDTYPE_IGN
!############################################################################
!############################################################################
!############################################################################
!      ###################################################
       SUBROUTINE LATLON_IGN(KLAMBERT,PX,PY,PLAT,PLON)
!      ###################################################
!
!!****  *LATLON_IGN * - Routine to compute geographical coordinates
!!
!!     PURPOSE
!!     -------
!        This routine computes the latitude and longitude of
!      an array given in LAMBERT coordinates
!
!
!!**   METHOD
!!     ------
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!     NOTE TECHNIQUE IGN NT/G 71 : 
!!        PROJECTION CARTOGRAPHIQUE CONIQUE COMFORME DE LAMBERT
!!        (www.ign.fr)
!!       
!!     AUTHOR
!!     ------
!!      E. Martin   *Meteo-France*
!!
!!     MODIFICATION
!!     ------------
!!       Original  10/2007        
!       S.Lafont remplace exposant reel 2.0 par exposant entier 2
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_CSTS,ONLY : XPI
USE MODD_IGN
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
INTEGER,              INTENT(IN) :: KLAMBERT  ! Lambert type           
REAL, DIMENSION(:),   INTENT(IN) :: PX,PY
                                           ! given conformal coordinates of the 
                                           ! processed points (meters);
REAL, DIMENSION(:),   INTENT(OUT):: PLAT,PLON    
                                           ! returned geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
!
!*     0.2    Declarations of local variables
REAL, DIMENSION(SIZE(PX)) :: ZGAMMA
REAL, DIMENSION(SIZE(PX)) :: ZR               ! length of arc meridian line projection
REAL, DIMENSION(SIZE(PX)) :: ZLATISO          ! Isometric latitude
REAL :: ZLAT0            ! For iteration
! 
INTEGER                         :: J, JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       1.     PRELIMINARY
!               -----------
!
      IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:LATLON_IGN',0,ZHOOK_HANDLE)
      ZR(:)    =SQRT( (PX(:)-XXS(KLAMBERT))**2 + (PY(:)-XYS(KLAMBERT))**2 ) 
      ZGAMMA(:)= ATAN ( (PX(:)-XXS(KLAMBERT)) / (XYS(KLAMBERT)-PY(:)) )
!
!*       2.     LONGITUDE
!               ---------
      PLON(:)=XLONP(KLAMBERT) +ZGAMMA(:)/XN(KLAMBERT) *180./XPI
!
!*       3.     LATITUDE
!               --------
      ZLATISO(:)=-1./XN(KLAMBERT) * ALOG(ABS(ZR(:)/XC(KLAMBERT)))
!   
!$OMP PARALLEL DO PRIVATE(JJ,J,ZLAT0)
      DO JJ=1,SIZE(PLAT)
        ZLAT0  =2. * ATAN (EXP(ZLATISO(JJ))) - XPI/2.
        DO J=1, 1000
         PLAT(JJ) = 2. * ATAN(                                               &
           ( (1+XECC(KLAMBERT)*SIN(ZLAT0))/(1-XECC(KLAMBERT)*SIN(ZLAT0)) )**(XECC(KLAMBERT)/2.)       &
             *EXP(ZLATISO(JJ)) )  -XPI/2.  
!
         IF (ABS(PLAT(JJ) - ZLAT0) < XCVGLAT ) EXIT
         ZLAT0=PLAT(JJ)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
!      
      PLAT(:)=PLAT(:) *180./XPI
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:LATLON_IGN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------------
END SUBROUTINE LATLON_IGN
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!
!      ################################################
       SUBROUTINE XY_IGN(KLAMBERT,PX,PY,PLAT,PLON)
!      ################################################
!
!!****  *XY_IGN * - Routine to compute Lambert coordinates   
!!
!!
!!     PURPOSE
!!     -------
!        This routine computes the Lambert coordinates              
!      of an array given in latitude-longitude coordinates
!
!
!!**   METHOD
!!     ------
!!
!!       WARNING: ALL INPUT AND OUTPUT ANGLES ARE IN DEGREES...
!!
!!     EXTERNAL
!!     --------
!!       None
!!
!!     REFERENCE
!!     ---------
!!     NOTE TECHNIQUE IGN NT/G 71 : 
!!        PROJECTION CARTOGRAPHIQUE CONIQUE COMFORME DE LAMBERT
!!        (www.ign.fr)
!!       
!!     AUTHOR
!!     ------
!!      E. Martin   *Meteo-France*
!!
!!     MODIFICATION
!!     ------------
!!       Original  10/2007        
!!       
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_CSTS, ONLY : XPI
USE MODD_IGN
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
INTEGER,              INTENT(IN) :: KLAMBERT  ! Lambert type           
REAL, DIMENSION(:),   INTENT(IN) :: PLAT,PLON    
                                           ! given geographic latitudes and 
                                           ! longitudes of the processed points 
                                           ! (degrees).
REAL, DIMENSION(:),   INTENT(OUT):: PX,PY
                                           ! returned Lambert coordinates of the 
                                           ! processed points (meters);
!
!*     0.2    Declarations of local variables
!
REAL :: ZPI180, ZPI4, ZECC2
REAL :: ZWRK     ! working arrays
REAL :: ZLATRAD, ZLONRAD ! longitude and latitude in radians
REAL :: ZGAMMA
REAL :: ZLATFI           ! Isometric latitude
REAL :: ZR               ! length of arc meridian line projection
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!
!
!-------------------------------------------------------------------------------
!
!*       1.     Latitude /Longitude in radian :
!               -------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:XY_IGN_1',0,ZHOOK_HANDLE)
!
ZPI180 = XPI / 180.
ZPI4 = XPI / 4.
ZECC2 = XECC(KLAMBERT) / 2.
!
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:XY_IGN_1',1,ZHOOK_HANDLE)
!
!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:XY_IGN_2',0,ZHOOK_HANDLE_OMP)
!$OMP DO PRIVATE(JJ,ZLONRAD,ZLATRAD,ZWRK,ZLATFI,ZGAMMA,ZR)
DO JJ=1,SIZE(PLON)
  !
  IF (PLON(JJ) > 180.) THEN
    ZLONRAD = (PLON(JJ) - 360. - XLONP(KLAMBERT)) * ZPI180
  ELSE
    ZLONRAD = (PLON(JJ) - XLONP(KLAMBERT)) * ZPI180
  ENDIF
  !
  ZLATRAD = PLAT(JJ) * ZPI180
  !
!*       2.     Calcul of the isometric latitude :
!               ----------------------------------
  !
  ZWRK   = SIN(ZLATRAD) * XECC(KLAMBERT)  
  !
  ZLATFI  = LOG(TAN(ZPI4 + ZLATRAD / 2.)) + ( (LOG(1-ZWRK)-LOG(1+ZWRK)) * ZECC2)
  !
!*       3.     Calcul of the lambert II coordinates X and YJJ
!               ---------------------------------------------
  !
  ZR      = EXP(- XN(KLAMBERT) * ZLATFI) * XC(KLAMBERT)
  !
  ZGAMMA  = XN(KLAMBERT) * ZLONRAD
  !
  PX(JJ) = XXS(KLAMBERT) + SIN(ZGAMMA) * ZR
  PY(JJ) = XYS(KLAMBERT) - COS(ZGAMMA) * ZR   
  !
ENDDO
!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:XY_IGN_2',1,ZHOOK_HANDLE_OMP)
!$OMP END PARALLEL
!
!-------------------------------------------------------------------------------
END SUBROUTINE XY_IGN
!-------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!
!      ################################################
       SUBROUTINE MAP_FACTOR_IGN(KLAMBERT,PX,PY,PMAP)
!      ################################################
!
!!****  *MAP_FACTOR_IGN * - Routine to compute IGN map factor
!!
!!
!!     PURPOSE
!!     -------
!      Calculation of the Map factor 
!      (ratio dist lambert / dist ellipsoide)
!
!!     REFERENCE
!!     ---------
!!     IGN formula
!!       
!!     AUTHOR
!!     ------
!!      E. Martin *Meteo-France*
!!
!!     MODIFICATION
!!     ------------
!!       Original  10/2007 
!       S.Lafont remplace exposant reel 2.0 par exposant entier 2
!!
!-------------------------------------------------------------------------------
!
!*     0.     DECLARATIONS
!             ------------
!
USE MODD_CSTS, ONLY : XPI
USE MODD_IGN
!
IMPLICIT NONE
!
!*     0.1    Declarations of arguments and results
!
INTEGER,              INTENT(IN) :: KLAMBERT ! Lambert type (1 to 6)
REAL, DIMENSION(:),   INTENT(IN) :: PX     ! X lambert coordinate
REAL, DIMENSION(:),   INTENT(IN) :: PY     ! Y lambert coordinate
                                           ! of the processed points (m).
REAL, DIMENSION(:),   INTENT(OUT):: PMAP   ! map factor
!
!*     0.2    Declarations of local variables
! 
!
REAL, DIMENSION(SIZE(PX))       :: ZLAT0    ! latitude for iteration
REAL, DIMENSION(SIZE(PX))       :: ZLAT     ! latitude
REAL, DIMENSION(SIZE(PX))       :: ZLATISO  ! isometric latitude
REAL, DIMENSION(SIZE(PX))       :: ZR       ! R in the IGN formula
REAL, DIMENSION(SIZE(PX))       :: ZGRANDN  ! N in the IGN formula
!
INTEGER                         :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     PRELIMINARY
!               -----------
!
      IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:MAP_FACTOR_IGN',0,ZHOOK_HANDLE)
      ZR(:)    =SQRT( (PX(:)-XXS(KLAMBERT))**2 + (PY(:)-XYS(KLAMBERT))**2 ) 
      ZLATISO(:)=-1./XN(KLAMBERT) * ALOG(ABS(ZR(:)/XC(KLAMBERT)))
      ZLAT0(:)  =2. * ATAN (EXP(ZLATISO(:))) - XPI/2.
!      
      DO J=1, 100
         ZLAT(:) = 2. * ATAN(                                               &
           ( (1+XECC(KLAMBERT)*SIN(ZLAT0(:))) / (1-XECC(KLAMBERT)*SIN(ZLAT0(:))) )**(XECC(KLAMBERT)/2.)  &
             *EXP(ZLATISO(:)) )  -XPI/2.  
!
         IF (MAXVAL(ABS(ZLAT(:) - ZLAT0(:))) < XCVGLAT ) EXIT
         ZLAT0(:)=ZLAT(:)
      ENDDO
!      
!
!*       2.     MAP FACTOR
!               ----------
!
      ZGRANDN = XA(KLAMBERT) / SQRT(1-(XECC(KLAMBERT)*SIN(ZLAT(:)))**2)
      PMAP(:)=XN(KLAMBERT)* ZR(:) / ( ZGRANDN(:)*COS(ZLAT(:)) )
IF (LHOOK) CALL DR_HOOK('MODE_GRIDTYPE_IGN:MAP_FACTOR_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE MAP_FACTOR_IGN
!-------------------------------------------------------------------------------
!

!############################################################################
!############################################################################
!############################################################################

END MODULE MODE_GRIDTYPE_IGN
