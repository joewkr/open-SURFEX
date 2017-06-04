!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###########################
SUBROUTINE TOPODYN_LAT(PRW,PDEF,PKAPPA,PKAPPAC,GTOPD)
!,PERROR)
!     ###########################
!
!
!     PURPOSE
!     -------
!     to distribute laterally soil water following topodyn concept
!
!
!     METHOD
!     ------
!
!     EXTERNAL
!     --------
!     none
!
!
!     AUTHOR
!     ------
!
!     G.-M. Saulnier * LTHE * 
!     K. Chancibault * CNRM *
!
!     MODIFICATIONS
!     -------------
!
!     Original    12/2003
!     writing in fortran 90 12/2004
!------------------------------------------------------------------------------------------
!
!*    0.0    DECLARATIONS
!            ------------
USE MODD_TOPODYN,       ONLY : NNCAT, NMESHT, XDMAXT, XDXT, XMPARA, NNMC, XCONN, NLINE,&
                                 XSLOP,  XDAREA, XLAMBDA
USE MODD_COUPLING_TOPD, ONLY : XWSTOPT, XDTOPT
USE MODD_TOPD_PAR,        ONLY : XSTEPK
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODI_FLOWDOWN
USE MODI_ABOR1_SFX
USE MODI_WRITE_FILE_VECMAP
USE MODI_WRITE_FILE_MAP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1   declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PRW
REAL, DIMENSION(:,:), INTENT(OUT)  :: PDEF
REAL, DIMENSION(:,:), INTENT(OUT)  :: PKAPPA
REAL, DIMENSION(:), INTENT(OUT)    :: PKAPPAC
LOGICAL, DIMENSION(:), INTENT(OUT) :: GTOPD
!
!*    0.2   declarations of local variables
!
LOGICAL              :: GFOUND  ! logical variable
REAL                 :: ZSOMME
REAL                 :: ZM      ! XMPARA in m
REAL                 :: ZDX     ! XDXT in m
REAL                 :: ZKVAL, ZKVALMIN, ZKVALMAX
REAL                 :: ZDAV    ! Averaged deficit (m)
REAL                 :: ZDAV2   ! Averaged deficit on ZA-ZAS-ZAD (m)
REAL                 :: ZNDMAXAV,ZNKAV ! temporary averaged maximal deficit and averaged similarity index
REAL                 :: ZDMAXAV,ZKAV   ! averaged maximal deficit and averaged similarity index
REAL                 :: ZFUNC
REAL                 :: ZDIF,ZDIFMIN   ! difference calcul
REAL                 :: ZNAS, ZNAD     ! temporary saturated and dry relative catchment area
REAL                 :: ZAS,ZAD        ! saturated and dry relative catchment area 
REAL                 :: ZA             ! total catchment area
REAL                 :: ZTMP
!
REAL, DIMENSION(NMESHT) :: ZDMAX     ! XDMAXT in m
REAL, DIMENSION(NMESHT) :: ZRW       ! PRW in m
REAL, DIMENSION(NMESHT) :: ZDINI     ! initial deficit
REAL, DIMENSION(NMESHT) :: ZMASK
REAL, DIMENSION(NMESHT) :: ZKAPPA_PACK, ZDMAX_PACK
!REAL, DIMENSION(NMESHT) :: ZTMP
!
INTEGER              :: J1, J2, JJ !
INTEGER              :: INKAPPA ! number of steps in similarity index distribution
INTEGER              :: INPCON  ! number of connected pixels
INTEGER              :: INAS    ! number of saturated pixels
INTEGER              :: INAD    ! number of dry pixels
INTEGER :: I_DIM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TOPODYN_LAT',0,ZHOOK_HANDLE)
!
PKAPPA(:,: )= 0.0
PKAPPAC(:) = 0.
GTOPD(:) = .TRUE.
PDEF(:,:) = 0.
ZAS=0.
ZAD=1.
!
DO JJ = 1,NNCAT
  !*    0.    Initialisation
  !           -------------- 
  ZMASK(:) = 0.0
  ZRW(:) = 0.0
  ZDMAX(:) = 0.0
  INPCON = 0
  ZDAV = 0.0
  ZDAV2 = 0.0
  GFOUND = .FALSE.
  ZDIFMIN = 99999.
  !
  ZKAV = 0.
  !
  ZRW(:) = PRW(JJ,:) 
  ZDMAX(:) = XDMAXT(JJ,:)
  ZDX = XDXT(JJ)
  ZM = XMPARA(JJ)
  ZDINI(:) = ZDMAX(:)
  !
  !*    0.2   definition of the catchment area concerned by lateral distribution
  !           ------------------------------------------------------------------
  !
  DO J1=1,NNMC(JJ)
    !
    IF ( ZRW(J1)>0.0 .AND. ZRW(J1)/=XUNDEF) THEN
      ZMASK(J1)=1.0
    ELSE
      ZMASK(J1)=0.0
    ENDIF
    !
  ENDDO
  !
  CALL FLOWDOWN(NNMC(JJ),ZMASK,XCONN(JJ,:,:),NLINE(JJ,:))
  !
  WHERE (ZMASK == 0.0) ZMASK = XUNDEF
  !
  !
  !*    1.    Calcul of hydrological similarity and topographic indexes 
  !           ---------------------------------------------------------
  !*    1.1   Calcul of averaged deficit and initialisation of indexes
  !           --------------------------------------------------------
  !
  ZA   = NNMC(JJ) * ZDX**2
  ZTMP=0.
  !
  DO J1=1,NNMC(JJ)
    !
    IF (ZMASK(J1)/=XUNDEF) THEN
      !
      PKAPPA(JJ,J1) = ZRW(J1) 
      INPCON = INPCON + 1
      ZDINI(J1) = ZDMAX(J1) - ZRW(J1)
      !
      IF ( ZDINI(J1) <0.0 ) THEN
        ! WRITE(*,*) J1,'Dini negatif !'
        ZTMP = ZTMP - ZDINI(J1) !we stock here water above saturation to be
                                !       distributed among the others pixels
        ZDINI(J1) = 0.
      ENDIF
      !
      ZDAV = ZDAV + ZDINI(J1)
      !
    ELSE
      !
      PKAPPA(JJ,J1) = XUNDEF
      !
    ENDIF
    !
  ENDDO
  !
  IF (ZTMP>0.) THEN
    !write(*,*) COUNT(ZDINI(:)<0.),' pixels avec ZDINI negatif. Volume total :', ZTMP
    WHERE ( ZDINI(:)>0. ) ZDINI(:) = ZDINI(:)-ZTMP/(COUNT(ZDINI(:)>0.))
  ENDIF
  !
  IF (INPCON >= NNMC(JJ)/1000) THEN
    !
    ZDAV = ZDAV / INPCON 
    ZDAV = ZDAV / ZM
    !
    !*    1.2   Propagation of indexes
    !           ----------------------
    !
    CALL FLOWDOWN(NNMC(JJ),PKAPPA(JJ,:),XCONN(JJ,:,:),NLINE(JJ,:))
    !
    !*    1.3   Distribution of indexes
    !           ----------------------
    !
    J2=1
    !
    DO WHILE ( .NOT.GFOUND .AND. J2.LE.NNMC(JJ) )
      !
      IF (ZMASK(J2)/=XUNDEF) THEN
        !
        GFOUND = .TRUE.
        ZKVAL = PKAPPA(JJ,J2) * EXP(XLAMBDA(JJ,J2))
        !ZKVAL = PKAPPA(JJ,J2) * XDAREA(JJ,J2) / XSLOP(JJ,J2)
        ZKVAL = LOG(ZKVAL)
        ZKVALMAX = ZKVAL
        ZKVALMIN = ZKVAL
        PKAPPA(JJ,J2) = ZKVAL
        !
      ELSE
        !
        PKAPPA(JJ,J2) = XUNDEF
        !
      ENDIF
      !
      J2 = J2 + 1
      !
    ENDDO
    !   
    DO J1 = J2,NNMC(JJ)
      !
      IF (ZMASK(J1)/=XUNDEF) THEN
        !
        ZKVAL = PKAPPA(JJ,J1) * EXP(XLAMBDA(JJ,J1))
!       ZKVAL = PKAPPA(JJ,J1) * XDAREA(JJ,J1) / XSLOP(JJ,J1)
        ZKVAL = LOG(ZKVAL)
        !
        IF (ZKVAL.GT.ZKVALMAX) THEN
          ZKVALMAX = ZKVAL
        ELSEIF (ZKVAL.LT.ZKVALMIN) THEN
          ZKVALMIN = ZKVAL
        ENDIF
        !
        PKAPPA(JJ,J1) = ZKVAL
        !
      ELSE
        !
        PKAPPA(JJ,J1) = XUNDEF
        !
      ENDIF
      !
    ENDDO
    !
    !*    1.4   Calcul of saturation index
    !           --------------------------
    !
    I_DIM = COUNT( ZMASK(1:NNMC(JJ))/=XUNDEF )
    ZKAPPA_PACK(:) = XUNDEF
    ZDMAX_PACK (:) = XUNDEF
    ZKAPPA_PACK(1:I_DIM) = PACK(PKAPPA(JJ,1:NNMC(JJ)),ZMASK(1:NNMC(JJ))/=XUNDEF)
    ZDMAX_PACK (1:I_DIM) = PACK(ZDMAX    (1:NNMC(JJ)),ZMASK(1:NNMC(JJ))/=XUNDEF)
    !
    INKAPPA = INT((ZKVALMAX - ZKVALMIN) / XSTEPK)
    !
    DO J1=1,INKAPPA
      !
      ZKVAL = ZKVALMIN + (XSTEPK * (J1-1))
      INAS = 0
      INAD = 0
      ZNDMAXAV = 0.0
      ZNKAV = 0.0
      !
      DO J2=1,I_DIM      
        !      
        IF ( ZKAPPA_PACK(J2).GE.ZKVAL ) THEN
          ! saturated pixel
          INAS = INAS + 1
        ELSEIF  (ZKAPPA_PACK(J2).LE.( ZKVAL-(ZDMAX_PACK(J2)/ZM)) ) THEN
          ! dry pixel
          INAD = INAD + 1
          ZNDMAXAV = ZNDMAXAV + ZDMAX_PACK(J2)
        ELSE
          ZNKAV = ZNKAV + ZKAPPA_PACK(J2)
        ENDIF
        !
      ENDDO
      ! 
      IF (INAD == 0) THEN
        ZNDMAXAV = 0.0
      ELSE
        ZNDMAXAV = ZNDMAXAV /  REAL(INAD)
      ENDIF
      !
      IF ( INPCON == INAS .OR. INPCON == INAD .OR. INPCON == (INAD+INAS)) THEN
        ZNKAV = 0.0
      ELSE
        ZNKAV = ZNKAV / REAL(INPCON - INAD - INAS)
      ENDIF
      !
      IF (INPCON /= 0) THEN
        ZNAS = REAL(INAS) / REAL(INPCON)
        ZNAD = REAL(INAD) / REAL(INPCON)
      ENDIF
      !
      ZFUNC = (1 - ZNAS - ZNAD) * ( ZKVAL - ZNKAV )
      IF (ZM /= 0.) ZFUNC = ZFUNC + (ZNAD * (ZNDMAXAV / ZM))
      !
      ZDIF = ABS( ZFUNC - ZDAV )
      !
      IF ( ZDIF.LT.ZDIFMIN ) THEN
        !
        ZDIFMIN = ZDIF
        PKAPPAC(JJ) = ZKVAL
        ZAS = ZNAS
        ZAD = ZNAD
        ZDMAXAV = ZNDMAXAV
        ZKAV = ZNKAV
        !
      ENDIF
      !
    ENDDO   
    !
    !*    2.     Local deficits calculation
    !            --------------------------
    !
    !*    2.1    New averaged deficit on A-Ad-As
    !            -------------------------------
    !
    ZDAV = ZDAV * ZM
    !
    IF ( ZAS<1. .AND. ZAD<1. .AND. (ZAS + ZAD/=1.) ) THEN
      !
      ZDAV2 = (ZDAV - ZDMAXAV * ZAD) / (1 - ZAS - ZAD)
      !
    ELSE
      !
      IF (ZAS>=1.) WRITE(*,*) 'ALL THE AREA IS SATURATED'
      IF (ZAD>=1.) WRITE(*,*) 'ALL THE AREA HAS A MAXIMAL DEFICIT'
      WRITE(*,*) 'ALL THE AREA',ZAS,ZAD
     ! CALL ABOR1_SFX("TOPODYN_LAT: ALL THE AREA IS SATURATED OR HAS A MAXIMAL DEFICIT")
      !
    ENDIF
    !
    !*    2.2    Local deficits 
    !            --------------
    !
   ! ZSOMME=0.0
    !ZTMP(:)=XUNDEF
    !
    DO J1=1,NNMC(JJ)
      !
      IF ( ZMASK(J1)/=XUNDEF ) THEN
        !
        IF ( (PKAPPA(JJ,J1).GT.(PKAPPAC(JJ) - ZDMAX(J1)/ZM)) .AND. (PKAPPA(JJ,J1).LT.PKAPPAC(JJ)) ) THEN
          !
          PDEF(JJ,J1) = ZM * (ZKAV - PKAPPA(JJ,J1)) + ZDAV2
          !ZTMP(J1) = 0.5
          IF (PDEF(JJ,J1) < 0.0) PDEF(JJ,J1) = 0.0
          !
        ELSEIF ( PKAPPA(JJ,J1).GE.PKAPPAC(JJ) ) THEN
          !
          PDEF(JJ,J1) = 0.0
          !ZTMP(J1) = 1.
          !
        ELSEIF ( PKAPPA(JJ,J1).LE.(PKAPPAC(JJ) - ZDMAX(J1)/ZM) ) THEN
          !
          PDEF(JJ,J1) = ZDMAX(J1)
          !ZTMP(J1) = -1.0
          !
        ENDIF
        !
        ! nouveau contenu en eau total (m)
        !ZSOMME = ZSOMME + ( XWSTOPT(JJ,J1)*XDTOPT(JJ,J1) - PDEF(JJ,J1) )
        !
      ELSE
        !
        PDEF(JJ,J1) = ZDMAX(J1)
        !
      ENDIF
      !
    ENDDO
    !
    ! variation du contenu en eau total
    DO J1=1,NNMC(JJ)
      !
      IF (PDEF(JJ,J1)<0.0) THEN
        WRITE(*,*) 'LAMBDA=',PKAPPA(JJ,J1),'LAMBDAC=',PKAPPAC(JJ)
      ENDIF
      !
    ENDDO
    !
    GTOPD(JJ)=.TRUE.
    !
 ELSE
    !
    !  'Pas de redistribution laterale'
    GTOPD(JJ)=.FALSE.
    !
    PKAPPA(JJ,:) = XUNDEF
    !  PDEF(JJ,:) = ZDMAX(:) - ZRW(:)
    PDEF(JJ,:) = ZDINI(:)
    PKAPPAC(JJ) = XUNDEF
    !
  ENDIF
  ! 
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TOPODYN_LAT',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOPODYN_LAT

