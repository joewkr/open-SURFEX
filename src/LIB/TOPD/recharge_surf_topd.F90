!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!-----------------------------------------------------------
!     #######################
      SUBROUTINE RECHARGE_SURF_TOPD(PHI,PHT,KI)
!     #######################
!
!!****  *RECHARGE_SURF_TOPD*  
!!
!!    PURPOSE
!!    -------
!
!     
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault  * LTHE / Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   12/2003
!!                 03/2014 (B. Vincendon) use of the number of pixels included in a mesh and a watershed
!! 
!!    WARNING
!!    ----------------
!!     WFC is the threshold for deficits 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_COUPLING_TOPD, ONLY: NMASKI, NMASKT, XWFCTOPT, XDMAXFC, XWTOPT,&
                                XDTOPT, XWSTOPT, NNPIX, NNBV_IN_MESH
USE MODD_TOPODYN,       ONLY: NNCAT, XDMAXT
!
USE MODD_SURF_PAR,        ONLY: NUNDEF,XUNDEF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
INTEGER, INTENT(IN) :: KI    ! Grid dimensions
REAL, DIMENSION(:), INTENT(INOUT)   :: PHI   ! water content variation since last time step from ISBA (m)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PHT   ! water content variation to provide to TOPODYN to be distributed (m) 
!
!*      0.2    declarations of local variables
!
!
LOGICAL, DIMENSION(NNCAT,SIZE(NMASKI,3)) :: GTEST
INTEGER            :: J1,J2,J3,J4     ! loop control 
INTEGER            :: INBSAT, INBALL
!
REAL                           :: ZREST            ! m
REAL                           :: ZWNEW            ! m3/m3
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('RECHARGE_SURF_TOPD',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
!*       1.     ISBA => TOPODYN-LAT
!               -------------------
!
PHT(:,:)=0.
!
DO J3 = 1,KI
  !
  !The water content is lower than the previous one : this case is dealed with first to fasten the computation time.
  IF (PHI(J3) <= 0.0) THEN
    !
    DO J1 = 1,NNCAT
      !
      J4 = 1
      J2 = NMASKI(J3,J1,J4)
      DO WHILE (J2 /= NUNDEF .AND. J4<=NNBV_IN_MESH(J3,J1) )
        !
        IF ( NMASKT(J1,J2) /= NUNDEF ) THEN
          !
          ZWNEW = XWTOPT(J1,J2) + PHI(J3) / XDTOPT(J1,J2)
          !
          IF ( ZWNEW >= XWFCTOPT(J1,J2) ) THEN
            !
            ! on reste au-dessus de la capacite au champ, malgre l'assechement
            IF (XDMAXFC(J1,J2)/=XUNDEF) XDMAXT(J1,J2) = XDMAXFC(J1,J2) 
            PHT(J1,J2) = (ZWNEW - XWFCTOPT(J1,J2)) * XDTOPT(J1,J2)
            !
          ELSE ! on passe au-dessous de la capacite au champ
            !
            IF (XWSTOPT(J1,J2)/=XUNDEF) &
                    XDMAXT(J1,J2) = (XWSTOPT(J1,J2) - ZWNEW) * XDTOPT(J1,J2)
            PHT(J1,J2) = 0.0
            !
          ENDIF
          !
          J4 = J4+1
          IF ( J4<=SIZE(NMASKI,3) ) J2 = NMASKI(J3,J1,J4)
          !
        ELSE ! pixel non défini dans Isba
          !
          XDMAXT(J1,J2) = 0.0
          PHT = 0.0
          !
        ENDIF
        !
      ENDDO
      !
    ENDDO
    !
  ELSE ! recharge > 0.0
    !
    ZREST=1.
    GTEST(:,:)=.TRUE.
    !
    DO WHILE ( ZREST>0.0 )
      !
      ZREST=0.0
      !
      DO J1=1,NNCAT
        !
        J4=1
        J2=NMASKI(J3,J1,J4)
        !
        DO WHILE ( J2/=NUNDEF .AND. J4<=NNBV_IN_MESH(J3,J1) )
          !
          IF ( GTEST(J1,J4) .AND. NMASKT(J1,J2)/=NUNDEF ) THEN
            !
            ZWNEW = XWTOPT(J1,J2) + PHI(J3) / XDTOPT(J1,J2)
            !
            IF ( XWTOPT(J1,J2) == XWSTOPT(J1,J2) ) THEN ! pixel déjà saturé
              !
              XDMAXT(J1,J2) = 0.0
              PHT(J1,J2) = 0.0
              ZREST = ZREST + PHI(J3)
              GTEST(J1,J4) = .FALSE.
              !
            ELSE IF ( ( XWSTOPT(J1,J2) - XWTOPT(J1,J2) ) * XDTOPT(J1,J2) <= PHI(J3) ) THEN
              !
              ! pixel va se saturer
              XDMAXT(J1,J2) = XDMAXFC(J1,J2)
              PHT(J1,J2) = ( XWSTOPT(J1,J2) - XWFCTOPT(J1,J2) ) * XDTOPT(J1,J2)
              ZREST = ZREST + PHI(J3) - PHT(J1,J2)
              GTEST(J1,J4)=.FALSE.
              !
            ELSE IF ( XWTOPT(J1,J2) < XWFCTOPT(J1,J2) ) THEN 
              !
              ! en dessous de la capacité au champ avant d'ajouter la recharge
              IF ( (XWTOPT(J1,J2) + PHI(J3)/XDTOPT(J1,J2)) <= XWFCTOPT(J1,J2) ) THEN 
                !
                ! en dessous de la capacité au champ avec la recharge 
                XDMAXT(J1,J2) = ( XWSTOPT(J1,J2) - ZWNEW ) * XDTOPT(J1,J2)
                PHT(J1,J2) = 0.0
                !
              ELSE ! au-dessus de la capacité au champ avec la recharge
                !
                XDMAXT(J1,J2) = XDMAXFC(J1,J2)
                PHT(J1,J2) = ( ZWNEW - XWFCTOPT(J1,J2) ) * XDTOPT(J1,J2)
                !
              ENDIF
              !
            ELSE ! au-dessus de la capacité au champ avant d'ajouter la recharge
              !
              XDMAXT(J1,J2) = XDMAXFC(J1,J2)
              PHT(J1,J2) = ( ZWNEW - XWFCTOPT(J1,J2) ) * XDTOPT(J1,J2)
              !
            ENDIF
            !
          ELSE IF ( NMASKT(J1,J2)==NUNDEF ) THEN! pixel non défini dans Isba
            !
            XDMAXT(J1,J2) = 0.0
            PHT = 0.0
            !
          ENDIF
          !
          J4 = J4+1
          IF ( J4<=SIZE(NMASKI,3) ) J2 = NMASKI(J3,J1,J4)
          !
        ENDDO
        !
      ENDDO
      !
      IF ( ZREST/=0.0 ) THEN
        !
        INBSAT=COUNT(.NOT.GTEST) !nb de pixels saturés avec ou sans la recharge
        !
        IF ( INBSAT == NNPIX(J3) ) THEN
          !
          IF (NNPIX(J3) > 400 ) THEN
            WRITE(*,*) 'MAILLE NUM=',J3, 'nb pix tot=',NNPIX(J3)
           ! CALL ABOR1_SFX("RECHARGE_SURF_TOPD: TOO MANY PIXELS SATURATED ")
          ELSE
            ZREST=0.0
          ENDIF
          !
        ELSE
          !
          PHI(J3) = PHI(J3) + ( ZREST / (NNPIX(J3) - INBSAT) ) ! nouvelle recharge à distribuer
          !
        ENDIF
      ENDIF
      !
    ENDDO
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('RECHARGE_SURF_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE RECHARGE_SURF_TOPD
