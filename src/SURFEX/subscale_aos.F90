!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SUBSCALE_AOS (U, UG, USS, OZ0EFFI, OZ0EFFJ)
!     #############################################
!
!!*SUBSCALE_AOS  computes the sum of the ratio: (h'-h)/L when  h'/L >h/L  
!!                  the ' is for subgrid scale orography
!!
!!
!!    METHOD
!!    ------
!!    See M.Georgelin and al. July 1994, Monthly Weather Review.
!!   
!!    EXTERNAL
!!    --------
!!
!!    AUTHOR
!!    ------
!!
!!    M. Georgelin      Laboratoire d'Aerologie
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    18/12/95
!!    (V. Masson) 10/03/97 rewrites the routine in doctor norm.
!!                         computations are made even if a only a few subsquares
!!                         contains data points.
!!                         returns to the calling routine the localization of
!!                         the points where the z0 coefficients are available.
!!    (V. Masson) 03/2004  externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PGDWORK,        ONLY : NSSO, XSSQO, LSSQO
USE MODD_PGD_GRID,       ONLY : NL
!
USE MODI_GET_ADJACENT_MESHES
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_MESH_DIM
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SSO_t), INTENT(INOUT) :: USS
!
LOGICAL, DIMENSION(:), INTENT(OUT) :: OZ0EFFI! .T. : the z0eff coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
LOGICAL, DIMENSION(:), INTENT(OUT) :: OZ0EFFJ! .T. : the z0eff coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
!
!*    0.2    Declaration of indexes
!            ----------------------
!
INTEGER :: JS1, JS2
INTEGER :: JL          ! loop index on grid meshs
INTEGER :: IL          ! grid mesh index of second subgrid point used
INTEGER :: JISS, JJSS  ! loop indexes for subsquares arrays
INTEGER :: JNEXT       ! loop index on subgrid meshes
INTEGER :: INEXT       ! index to add to JISS or JJSS to obtain the following
!                      ! point containing a data in a segment
INTEGER, DIMENSION(:), ALLOCATABLE :: ILEFT   ! index of left   grid mesh 
INTEGER, DIMENSION(:), ALLOCATABLE :: IRIGHT  ! index of right  grid mesh 
INTEGER, DIMENSION(:), ALLOCATABLE :: ITOP    ! index of top    grid mesh 
INTEGER, DIMENSION(:), ALLOCATABLE :: IBOTTOM ! index of bottom grid mesh
!
!*    0.3    Declaration of counters inside a grid (JL)
!            -----------------------
!
INTEGER :: IHO2COUNTERJP ! number of times where h/2 has been summed for JP coef
INTEGER :: IHO2COUNTERJM ! number of times where h/2 has been summed for JM coef
INTEGER :: IHO2COUNTERIP ! number of times where h/2 has been summed for IP coef
INTEGER :: IHO2COUNTERIM ! number of times where h/2 has been summed for IM coef
INTEGER :: IAOSCOUNTER   ! number of segments where A/S has been summed
INTEGER :: IAOSDIST ! distance between first and last subsquares used in
!                   ! computation of A/S in a subsegment of the grid
LOGICAL :: GFIRST   ! T indicates the first point has been found for this segment.
!
!*    0.4    Declaration of working arrays inside a grid (JL)
!            -----------------------------
!
REAL, DIMENSION(NSSO) :: ZAOSIP ! A/S in each subsegment for IP coef.
REAL, DIMENSION(NSSO) :: ZAOSIM ! A/S in each subsegment for IM coef.
REAL, DIMENSION(NSSO) :: ZAOSJP ! A/S in each subsegment for JP coef.
REAL, DIMENSION(NSSO) :: ZAOSJM ! A/S in each subsegment for JM coef.
REAL :: ZAIP      ! Area in a subsegment for IP coef.
REAL :: ZAIM      ! Area in a subsegment for IM coef.
REAL :: ZAJP      ! Area in a subsegment for JP coef.
REAL :: ZAJM      ! Area in a subsegment for JM coef.
REAL :: ZSUMHO2IP ! sum of h/2 in the grid for IP coef.
REAL :: ZSUMHO2IM ! sum of h/2 in the grid for IM coef.
REAL :: ZSUMHO2JP ! sum of h/2 in the grid for JP coef.
REAL :: ZSUMHO2JM ! sum of h/2 in the grid for JM coef.
REAL :: ZSSAOS    ! A/S between 2 following points along a subsegment
REAL :: ZSLOPE    ! slope between 2 following points along a subsegment
REAL :: ZDXEFF    ! width of a subsquare along I axis
REAL :: ZDYEFF    ! width of a subsquare along J axis
!
!*    0.5    Declaration of other local variables
!            ------------------------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISSO, ISSOT
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ISSQOT, ISSQO
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZSSQO
LOGICAL, DIMENSION(:), ALLOCATABLE :: GZ0EFFI, GZ0EFFJ
REAL, DIMENSION(:), ALLOCATABLE   :: ZDX, ZDY, ZSEA, ZAVG_ZS, ZMESH_SIZE    ! grid mesh size in x direction
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSIP_ALL, ZAOSIM_ALL, ZAOSJP_ALL, ZAOSJM_ALL 
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2IP_ALL, ZHO2IM_ALL, ZHO2JP_ALL, ZHO2JM_ALL 
REAL :: ZSLOPEIP ! x mean slope
REAL :: ZSLOPEJP ! y mean slope
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
!*    1.1    Occurence of computation of the coefficients
!            --------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_AOS',0,ZHOOK_HANDLE)
OZ0EFFI(:)=.FALSE.
OZ0EFFJ(:)=.FALSE.
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZSEA      (U%NDIM_FULL))
  ALLOCATE(ZAVG_ZS   (U%NDIM_FULL))
  ALLOCATE(ZMESH_SIZE(U%NDIM_FULL))  
  ALLOCATE(ISSQOT(U%NDIM_FULL,NSSO,NSSO))
  ALLOCATE(ZSSQO (U%NDIM_FULL,NSSO,NSSO))
ELSE
  ALLOCATE(ZSEA      (0))
  ALLOCATE(ZAVG_ZS   (0)) 
  ALLOCATE(ISSQOT(0,0,0))
  ALLOCATE(ZSSQO (0,0,0))
ENDIF
!
 CALL GATHER_AND_WRITE_MPI(U%XSEA,ZSEA)
 CALL GATHER_AND_WRITE_MPI(USS%XAVG_ZS,ZAVG_ZS)
 CALL GATHER_AND_WRITE_MPI(UG%G%XMESH_SIZE,ZMESH_SIZE)  
 CALL GATHER_AND_WRITE_MPI(XSSQO,ZSSQO)
!
ALLOCATE(ISSQO(SIZE(XSSQO,1),NSSO,NSSO))
ISSQO(:,:,:) = 0
WHERE (LSSQO(:,:,:)) ISSQO(:,:,:) = 1
 CALL GATHER_AND_WRITE_MPI(ISSQO,ISSQOT)
DEALLOCATE(ISSQO)
!
!*    1.2    Grid dimension (meters)
!            -----------------------
!
IF (NRANK==NPIO) THEN
  !
  ALLOCATE(ZDX(U%NDIM_FULL),ZDY(U%NDIM_FULL))
  CALL GET_MESH_DIM(UG%G%CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,ZDX,ZDY,ZMESH_SIZE)
  DEALLOCATE(ZMESH_SIZE)
  !
  !
  !*    1.3    Left, top, right and bottom adjacent gris meshes
  !            ------------------------------------------------
  !
  ALLOCATE(ILEFT(U%NDIM_FULL),IRIGHT(U%NDIM_FULL),ITOP(U%NDIM_FULL),IBOTTOM(U%NDIM_FULL))
  CALL GET_ADJACENT_MESHES(UG%G%CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,&
                                ILEFT,IRIGHT,ITOP,IBOTTOM)
  !
  ALLOCATE(GZ0EFFI(U%NDIM_FULL),GZ0EFFJ(U%NDIM_FULL))
  ALLOCATE(ZHO2IM_ALL(U%NDIM_FULL),ZHO2IP_ALL(U%NDIM_FULL),&
           ZAOSIM_ALL(U%NDIM_FULL),ZAOSIP_ALL(U%NDIM_FULL))
  ALLOCATE(ZHO2JM_ALL(U%NDIM_FULL),ZHO2JP_ALL(U%NDIM_FULL),&
           ZAOSJM_ALL(U%NDIM_FULL),ZAOSJP_ALL(U%NDIM_FULL))   
  GZ0EFFI(:) = .FALSE.
  GZ0EFFJ(:) = .FALSE.
  !
  !*    1.4    Mean slopes between 2 grid meshes
  !            -----------
  !
  !
  DO JL=1,U%NDIM_FULL
    !
    ZSLOPEIP = 0.
    ZSLOPEJP = 0.
    !
    IF (IRIGHT(JL)/=0 .AND. ILEFT(JL)/=0) THEN
      ZSLOPEIP =  0.5 * ( ZAVG_ZS(IRIGHT(JL)) - ZAVG_ZS(JL) ) &
                            / ( 0.5 * (ZDX(IRIGHT(JL)) + ZDX(JL)) ) &
                      + 0.5 * ( ZAVG_ZS(JL) - ZAVG_ZS(ILEFT (JL)) ) &
                            / ( 0.5 * (ZDX(JL)  + ZDX(ILEFT(JL))) )  
    ELSE
      ZSLOPEIP = 0.
    END IF
    IF (ITOP(JL)/=0 .AND. IBOTTOM(JL)/=0) THEN
      ZSLOPEJP =  0.5 * ( ZAVG_ZS(ITOP(JL))     - ZAVG_ZS(JL) ) &
                            / ( 0.5 * (ZDY(ITOP(JL))     + ZDY(JL)) ) &
                      + 0.5 * ( ZAVG_ZS(JL) - ZAVG_ZS(IBOTTOM (JL)) ) &
                            / ( 0.5 * (ZDY(JL)  + ZDY(IBOTTOM(JL))) )  
    ELSE
      ZSLOPEJP = 0.
    END IF
!
!----------------------------------------------------------------------------
!
!*    2.     Loop on grid points
!            -------------------
!!
!*    2.1    No land in grid mesh
!            --------------------
!
    IF (ZSEA(JL)==1.) CYCLE
!
!*    2.2    Index Initializations
!            ---------------------
!
    ZDXEFF=ZDX(JL)/FLOAT(NSSO)
    ZDYEFF=ZDY(JL)/FLOAT(NSSO)
!
!----------------------------------------------------------------------------
!
!*    3.     Computations for IP and IM fields
!            ---------------------------------
!
    ZAOSIP(:)=0.
    ZAOSIM(:)=0.
    ZSUMHO2IP=0.
    ZSUMHO2IM=0.
    IHO2COUNTERIP=0
    IHO2COUNTERIM=0
    IAOSCOUNTER=0
!
!*    3.1    loop on jss index (there is no specific computation along j)
!            -----------------
!
    DO JJSS=1,NSSO
!
!*    3.1.1  initializes counters for the A/S subscale segment computation
!
      GFIRST = .TRUE.
      IAOSDIST=0
      ZAIP=0.
      ZAIM=0.
!
!*    3.2    loop on iss index
!            -----------------
!
      DO JISS=1,NSSO
!
!*    3.3    search for two consecutive grid points
!            --------------------------------------
!
!*    3.3.1 first one
!
        IF (ISSQOT(JL,JISS,JJSS)==0 ) CYCLE
!
!*    3.3.2  second one (up to one grid mesh further)
!
        DO JNEXT=1,NSSO
          IF (JISS+JNEXT>NSSO) THEN
            IL    = IRIGHT(JL)
            INEXT = JISS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JISS+JNEXT
          END IF
          ! no right point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (ISSQOT(IL,INEXT,JJSS)==1) EXIT
        END DO
!
!*    3.3.3  none found: end of loop along jss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    3.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!*    3.4    add terms to sums of A/S and h/2
!            --------------------------------
!
        IF (GFIRST) IAOSCOUNTER=IAOSCOUNTER+1
        GFIRST = .FALSE.
        IAOSDIST   =IAOSDIST+JNEXT
!
!*    3.4.1  mean slope
!
        ZSLOPE=ZSLOPEIP
!
!*    3.4.2  A/S term
!
        ZSSAOS =  ZSSQO(IL,INEXT,JJSS) - ZSSQO(JL,JISS,JJSS) &
                  - ZSLOPE * ZDXEFF * JNEXT  
        IF (ZSSAOS>0.) ZAIP=ZAIP+ZSSAOS
        IF (ZSSAOS<0.) ZAIM=ZAIM-ZSSAOS
!
!*    3.4.3  h/2 term
!
        IF (ZSSAOS>0.) THEN
          ZSUMHO2IP = ZSUMHO2IP + 0.5 * ZSSAOS
          IHO2COUNTERIP=IHO2COUNTERIP+1
        END IF
        IF (ZSSAOS<0.) THEN
          ZSUMHO2IM = ZSUMHO2IM - 0.5 * ZSSAOS
          IHO2COUNTERIM=IHO2COUNTERIM+1
        END IF
!
!*    3.5    end of loop on iss index
!            ------------------------
!
      END DO
      IF (IAOSDIST>0) THEN
        ZAOSIP(JJSS)=ZAIP/(ZDXEFF*IAOSDIST)
        ZAOSIM(JJSS)=ZAIM/(ZDXEFF*IAOSDIST)
      END IF
!
!*    3.6    end of loop on jss index
!            ------------------------
!
    END DO
!
!*    3.7    end of IP and IM coefficients
!            -----------------------------
!
    IF (IAOSCOUNTER>0) THEN
      ZAOSIP_ALL(JL)=SUM(ZAOSIP) / IAOSCOUNTER
      ZAOSIM_ALL(JL)=SUM(ZAOSIM) / IAOSCOUNTER
      IF (IHO2COUNTERIP>0) THEN
        ZHO2IP_ALL(JL)=ZSUMHO2IP   / IHO2COUNTERIP
      ELSE
        ZHO2IP_ALL(JL)=0.
      END IF
      IF (IHO2COUNTERIM>0) THEN
        ZHO2IM_ALL(JL)=ZSUMHO2IM   / IHO2COUNTERIM
      ELSE
        ZHO2IM_ALL(JL)=0.
      END IF
      GZ0EFFI(JL)=.TRUE.
    END IF
!
!----------------------------------------------------------------------------
!
!*    4.     Computations for JP and JM fields
!            ---------------------------------
!
    ZAOSJP(:)=0.
    ZAOSJM(:)=0.
    ZSUMHO2JP=0.
    ZSUMHO2JM=0.
    IHO2COUNTERJP=0
    IHO2COUNTERJM=0
    IAOSCOUNTER=0
!
!*    4.1    loop on iss index (there is no specific computation along i)
!            -----------------
!
    DO JISS=1,NSSO
!
!*    4.1.1  initializes counters for the A/S subscale segment computation
!
      GFIRST = .TRUE.
      IAOSDIST=0
      ZAJP=0.
      ZAJM=0.
!
!*    4.2    loop on jss index
!            -----------------
!
      DO JJSS=1,NSSO
!
!*    4.3    search for two consecutive grid points
!            --------------------------------------
!
!*    4.3.1 first one
!
        IF (ISSQOT(JL,JISS,JJSS)==0 ) CYCLE
!
!*    4.3.2  second one (up to one grid mesh further)
!
        DO JNEXT=1,NSSO
          IF (JJSS+JNEXT>NSSO) THEN
            IL    = ITOP(JL)
            INEXT = JJSS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JJSS+JNEXT
          END IF
          ! no right point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (ISSQOT(IL,JISS,INEXT)==1) EXIT
        END DO
!
!*    4.3.3  none found: end of loop along jss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    4.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!
!*    4.4    add terms to sums of A/S and h/2
!            --------------------------------
!
        IF (GFIRST) IAOSCOUNTER=IAOSCOUNTER+1
        GFIRST = .FALSE.
        IAOSDIST   =IAOSDIST+JNEXT
!
!*    4.4.1  mean slope
!
        ZSLOPE=ZSLOPEJP
!
!*    4.4.2  A/S term
!
        ZSSAOS =  ZSSQO(IL,JISS,INEXT) - ZSSQO(JL,JISS,JJSS) &
                  - ZSLOPE * ZDYEFF * JNEXT  
        IF (ZSSAOS>0.) ZAJP=ZAJP+ZSSAOS
        IF (ZSSAOS<0.) ZAJM=ZAJM-ZSSAOS
!
!*    4.4.3  h/2 term
!
        IF (ZSSAOS>0.) THEN
          ZSUMHO2JP = ZSUMHO2JP + 0.5 * ZSSAOS
          IHO2COUNTERJP=IHO2COUNTERJP+1
        END IF
        IF (ZSSAOS<0.) THEN
          ZSUMHO2JM = ZSUMHO2JM - 0.5 * ZSSAOS
          IHO2COUNTERJM=IHO2COUNTERJM+1
        END IF
!
!*    4.5    end of loop on jss index
!            ------------------------
!
      END DO
      IF (IAOSDIST>0) THEN
        ZAOSJP(JISS)=ZAJP/(ZDYEFF*IAOSDIST)
        ZAOSJM(JISS)=ZAJM/(ZDYEFF*IAOSDIST)
      END IF
!
!*    4.6    end of loop on iss index
!            ------------------------
!
    END DO
!
!*    4.7    end of JP and JM coefficients
!            -----------------------------
!
    IF (IAOSCOUNTER>0) THEN
      ZAOSJP_ALL(JL)=SUM(ZAOSJP) /IAOSCOUNTER
      ZAOSJM_ALL(JL)=SUM(ZAOSJM) /IAOSCOUNTER
      IF (IHO2COUNTERJP>0) THEN
        ZHO2JP_ALL(JL)=ZSUMHO2JP   /IHO2COUNTERJP
      ELSE
        ZHO2JP_ALL(JL)=0.
      END IF
      IF (IHO2COUNTERJM>0) THEN
        ZHO2JM_ALL(JL)=ZSUMHO2JM   /IHO2COUNTERJM
      ELSE
        ZHO2JM_ALL(JL)=0.
      END IF
      GZ0EFFJ(JL)=.TRUE.
    END IF
!
  END DO
  ! 
ELSE
  ALLOCATE(ZHO2IM_ALL(0),ZHO2IP_ALL(0),ZAOSIM_ALL(0),ZAOSIP_ALL(0))
  ALLOCATE(ZHO2JM_ALL(0),ZHO2JP_ALL(0),ZAOSJM_ALL(0),ZAOSJP_ALL(0))
ENDIF
!
DEALLOCATE(ZSSQO,ISSQOT)
DEALLOCATE(ZSEA)  
DEALLOCATE(ZAVG_ZS)
!
 CALL READ_AND_SEND_MPI(ZAOSIP_ALL,USS%XAOSIP)
 CALL READ_AND_SEND_MPI(ZAOSIM_ALL,USS%XAOSIM)
 CALL READ_AND_SEND_MPI(ZHO2IP_ALL,USS%XHO2IP)
 CALL READ_AND_SEND_MPI(ZHO2IM_ALL,USS%XHO2IM)
DEALLOCATE(ZHO2IM_ALL,ZHO2IP_ALL,ZAOSIM_ALL,ZAOSIP_ALL)
!
 CALL READ_AND_SEND_MPI(ZAOSJP_ALL,USS%XAOSJP)
 CALL READ_AND_SEND_MPI(ZAOSJM_ALL,USS%XAOSJM)
 CALL READ_AND_SEND_MPI(ZHO2JP_ALL,USS%XHO2JP)
 CALL READ_AND_SEND_MPI(ZHO2JM_ALL,USS%XHO2JM)
DEALLOCATE(ZHO2JM_ALL,ZHO2JP_ALL,ZAOSJM_ALL,ZAOSJP_ALL)
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ISSOT(U%NDIM_FULL))
  ISSOT(:) = 0
  WHERE (GZ0EFFI(:)) ISSOT(:) = 1
  DEALLOCATE(GZ0EFFI)
ELSE
  ALLOCATE(ISSOT(0))
ENDIF
ALLOCATE(ISSO(NL))
 CALL READ_AND_SEND_MPI(ISSOT,ISSO)
WHERE(ISSO(:)==1) OZ0EFFI(:) = .TRUE.
!
IF (NRANK==NPIO) THEN
  ISSOT(:) = 0
  WHERE (GZ0EFFJ(:)) ISSOT(:) = 1
  DEALLOCATE(GZ0EFFJ)
ENDIF
 CALL READ_AND_SEND_MPI(ISSOT,ISSO)
WHERE(ISSO(:)==1) OZ0EFFJ(:) = .TRUE.
DEALLOCATE(ISSO)
!
DEALLOCATE(ISSOT)
!
!-------------------------------------------------------------------------------
!
!*    5.     Next grid point
!            ---------------
!

IF (LHOOK) CALL DR_HOOK('SUBSCALE_AOS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUBSCALE_AOS
