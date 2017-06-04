!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_INIT_DEP_ISBA_n (CHI, NCHI, NP, DTCO, KPATCH, OCOVER, PCOVER, &
                                     KCH,KLUOUT,KLU)
!!    ##################################################
!!
!!*** *CH_INIT_DEP_ISBA_n*
!!
!!    PURPOSE
!!    -------
!        The purpose of this subroutine is to calculate the surface flux
!     (emission or deposition) for the chemical (=scalar) variable
!!
!!**  METHOD
!!    ------
!!       The surface flux will be calculated using an exchange velocity
!!    and a surface mixing ratio for each species:
!!    Flux = v_exchange * ( C(first model level) )
!!    Exchange velocity and surface concentration will be read from
!!    the general purpose input file, or, 
!!    if CCH_DRY_DEP = "WES89", the Wesley method will be used
!!    to calculate the dry deposition velocities. The wesely code
!!    has been separate into four subroutines: ch_teb_depn.f90 for town area;
!!    ch_water_depn.f90 for inland water; ch_sea_depn.f90 for sea and
!!    ch_isba_depn.f90 for nature area.
!!
!!    information read from the input file:
!!       surface values SURFVALU
!!       exchange velocities EXCHGVEL (non-Wesley)
!!
!!    in addition for Wesley:
!!       chemical molecular diffusivity MASS_MOL
!!       molecular reactivity factor REA_FACT
!!       surface resistance SURF_RES
!!       molecular effective Henry constant HENRY_SP
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!    K. Suhre    *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 17/11/95
!!    05/08/96 (K. Suhre) restructured
!!    19/02/98 (P. Tulet) add explicit dry deposition for chemical species
!!    11/08/98 (N. Asencio) add parallel code
!!    29/03/99 (K. Suhre) add IKB = MIN(2,SIZE(PSVT,3)) 
!!                        so that this subroutine can be called by the box model
!!    16/01/01 (P. Tulet) restructured
!!    18/01/01 (P. Tulet) add patch vegetative class, town and water/sea 
!!                        for friction velocity and aerodynamical resistance 
!!    18/07/03 (P. Tulet) surface externalisation
!!    
!!
!!    EXTERNAL
!!    --------
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t, CH_ISBA_NP_t
USE MODD_ISBA_n, ONLY : ISBA_NP_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODI_CH_OPEN_INPUTB  ! open the general purpose ASCII input file
USE MODI_CONVERT_COVER_CH_ISBA
!
USE MODD_CH_ISBA,        ONLY: XRCCLAYSO2, XRCCLAYO3, XRCSANDSO2, XRCSANDO3, &
                                 XRCSNOWSO2, XRCSNOWO3, XLANDREXT  
USE MODD_CH_SURF
USE MODD_SURF_PAR,       ONLY : XUNDEF
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(CH_ISBA_NP_t), INTENT(INOUT) :: NCHI
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
INTEGER, INTENT(IN) :: KPATCH
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:), INTENT(IN) :: PCOVER
!
INTEGER,                         INTENT(IN)  :: KCH      ! chemistry input file
INTEGER,                         INTENT(IN)  :: KLUOUT   ! output listing channel
INTEGER,                         INTENT(IN)  :: KLU      ! number of points
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=40) :: YFORMAT    
! format for input
 CHARACTER(LEN=40) :: YOUTFORMAT = '(A32,2E15.5)'
! format for output
INTEGER :: IRESIS         ! number of chemical reactivity factor to be read
 CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE :: YRESISNAME !resistance name
REAL             , DIMENSION(:), ALLOCATABLE :: ZRESISVAL 
! chemical reactivity factor value
!
INTEGER :: JI, JNREAL, JP ! loop control variables
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!=============================================================================
!
  !
  !        1. Default values
  !        
  IF (LHOOK) CALL DR_HOOK('CH_INIT_DEP_ISBA_N',0,ZHOOK_HANDLE)
  XRCCLAYSO2 = XUNDEF
  XRCCLAYO3  = XUNDEF
  XRCSANDSO2 = XUNDEF
  XRCSANDO3  = XUNDEF
  XRCSNOWSO2 = XUNDEF
  XRCSNOWO3  = XUNDEF
  XLANDREXT  = XUNDEF
  !
  !-----------------------------------------------------------------------------
  !
  !
  IF (CHI%CCH_DRY_DEP == "WES89") THEN
    !
    !*       2.    Physiographic fields
    !
    DO JP = 1,KPATCH
      !
      ALLOCATE(NCHI%AL(JP)%XSOILRC_SO2(NP%AL(JP)%NSIZE_P))
      ALLOCATE(NCHI%AL(JP)%XSOILRC_O3 (NP%AL(JP)%NSIZE_P))
      !
      CALL CONVERT_COVER_CH_ISBA(DTCO, PCOVER, OCOVER, KPATCH, JP, NP%AL(JP), &
                               NCHI%AL(JP)%XSOILRC_SO2, NCHI%AL(JP)%XSOILRC_O3)
      !
      !---------------------------------------------------------------------------
      !
      !
      !*       3.    read surface resistance SURF_RES
      !
      ALLOCATE(NCHI%AL(JP)%XDEP(KLU,CHI%SVI%NBEQ))
      !
    ENDDO
    !
    ! open input file
    WRITE(KLUOUT,*) &
           "CH_INIT_DEP_ISBA_n: reading  reactivity factor "  
    CALL CH_OPEN_INPUTB("SURF_RES", KCH, KLUOUT)
    !
    ! read number of input surface resistance IRESIS
    READ(KCH, *) IRESIS
    WRITE(KLUOUT,*) "number of reactivity factor : ", IRESIS
    !
    ! read data input format
    READ(KCH,"(A)") YFORMAT
    WRITE(KLUOUT,*) "input format is: ", YFORMAT
    !
    ! allocate fields
    ALLOCATE(YRESISNAME(IRESIS))
    ALLOCATE(ZRESISVAL(IRESIS))
    !
    ! read reactivity factor 
    DO JI = 1, IRESIS
      READ(KCH,YFORMAT) YRESISNAME(JI), ZRESISVAL(JI)
      WRITE(KLUOUT,YFORMAT) YRESISNAME(JI), ZRESISVAL(JI)
    END DO
!
    ! close file
    DO JNREAL = 1, IRESIS
      IF ('LANDREXT'== YRESISNAME(JNREAL) (1:8)) XLANDREXT = ZRESISVAL(JNREAL) 
      IF ('RCSANDSO2'== YRESISNAME(JNREAL) (1:9)) XRCSANDSO2 = ZRESISVAL(JNREAL)
      IF ('RCSANDO3'== YRESISNAME(JNREAL) (1:8)) XRCSANDO3  = ZRESISVAL(JNREAL) 
      IF ('RCCLAYSO2'== YRESISNAME(JNREAL) (1:9)) XRCCLAYSO2 = ZRESISVAL(JNREAL)
      IF ('RCCLAYO3'== YRESISNAME(JNREAL) (1:8)) XRCCLAYO3  = ZRESISVAL(JNREAL) 
      IF ('RCSNOWSO2'== YRESISNAME(JNREAL) (1:9)) XRCSNOWSO2 = ZRESISVAL(JNREAL)
      IF ('RCSNOWO3'== YRESISNAME(JNREAL) (1:8)) XRCSNOWO3  = ZRESISVAL(JNREAL) 
    END DO
    !
      WRITE(KLUOUT,'(A)') '----------------------------------------------------'
      WRITE(KLUOUT,'(A)') 'SURF_RES'
      WRITE(KLUOUT,'(A)') 'surface resistances (s/m)'
      WRITE(KLUOUT,'(I4)') 7
      WRITE(KLUOUT,'(A)') YOUTFORMAT
      WRITE(KLUOUT,YOUTFORMAT) 'LANDREXT',   XLANDREXT 
      WRITE(KLUOUT,YOUTFORMAT) 'RCSANDSO2',  XRCSANDSO2 
      WRITE(KLUOUT,YOUTFORMAT) 'RCSANDO3',   XRCSANDO3  
      WRITE(KLUOUT,YOUTFORMAT) 'RCCLAYSO2',  XRCCLAYSO2 
      WRITE(KLUOUT,YOUTFORMAT) 'RCCLAYO3',   XRCCLAYO3  
      WRITE(KLUOUT,YOUTFORMAT) 'RCSNOWSO2',  XRCSNOWSO2 
      WRITE(KLUOUT,YOUTFORMAT) 'RCSNOWO3',   XRCSNOWO3  
    !
   DEALLOCATE(YRESISNAME)
   DEALLOCATE(ZRESISVAL)
  ELSE
    DO JP = 1,KPATCH
      ALLOCATE(NCHI%AL(JP)%XDEP(0,0))
    ENDDO
  END IF
IF (LHOOK) CALL DR_HOOK('CH_INIT_DEP_ISBA_N',1,ZHOOK_HANDLE)
  !
  !=============================================================================
  !
END SUBROUTINE CH_INIT_DEP_ISBA_n
