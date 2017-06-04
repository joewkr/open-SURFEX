!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE CH_INIT_DEPCONST(HPROGRAM,HCHEM_SURF_FILE,KLUOUT,HSV)
!!    ##################################################
!!
!!*** *CH_INIT_DEPCONST*
!!
!!    PURPOSE
!!    -------
!      Read Henry Specific constant,  Molecular Mass and Biological reactivity
!!     factor.
!!     
!!
!!**  METHOD
!!    ------
!
!!    Chemical constant will be read from
!!    the general purpose input file . 
!!
!!       chemical molecular diffusivity MASS_MOL
!!       molecular reactivity factor REA_FACT     
!!       molecular effective Henry constant HENRY_SP
!!
!!
!!    REFERENCE
!!    ---------
!!    
!!    AUTHOR
!!    ------
!!    P. Tulet    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 16/02/01

!!    EXTERNAL
!!    --------
!!
! open the general purpose ASCII input file
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODI_CH_OPEN_INPUTB
USE MODD_CH_SURF
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
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! Program name
CHARACTER(LEN=28), INTENT(IN)  :: HCHEM_SURF_FILE ! ascii file for chemistry aggregation
INTEGER,                  INTENT(IN)  :: KLUOUT   ! output listing channel
 CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN)  :: HSV      ! name of chemical species
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=40) :: YFORMAT    
                          ! format for input
 CHARACTER(LEN=40) :: YOUTFORMAT = '(A32,2E15.5)'
                          ! format for output
!
INTEGER :: IMASS          ! number of molecular diffusivity to be read
 CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE :: YMASSMOLNAME !species names
REAL             , DIMENSION(:), ALLOCATABLE :: ZMASSMOLVAL
                          ! molecular diffusivity value
!
INTEGER :: IREACT         ! number of chemical reactivity factor to be read
 CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE :: YREACTNAME !species names
REAL             , DIMENSION(:), ALLOCATABLE :: ZREACTVAL 
                          ! chemical reactivity factor value
!
INTEGER :: IHENRY         ! number of chemical Henry constant to be read
 CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE :: YHENRYNAME !species names
 character(len=50) :: yname
REAL             , DIMENSION(:,:), ALLOCATABLE :: ZHENRYVAL
                          !chemical Henry constant value
!
INTEGER :: ICH      ! unit of input chemical file
!
INTEGER :: JI, JN, JNREAL ! loop control variables
INTEGER :: INACT          ! array pointer
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    ALLOCATE FIELD
!              --------------
!
IF (LHOOK) CALL DR_HOOK('CH_INIT_DEPCONST',0,ZHOOK_HANDLE)
IF(.NOT. ALLOCATED(XSREALMASSMOLVAL)) ALLOCATE( XSREALMASSMOLVAL(SIZE(HSV,1)) )
IF(.NOT. ALLOCATED(XSREALREACTVAL)  ) ALLOCATE( XSREALREACTVAL(SIZE(HSV,1)) )
IF(.NOT. ALLOCATED(XSREALHENRYVAL)  ) ALLOCATE( XSREALHENRYVAL(SIZE(HSV,1),2) )
!
 CALL OPEN_NAMELIST(HPROGRAM,ICH,HFILE=HCHEM_SURF_FILE)
!
!*       2.  read chemical molecular diffusivity MASS_MOL
!
! open input file
  WRITE(KLUOUT,*) &
       "CH_INIT_CONST: reading  molar mass" 
  CALL CH_OPEN_INPUTB("MASS_MOL", ICH, KLUOUT)
!
! read number of molecular diffusivity IMASS
  READ(ICH, *) IMASS
  WRITE(KLUOUT,*) "number of molecular diffusivity: ", IMASS
!
! read data input format
  READ(ICH,"(A)") YFORMAT
  WRITE(KLUOUT,*) "input format is: ", YFORMAT
!
! allocate fields
  ALLOCATE(YMASSMOLNAME(IMASS))
  ALLOCATE(ZMASSMOLVAL(IMASS))
!
! read molecular diffusivity
  DO JI = 1, IMASS
    READ(ICH,YFORMAT) YMASSMOLNAME(JI), ZMASSMOLVAL(JI)
    WRITE(KLUOUT,YFORMAT) YMASSMOLNAME(JI), ZMASSMOLVAL(JI)
  END DO
!
!
    WRITE(KLUOUT,'(A)') '----------------------------------------------------'
    WRITE(KLUOUT,'(A)') 'MASS_MOL'
    WRITE(KLUOUT,'(A)') 'molecular mass (in g/mol) for molecular diffusion'
    WRITE(KLUOUT,'(I4)') SIZE(HSV,1)
    WRITE(KLUOUT,'(A)') YOUTFORMAT
!
  XSREALMASSMOLVAL(:) = 50. ! default molecular mass 
  DO JNREAL = 1, SIZE(HSV,1)
    INACT = 0
    search_loop3 : DO JN = 1, IMASS
      IF (HSV(JNREAL) .EQ. YMASSMOLNAME(JN)) THEN
        INACT = JN
        EXIT search_loop3
      END IF
    END DO search_loop3
    IF (INACT .NE. 0) XSREALMASSMOLVAL(JNREAL) = ZMASSMOLVAL(INACT)
      WRITE(KLUOUT,YOUTFORMAT) HSV(JNREAL), XSREALMASSMOLVAL(JNREAL)
  END DO
!
!
!-----------------------------------------------------------------------------
!
!*       3.  read molecular reactivity factor REA_FACT
!
! open input file
   WRITE(KLUOUT,*) &
       "CH_INIT_CONST: reading  reactivity factor "  
  CALL CH_OPEN_INPUTB("REA_FACT", ICH, KLUOUT)
!
! read number of molecular diffusivity IREACT
  READ(ICH, *) IREACT
  WRITE(KLUOUT,*) "number of reactivity factor : ", IREACT
!
! read data input format
  READ(ICH,"(A)") YFORMAT
  WRITE(KLUOUT,*) "input format is: ", YFORMAT
!
! allocate fields
  ALLOCATE(YREACTNAME(IREACT))
  ALLOCATE(ZREACTVAL(IREACT))
! read reactivity factor 
  DO JI = 1, IREACT
    READ(ICH,YFORMAT) YREACTNAME(JI), ZREACTVAL(JI)
    WRITE(KLUOUT,YFORMAT) YREACTNAME(JI), ZREACTVAL(JI)
  END DO
!
    WRITE(KLUOUT,'(A)') '----------------------------------------------------'
    WRITE(KLUOUT,'(A)') 'REA_FACT'
    WRITE(KLUOUT,'(A)') 'reactivity factor'
    WRITE(KLUOUT,'(I4)') SIZE(HSV,1)
    WRITE(KLUOUT,'(A)') YOUTFORMAT
!    
  XSREALREACTVAL(:) = 0.0 ! default (high surface resistance)
  DO JNREAL = 1, SIZE(HSV,1)
    INACT = 0
    search_loop4 : DO JN = 1, IREACT
      IF (HSV(JNREAL) .EQ. YREACTNAME(JN)) THEN
        INACT = JN
        EXIT search_loop4
      END IF
    END DO search_loop4
    IF (INACT .NE. 0) XSREALREACTVAL(JNREAL) = ZREACTVAL(INACT)
      WRITE(KLUOUT,YOUTFORMAT) HSV(JNREAL), XSREALREACTVAL(JNREAL)
  END DO
!
!
!-----------------------------------------------------------------------------
!
!*       4.  read molecular effective  Henry constant HENRY_SP
!
! open input file
  WRITE(KLUOUT,*) &
       "CH_INIT_CONST: reading effective Henry constant", &
       " and its temperature correction "  
  CALL CH_OPEN_INPUTB("HENRY_SP", ICH, KLUOUT)
!
! read number of molecular diffusivity IHENRY
  READ(ICH, *) IHENRY
  WRITE(KLUOUT,*) "number of reactivity factor : ", IHENRY
!
! read data input format
  READ(ICH,"(A)") YFORMAT
  WRITE(KLUOUT,*) "input format is: ", YFORMAT
!
! allocate fields
  ALLOCATE(YHENRYNAME(IHENRY))
  ALLOCATE(ZHENRYVAL(IHENRY,2))
!
! read reactivity factor 
  DO JNREAL = 1, IHENRY
    READ(ICH,YFORMAT) YHENRYNAME(JNREAL), ZHENRYVAL(JNREAL,1),&
                             ZHENRYVAL(JNREAL,2)  
    WRITE(KLUOUT,YFORMAT) YHENRYNAME(JNREAL), ZHENRYVAL(JNREAL,1),&
                             ZHENRYVAL(JNREAL,2)  
  END DO
!
  WRITE(KLUOUT,'(A)') '----------------------------------------------------'
  WRITE(KLUOUT,'(A)') 'HENRY_SP'
  WRITE(KLUOUT,'(A)') 'Henrys law constants factor / exponent'
  WRITE(KLUOUT,'(I4)') SIZE(HSV,1)
  WRITE(KLUOUT,'(A)') YOUTFORMAT
!  
  XSREALHENRYVAL(:,1) = 1E-8 ! no deposition; low Henry constant
  XSREALHENRYVAL(:,2) = 0. ! 
  DO JNREAL = 1, SIZE(HSV,1)
    INACT = 0
    search_loop5 : DO JN = 1, IHENRY
      IF (HSV(JNREAL) .EQ. YHENRYNAME(JN)) THEN
        INACT = JN
        EXIT search_loop5
      END IF
    END DO search_loop5
    IF (INACT .NE. 0) XSREALHENRYVAL(JNREAL,1) = ZHENRYVAL(INACT,1)
    IF (INACT .NE. 0) XSREALHENRYVAL(JNREAL,2) = ZHENRYVAL(INACT,2)
    WRITE(KLUOUT,YOUTFORMAT) HSV(JNREAL), &
                      XSREALHENRYVAL(JNREAL,1),&
                      XSREALHENRYVAL(JNREAL,2)  
  END DO

 CALL CLOSE_NAMELIST(HPROGRAM,ICH)

IF (LHOOK) CALL DR_HOOK('CH_INIT_DEPCONST',1,ZHOOK_HANDLE)
!
END SUBROUTINE CH_INIT_DEPCONST
