!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE READ_SURF_ATM     (HPROGRAM, PFIELD, KFORC_STEP, KNB, KINIT)  
!**************************************************************************
!
!!    PURPOSE
!!    -------
!         Read in the ascii file the atmospheric forcing for the actual time
!         step KFORC_STEP, and for the next one.
!         The two time step are needed for the time interpolation of the
!         forcing.
!         If the end of the file  is reached, set the two step to the last
!         values.
!         Return undef value if the variable is not present
!!
!!**  METHOD
!!    ------
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
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     03/2008
!         
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NINDEX, XTIME_COMM_READ, XTIME_NPIO_READ
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_IO_SURF_OL, ONLY : XSTART,XCOUNT,XSTRIDE,LPARTR
USE MODD_IO_SURF_ASC,ONLY : NNI_FORC
!
USE MODD_ARCH, ONLY : LITTLE_ENDIAN_ARCH
!
USE MODE_CHAR2REAL
!
USE MODI_ABOR1_SFX
USE MODI_READ_AND_SEND_MPI
USE MODI_GATHER_AND_WRITE_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
! global variables
REAL, DIMENSION(:,:),INTENT(INOUT) :: PFIELD
INTEGER,INTENT(IN)               :: KFORC_STEP
INTEGER,INTENT(IN)               :: KNB 
INTEGER,INTENT(IN)               :: KINIT
 CHARACTER(LEN=6)    ,INTENT(IN)  :: HPROGRAM

! local variables
INTEGER                          :: I, INI, J, I1
 CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE  :: YF
 CHARACTER(LEN=4) :: YWORK
DOUBLE PRECISION :: XTIME0
REAL*4                            :: ZWORK4
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFIELD
REAL                              :: ZWORK
LOGICAL                          :: GSWAP              ! T: swap has been done
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM',0,ZHOOK_HANDLE)
!
IF (NRANK==NPIO) THEN
  INI = SIZE(NINDEX)
  ALLOCATE(ZFIELD(INI,SIZE(PFIELD,2)))
  IF (HPROGRAM == 'BINARY') THEN
    ALLOCATE(YF(INI))
  ENDIF
ELSE
  ALLOCATE(ZFIELD(0,0)) 
  ALLOCATE(YF(0))
ENDIF
!
 CALL GATHER_AND_WRITE_MPI(PFIELD,ZFIELD)
!
IF (NRANK==NPIO) THEN
  !
#ifdef SFX_MPI
  XTIME0 = MPI_WTIME()
#endif
  !
  IF (HPROGRAM == 'ASCII ') THEN
    !
    IF (KFORC_STEP .EQ. 1) THEN
      I1 = 1
      REWIND(KINIT)
    ELSE
      I1 = 2
      ZFIELD(:,1) = ZFIELD(:,KNB)
    ENDIF
    DO I=I1,KNB
      IF (NNI_FORC==1) THEN
        READ(UNIT=KINIT,FMT=*) ZWORK
        ZFIELD(:,I) = ZWORK
      ELSE
        READ(UNIT=KINIT,FMT=*) ZFIELD(:,I)
      END IF
    ENDDO
    !
  ELSE IF (HPROGRAM == 'BINARY') THEN
    !
    IF (KFORC_STEP .EQ. 1) THEN    
      I1 = 1
      GSWAP = .FALSE.
    ELSE
      I1 = 2
      ZFIELD(:,1) = ZFIELD(:,KNB)
    ENDIF
    DO I=I1,KNB
      IF (NNI_FORC==1) THEN
        READ(UNIT=KINIT,REC=KFORC_STEP+I-1) YWORK
        YF(:) = YWORK
      ELSE
        READ(UNIT=KINIT,REC=KFORC_STEP+I-1) YF(:)
      END IF
      ZFIELD(:,I) = YF(:)
      IF (     ANY(ABS(ZFIELD(:,I))>0. .AND. ABS(ZFIELD(:,I))<1.E-30) &
          .OR. ANY(ABS(ZFIELD(:,I))>1.E6)                       ) THEN  
        CALL ABOR1_SFX('READ_SURF_ATM: SWAP SET IN YOUR PARAMS_CONFIG FILE SEEMS '//&
          'INAPPROPRIATE - VERIFY  ')  
      END IF  
    ENDDO
    !
  ENDIF
  !
#ifdef SFX_MPI
  XTIME_NPIO_READ = XTIME_NPIO_READ + (MPI_WTIME() - XTIME0)
#endif
  !  
ENDIF
!
 CALL READ_AND_SEND_MPI(ZFIELD,PFIELD)
!
DEALLOCATE(ZFIELD)
IF (HPROGRAM=='BINARY') THEN
  DEALLOCATE(YF)
ENDIF
!
LPARTR=.FALSE.
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ATM',1,ZHOOK_HANDLE)

END SUBROUTINE READ_SURF_ATM
