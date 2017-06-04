!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_BUFFER_GRID(KLUOUT,HINMODEL,TPTIME_BUF)
!     ##########################################################################
!
!!****  *PREP_BUFFER_GRID* - reads BUFFER infos
!!
!!    PURPOSE
!!    -------
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
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   03/2005
!!      Y. Seity   08/2006 :  for NEC Prepsurfex configuration (pbs LEN /=6)
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_READ_BUFFER
!
USE MODD_GRID_BUFFER,  ONLY : NNI
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
INTEGER,          INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6), INTENT(OUT)   :: HINMODEL  ! originating model
TYPE (DATE_TIME)                :: TPTIME_BUF    ! current date and time

!
!* 0.2 Declaration of local variables
!      ------------------------------
INTEGER                           :: IRET      ! return code
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_BUFFER_GRID',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' -- Buffer reader started'
!
! 
!---------------------------------------------------------------------------------------
!* 1.  Read HINMODEL
!---------------------------------------------------------------------------------------
!
 CALL READ_BUFFER('INMODE',HINMODEL,IRET)

    IF (HINMODEL =='ALADIN' ) THEN
      WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Aladin model'
   ELSE
      CALL ABOR1_SFX('PREP_BUFFER_GRID: UNSUPPORTED GRIB FILE FORMAT')
   END IF
!
!---------------------------------------------------------------------------------------
!* 3. Number of points
!---------------------------------------------------------------------------------------
!
 CALL READ_BUFFER('NNI   ',NNI,IRET)
WRITE (KLUOUT,*)'apres READ BUFFER NNI=',NNI
!---------------------------------------------------------------------------------------
!* 2.4 Read date
!---------------------------------------------------------------------------------------
!
WRITE (KLUOUT,'(A)') ' | Reading date'
!
 CALL READ_BUFFER('YEAR  ',TPTIME_BUF%TDATE%YEAR,IRET )
 CALL READ_BUFFER('MONTH ',TPTIME_BUF%TDATE%MONTH,IRET )
 CALL READ_BUFFER('DAY   ',TPTIME_BUF%TDATE%DAY,IRET )
 CALL READ_BUFFER('TIME  ', TPTIME_BUF%TIME,IRET)
IF (LHOOK) CALL DR_HOOK('PREP_BUFFER_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_BUFFER_GRID
