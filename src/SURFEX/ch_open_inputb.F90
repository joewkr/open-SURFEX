!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_OPEN_INPUTB(HKEYWORD,KCHANNEL,KLUOUT)
!!    #########################################################################
!!
!!*** *CH_OPEN_INPUTB*
!!
!!    PURPOSE
!!    -------
!       Open the general purpose chemical input file and position the
!     file pointer after the indicated keyword.
!!
!!**  METHOD
!!    ------
!!      An unused input channel is selected using OPEN_ll.
!!    The file will be rewinded
!!    at each call and data will be read in using (A8)-format until the
!!    given keyword is found. The following comment line will then
!!    be read and printed and the input channel number will be returned.
!!    After reading the needed data, the user must assure that the file
!!    will be closed and that the unit will be freed using CLOSE_ll.
!!
!!    REFERENCE
!!    ---------
!!    MesoNH book 2
!!
!!    AUTHOR
!!    ------
!!    K. Suhre   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 03/11/95
!!    05/08/96 (K. Suhre) restructured
!!    11/08/98 (N. Asencio) add parallel code
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    none
!!
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN) :: HKEYWORD         ! keyword for positioning
INTEGER         , INTENT(IN) :: KCHANNEL         ! I/O channel to choose
INTEGER,          INTENT(IN) :: KLUOUT           ! output listing logical unit
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=79) :: YIN ! character string for line-by-line read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    READ INPUT DATA UNTIL KEYWORD IS FOUND
!              --------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CH_OPEN_INPUTB',0,ZHOOK_HANDLE)
REWIND(KCHANNEL)
!
! open the input file
!
! read general comment line and print it
READ(KCHANNEL,"(A)") YIN
WRITE(KLUOUT, *) YIN
!
search_key : DO
  READ(KCHANNEL,"(A8)", END=100) YIN
  IF (HKEYWORD(1:8) .EQ. YIN(1:8)) EXIT search_key
ENDDO search_key
!
! read specific comment line and print it
WRITE(KLUOUT,*) &
     "Keyword ", HKEYWORD(1:8), " has been found in chemistry data file, the specific comment is:"  
READ(KCHANNEL,"(A)") YIN
WRITE(KLUOUT, *) YIN
!
IF (LHOOK) CALL DR_HOOK('CH_OPEN_INPUTB',1,ZHOOK_HANDLE)
RETURN
!
!-------------------------------------------------------------------------------
!
!*       2.    IF THE KEYWORD HAS NOT BEEN FOUND, ABORT
!              ---------------------------------------
!
100 CONTINUE
WRITE(KLUOUT,*) "CH_OPEN_INPUTB-Error: Keyword ", HKEYWORD(1:8), " not found."
 CALL ABOR1_SFX('CH_OPEN_INPUTB: KEYWORD '//HKEYWORD(1:8)//' NOT FOUND')
IF (LHOOK) CALL DR_HOOK('CH_OPEN_INPUTB',1,ZHOOK_HANDLE)
!
END SUBROUTINE CH_OPEN_INPUTB
