!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      MODULE MODI_READ_BUFFER
!     #######################
INTERFACE READ_BUFFER
!
      SUBROUTINE READ_BUFX1(HNAME,PFIELD,KRET)
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
REAL, DIMENSION(:),           INTENT(OUT)  :: PFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
END SUBROUTINE READ_BUFX1
!
      SUBROUTINE READ_BUFX0(HNAME,PFIELD,KRET)
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
REAL,                         INTENT(OUT)  :: PFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
END SUBROUTINE READ_BUFX0
!
      SUBROUTINE READ_BUFN0(HNAME,KFIELD,KRET)
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
INTEGER,                      INTENT(OUT)  :: KFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
END SUBROUTINE READ_BUFN0
!
      SUBROUTINE READ_BUFN1(HNAME,KFIELD,KRET)
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
INTEGER, DIMENSION(:),        INTENT(OUT)  :: KFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
END SUBROUTINE READ_BUFN1
!
      SUBROUTINE READ_BUFC0(HNAME,HFIELD,KRET)
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME     ! name of field
 CHARACTER(LEN=*),             INTENT(OUT)  :: HFIELD    ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
END SUBROUTINE READ_BUFC0
!
END INTERFACE
END MODULE MODI_READ_BUFFER
!     #######################################################
 SUBROUTINE READ_BUFC0(HNAME,HFIELD,KRET)
!     #######################################################
!
!!****  *READ_BUFC0* - routine to read a character (LEN=6) in a buffer from SURFEX
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "get_bufc0.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME     ! name of field
 CHARACTER(LEN=*),             INTENT(OUT)  :: HFIELD    ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                    :: ILUOUT ! Listing file number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFC0',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('AROME ',ILUOUT)
!
#ifdef SFX_ARO
 CALL GET_BUFC0(HNAME,HFIELD,LEN(HFIELD),KRET)
#endif
!
IF (KRET /=0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'error when de-burrering article', HNAME,' KRET=',KRET
  WRITE(ILUOUT,*) ' '
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFC0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFC0
!     #######################################################
 SUBROUTINE READ_BUFN0(HNAME,KFIELD,KRET)
!     #######################################################
!
!!****  *READ_BUFN0* - routine to read an integer in a buffer from SURFEX
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "get_bufn0.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
INTEGER,                      INTENT(OUT)  :: KFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                    :: ILUOUT ! Listing file number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFN0',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('AROME ',ILUOUT)
!
#ifdef SFX_ARO
 CALL GET_BUFN0(HNAME,KFIELD,KRET)
#endif
!
IF (KRET /=0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'error when de-burrering article', HNAME,' KRET=',KRET
  WRITE(ILUOUT,*) ' '
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFN0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFN0
!     #######################################################
 SUBROUTINE READ_BUFN1(HNAME,KFIELD,KRET)
!     #######################################################
!
!!****  *READ_BUFN1* - routine to read a 1D integer array in a buffer from SURFEX
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "get_bufn1.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
INTEGER, DIMENSION(:),        INTENT(OUT)  :: KFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                    :: ILUOUT ! Listing file number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFN1',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('AROME ',ILUOUT)
!
#ifdef SFX_ARO
 CALL GET_BUFN1(HNAME,SIZE(KFIELD),KFIELD,KRET)
#endif
!
IF (KRET /=0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'error when de-burrering article', HNAME,' KRET=',KRET
  WRITE(ILUOUT,*) ' '
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFN1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFN1
!     #######################################################
 SUBROUTINE READ_BUFX0(HNAME,PFIELD,KRET)
!     #######################################################
!
!!****  *READ_BUFX0* - routine to read a real in a buffer from SURFEX
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "get_bufx0.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
REAL,                         INTENT(OUT)  :: PFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                    :: ILUOUT ! Listing file number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFX0',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('AROME ',ILUOUT)
!
#ifdef SFX_ARO
 CALL GET_BUFX0(HNAME,PFIELD,KRET)
#endif
!
IF (KRET /=0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'error when de-burrering article', HNAME,' KRET=',KRET
  WRITE(ILUOUT,*) ' '
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFX0',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFX0
!     #######################################################
 SUBROUTINE READ_BUFX1(HNAME,PFIELD,KRET)
!     #######################################################
!
!!****  *READ_BUFX1* - routine to read a 1D real array in a buffer from SURFEX
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
!!      S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_ARO
#include "get_bufx1.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),             INTENT(IN)   :: HNAME        ! name of field
REAL, DIMENSION(:),           INTENT(OUT)  :: PFIELD ! array containing the data field
INTEGER,                      INTENT(OUT)  :: KRET      !  error code
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                    :: ILUOUT ! Listing file number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFX1',0,ZHOOK_HANDLE)
 CALL GET_LUOUT('AROME ',ILUOUT)
!
#ifdef SFX_ARO
 CALL GET_BUFX1(HNAME,SIZE(PFIELD),PFIELD,KRET)
#endif
!
IF (KRET /=0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) '-------'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) 'error when de-burrering article', HNAME,' KRET=',KRET
  WRITE(ILUOUT,*) ' '
ENDIF
IF (LHOOK) CALL DR_HOOK('MODI_READ_BUFFER:READ_BUFX1',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_BUFX1
