!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_FASURFEX
!
  INTERFACE FAECR
     MODULE PROCEDURE FAECR_I
     MODULE PROCEDURE FAECR_R
     MODULE PROCEDURE FAECR_L
     MODULE PROCEDURE FAECR_C
     MODULE PROCEDURE FAECR_I_D
     MODULE PROCEDURE FAECR_R_D
     MODULE PROCEDURE FAECR_L_D
  END INTERFACE
!  
  INTERFACE FALIT
     MODULE PROCEDURE FALIT_I
     MODULE PROCEDURE FALIT_R
     MODULE PROCEDURE FALIT_L
     MODULE PROCEDURE FALIT_C
     MODULE PROCEDURE FALIT_I_D
     MODULE PROCEDURE FALIT_R_D
     MODULE PROCEDURE FALIT_L_D
  END INTERFACE
!
CONTAINS
!
! #############################################################
  SUBROUTINE FAECR_I(KREP,KN,CNOMC,KDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KDATA
!
    REAL(KIND=8), DIMENSION(1)        :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_I',0,ZHOOK_HANDLE)
!    
    ZDONNE(1)=REAL(KDATA,8)
    IL=SIZE(ZDONNE)
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,IL)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_I',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_I
!
! #############################################################
  SUBROUTINE FAECR_I_D(KREP,KN,CNOMC,KSIZE,KDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    INTEGER,DIMENSION(KSIZE),INTENT(IN):: KDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)      :: ZDONNE
    INTEGER                            :: I
    REAL(KIND=JPRB)                    :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_I_D',0,ZHOOK_HANDLE)
!    
    DO I=1,KSIZE
       ZDONNE(I)=REAL(KDATA(I),8)
    END DO
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,KSIZE)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_I_D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_I_D

! #############################################################
  SUBROUTINE FAECR_R(KREP,KN,CNOMC,PDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    REAL,INTENT(IN)                   :: PDATA
!
    REAL(KIND=8), DIMENSION(1)        :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_R',0,ZHOOK_HANDLE)
!    
    ZDONNE(1)=REAL(PDATA,8)
    IL=SIZE(ZDONNE)
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,IL)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_R',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_R
!
! #############################################################
  SUBROUTINE FAECR_R_D(KREP,KN,CNOMC,KSIZE,PDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    REAL,DIMENSION(KSIZE),INTENT(IN)  :: PDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_R_D',0,ZHOOK_HANDLE)
!    
    DO I=1,KSIZE
       ZDONNE(I)=REAL(PDATA(I),8)
    END DO
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,KSIZE)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_R_D',1,ZHOOK_HANDLE)   
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_R_D
!
! #############################################################
  SUBROUTINE FAECR_L(KREP,KN,CNOMC,LDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    LOGICAL,INTENT(IN)                :: LDATA
!
    REAL(KIND=8),DIMENSION(1)         :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_L',0,ZHOOK_HANDLE)
!    
    IF (LDATA) THEN 
      ZDONNE(1)=1.
    ELSE
      ZDONNE(1)=0.
    ENDIF
    IL=SIZE(ZDONNE)
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,IL)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_L',1,ZHOOK_HANDLE)   
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_L
!
! #############################################################
  SUBROUTINE FAECR_L_D(KREP,KN,CNOMC,KSIZE,LDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    LOGICAL,DIMENSION(KSIZE),INTENT(IN) :: LDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_L_D',0,ZHOOK_HANDLE)
!    
    DO I=1,KSIZE
       IF (LDATA(I)) THEN 
          ZDONNE(I)=1.
       ELSE
          ZDONNE(I)=0.
       ENDIF
    END DO
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,KSIZE)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_L_D',1,ZHOOK_HANDLE)   
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_L_D
!
! #############################################################
  SUBROUTINE FAECR_C(KREP,KN,CNOMC,KSIZE,CDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    CHARACTER,DIMENSION(KSIZE),INTENT(IN) :: CDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_C',0,ZHOOK_HANDLE)
!    
    DO I=1,KSIZE
       ZDONNE(I)=REAL(ICHAR(CDATA(I)),8)
    END DO
    CALL FAISAN(KREP,KN,CNOMC,ZDONNE,KSIZE)
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FAECR_C',1,ZHOOK_HANDLE)   
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FAECR_C
!
! #############################################################
  SUBROUTINE FALIT_I_D(KREP,KN,CNOMC,KSIZE,KDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    INTEGER,DIMENSION(KSIZE),INTENT(OUT) :: KDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_I_D',0,ZHOOK_HANDLE)
!    
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,KSIZE)
    DO I=1,KSIZE
       KDATA(I)=ANINT(ZDONNE(I),KIND(KDATA))
    END DO
!
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_I_D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_I_D 
!
! #############################################################
  SUBROUTINE FALIT_I(KREP,KN,CNOMC,KDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(OUT)               :: KDATA
!
    REAL(KIND=8), DIMENSION(1)        :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_I',0,ZHOOK_HANDLE)
!    
    IL=SIZE(ZDONNE)
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,IL)
    KDATA=ANINT(ZDONNE(1),KIND(KDATA))
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_I',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_I 
!
! #############################################################
  SUBROUTINE FALIT_R_D(KREP,KN,CNOMC,KSIZE,PDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    REAL,DIMENSION(KSIZE),INTENT(OUT) :: PDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_R_D',0,ZHOOK_HANDLE)
!    
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,KSIZE)
    DO I=1,KSIZE
       PDATA(I)=REAL(ZDONNE(I),KIND(PDATA))
    END DO
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_R_D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_R_D
!
! #############################################################
  SUBROUTINE FALIT_R(KREP,KN,CNOMC,PDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    REAL,INTENT(OUT)                  :: PDATA
!
    REAL(KIND=8), DIMENSION(1)        :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_R',0,ZHOOK_HANDLE)
!    
    IL=SIZE(ZDONNE)
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,IL)
    PDATA=REAL(ZDONNE(1),KIND(PDATA))
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_R',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_R
!
! #############################################################
  SUBROUTINE FALIT_L_D(KREP,KN,CNOMC,KSIZE,LDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    LOGICAL,DIMENSION(KSIZE),INTENT(OUT)  :: LDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_L_D',0,ZHOOK_HANDLE)
!    
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,KSIZE)
    DO I=1,KSIZE
       LDATA(I)=LOGICAL(ZDONNE(I)==1.,KIND(LDATA))
    END DO
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_L_D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_L_D 
!
! #############################################################
  SUBROUTINE FALIT_L(KREP,KN,CNOMC,LDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
!
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    LOGICAL,INTENT(OUT)               :: LDATA
!
    REAL(KIND=8), DIMENSION(1)        :: ZDONNE
    INTEGER                           :: IL
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_L',0,ZHOOK_HANDLE)
!    
    IL=SIZE(ZDONNE)
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,IL)
    LDATA=LOGICAL(ZDONNE(1)==1.,KIND(LDATA))
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_L',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
  END SUBROUTINE FALIT_L 
!
! #############################################################
  SUBROUTINE FALIT_C(KREP,KN,CNOMC,KSIZE,CDATA)
! #############################################################
!
    USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
    USE PARKIND1  ,ONLY : JPRB
!
    IMPLICIT NONE
!
    INTEGER,INTENT(INOUT)             :: KREP
    INTEGER,INTENT(IN)                :: KN
    CHARACTER(LEN=18),INTENT(IN)      :: CNOMC
    INTEGER,INTENT(IN)                :: KSIZE
    CHARACTER,DIMENSION(KSIZE),INTENT(OUT)  :: CDATA
!
    REAL(KIND=8),DIMENSION(KSIZE)     :: ZDONNE
    INTEGER                           :: I,J
    REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_C',0,ZHOOK_HANDLE)
!    
    CALL FALAIS(KREP,KN,CNOMC,ZDONNE,KSIZE)
    DO I=1,KSIZE
       J=ANINT(ZDONNE(I))
       CDATA(I)=CHAR(J)
    END DO
!    
    IF (LHOOK) CALL DR_HOOK('MODE_FASURFEX:FALIT_C',1,ZHOOK_HANDLE)    
!-------------------------------------------------------------------------------
!    
  END SUBROUTINE FALIT_C
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END MODULE MODE_FASURFEX


