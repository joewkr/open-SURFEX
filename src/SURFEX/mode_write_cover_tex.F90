!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################
      MODULE MODE_WRITE_COVER_TEX
!     ##########################
!
!-------------------------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
FUNCTION NB (PX,KMAX)  RESULT (KNB)
!
IMPLICIT NONE
!
REAL,    INTENT(IN)            :: PX   ! real
INTEGER, INTENT(IN), OPTIONAL  :: KMAX
INTEGER                        :: KNB  !
!
INTEGER :: IMAX     ! maximum number of decimals
INTEGER :: IX
INTEGER :: JK
INTEGER :: IDEC,IINT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:NB',0,ZHOOK_HANDLE)
IMAX=2
IF (PRESENT(KMAX)) IMAX=KMAX
!
IINT=0
!

DO JK=0,8
  IF ( INT(PX/10**JK+1.E-7)/=0 ) IINT=IINT+1
END DO
!
DO JK=1,4
  IX=INT(PX*10**IMAX+1.E-7)
  IF (IX==0) THEN
    IMAX=IMAX+1
  ELSE
    EXIT
  END IF
END DO
IF (IX==0) IMAX=2
!
IDEC=IMAX
!
DO JK=1,IMAX
  IF ( IX == NINT(IX/FLOAT(10**JK))*10**JK ) THEN
    IDEC=IDEC-1
  END IF
END DO
!
KNB=MAX(IINT,1)+IDEC+1
KNB=KNB+1
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:NB',1,ZHOOK_HANDLE)
!
END FUNCTION NB
!
!-------------------------------------------------------------------------------
!
FUNCTION NBT (PX,KMAX)  RESULT (KNBT)
!
IMPLICIT NONE
!
REAL,    INTENT(IN)            :: PX   ! real
INTEGER, INTENT(IN), OPTIONAL  :: KMAX
INTEGER                        :: KNBT !
!
INTEGER :: IMAX     ! maximum number of decimals
INTEGER :: IX
INTEGER :: JK
INTEGER :: IDEC,IINT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:NBT',0,ZHOOK_HANDLE)
IMAX=2
IF (PRESENT(KMAX)) IMAX=KMAX
!
IINT=0
!
DO JK=0,8
  IF ( INT(PX/10.**JK+1.E-7)/=0 ) IINT=IINT+1
END DO
!
DO JK=1,4
  IX=INT(PX*10**IMAX+1.E-7)
  IF (IX==0) THEN
    IMAX=IMAX+1
  ELSE
    EXIT
  END IF
END DO
IF (IX==0) IMAX=2
!
IDEC=IMAX
!
DO JK=1,IMAX
  IF ( IX == NINT(IX/FLOAT(10**JK))*10**JK ) THEN
    IDEC=IDEC-1
  END IF
END DO
!
KNBT=MAX(IINT+IDEC+1,2)
KNBT=KNBT+1
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:NBT',1,ZHOOK_HANDLE)
!
END FUNCTION NBT
!
!-------------------------------------------------------------------------------
!
FUNCTION DEC (PX,KMAX) RESULT (KDEC)
!
IMPLICIT NONE
!
REAL,    INTENT(IN)            :: PX   ! real
INTEGER, INTENT(IN), OPTIONAL  :: KMAX
INTEGER                        :: KDEC ! number of decimals of PX
!
INTEGER :: IMAX     ! maximum number of decimals
INTEGER :: IX
INTEGER :: JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:DEC',0,ZHOOK_HANDLE)
IMAX=2
IF (PRESENT(KMAX)) IMAX=KMAX
!
!
DO JK=1,4
  IX=INT(PX*10**IMAX+1.E-7)
  IF (IX==0) THEN
    IMAX=IMAX+1
  ELSE
    EXIT
  END IF
END DO
IF (IX==0) IMAX=2
!
KDEC=IMAX
!
DO JK=1,IMAX
  IF ( IX == NINT(IX/FLOAT(10**JK))*10**JK ) THEN
    KDEC=KDEC-1
  END IF
END DO
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:DEC',1,ZHOOK_HANDLE)
!
!
END FUNCTION DEC
!-------------------------------------------------------------------------------
SUBROUTINE HLINE(KTEX,GLINE,I)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)    :: KTEX  ! logical unit
LOGICAL, INTENT(INOUT) :: GLINE ! flag to write one line
INTEGER, INTENT(IN)    :: I     ! line number
REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:HLINE',0,ZHOOK_HANDLE)
  IF ((I==3 .OR. I==  7 .OR. I==  9 .OR. I==15  .OR. I== 23 .OR. I== 24 &
              .OR. I== 28 .OR. I== 33 .OR. I==47  .OR. I== 66 .OR. I== 79 &
              .OR. I== 87 .OR. I==104 .OR. I==122 .OR. I==123 .OR. I==125 &
              .OR. I==161 .OR. I==173 .OR. I==176 .OR. I==181 .OR. I==186 &
              .OR. I==191 .OR. I==197 .OR. I==198 .OR. I==207 .OR. I==214 &
              .OR. I==219 .OR. I==225 .OR. I==229 .OR. I==232 .OR. I==235 &
              .OR. I==241 .OR. I==243                                    )&
             .AND. GLINE ) THEN  
    WRITE(KTEX,*) '\hline'
    GLINE=.FALSE.
  END IF
IF (LHOOK) CALL DR_HOOK('MODE_WRITE_COVER_TEX:HLINE',1,ZHOOK_HANDLE)
END SUBROUTINE HLINE
!-------------------------------------------------------------------------------
END MODULE MODE_WRITE_COVER_TEX
