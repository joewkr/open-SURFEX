!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SOILTEMP_ARP_PAR (IO, HPROGRAM)
!     ##############################################################
!
!!**** *SOILTEMP_ARP_PAR* Impose special pseudo depth for "force-restore"
!!                        multilayer deep temperature
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2009
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
!
USE MODN_SOILTEMP_ARP
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
!

 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!

IF (LHOOK) CALL DR_HOOK('SOILTEMP_ARP_PAR',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN

 SODELX(:)      = XUNDEF
 LTEMP_ARP      = .FALSE.
 NTEMPLAYER_ARP = 4
 !
 !-------------------------------------------------------------------------------
 !
 !*    2.      Input value for SODELX variable
 !             -------------------------------
 !
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
 !
 CALL POSNAM(ILUNAM,'NAM_SOILTEMP_ARP',GFOUND,ILUOUT)
 IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_SOILTEMP_ARP)
 !
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    3.      Consistency
!             -----------
!
IF(LTEMP_ARP.AND.IO%CISBA=='DIF')THEN
   LTEMP_ARP=.FALSE.
   WRITE(ILUOUT,*)'LTEMP_ARP put at False because you use the ISBA-DF scheme'
ENDIF
!
IF(LTEMP_ARP)THEN
  IF(NTEMPLAYER_ARP>NMAX_LAYER)THEN     
    WRITE(ILUOUT,*)'NTEMPLAYER_ARP is too big (>10), NTEMPLAYER_ARP= ',NTEMPLAYER_ARP
    CALL ABOR1_SFX('NTEMPLAYER_ARP is too big (>10)')
  ELSEIF(NTEMPLAYER_ARP<4)THEN
    WRITE(ILUOUT,*)'NTEMPLAYER_ARP must be at least equal to 4, NTEMPLAYER_ARP= ',NTEMPLAYER_ARP
    CALL ABOR1_SFX('NTEMPLAYER_ARP must be at least equal to 4')
  ENDIF
  IF(COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF)>0.AND. &
       COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF)/=NTEMPLAYER_ARP)THEN  
    WRITE(ILUOUT,*)'Number of SODELX imposed values =',COUNT(SODELX(1:NTEMPLAYER_ARP)/=XUNDEF),&
                     ' /= NTEMPLAYER_ARP= ',NTEMPLAYER_ARP  
    CALL ABOR1_SFX('SODELX imposed values /= NTEMPLAYER_ARP')
  ENDIF          
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4.      Initialization
!             -------------------------------
!
IF(LTEMP_ARP)THEN
!
  ALLOCATE(IO%XSODELX(NTEMPLAYER_ARP))
!
  IF(ALL(SODELX(:)==XUNDEF))THEN
!          
    IO%XSODELX(1)=0.5
    IO%XSODELX(2)=1.5
    IO%XSODELX(3)=4.5
    IO%XSODELX(4)=13.5
    WRITE(ILUOUT,*)'SODELX default values : ',IO%XSODELX(:)
!    
  ELSE
!          
    IO%XSODELX(:)=SODELX(1:NTEMPLAYER_ARP)
    WRITE(ILUOUT,*)'SODELX imposed to : ',IO%XSODELX(:)
!    
  ENDIF
!
ELSE
!
  ALLOCATE(IO%XSODELX(0))
!
ENDIF
!
IO%LTEMP_ARP     =LTEMP_ARP
IO%NTEMPLAYER_ARP=NTEMPLAYER_ARP
IF (LHOOK) CALL DR_HOOK('SOILTEMP_ARP_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOILTEMP_ARP_PAR
