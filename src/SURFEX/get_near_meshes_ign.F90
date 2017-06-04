!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_NEAR_MESHES_IGN(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)
!     ##############################################################
!
!!**** *GET_NEAR_MESHES_IGN* get the near grid mesh indices
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!
!     Modifié par Renaud Lestrigant (02/2016) : changement complet de l'algo
!     de recherche des plus proches voisins.
!     Récupération d'un code sur Internet et adaptation locale.
!     (http://jblevins.org/mirror/amiller/qsort.f90)
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI, ONLY : NINDEX, NRANK, NNUM
USE MODE_GRIDTYPE_IGN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
INTEGER,                         INTENT(IN)    :: KNEAR_NBR ! number of nearest points wanted
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(:,:),POINTER :: KNEAR    ! near mesh indices
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL,DIMENSION(KL)  :: ZX
REAL,DIMENSION(KL)  :: ZY
REAL,DIMENSION(KL)  :: ZDX
REAL,DIMENSION(KL)  :: ZDY
REAL,DIMENSION(KL)  :: ZDIS
INTEGER,DIMENSION(KL) :: INDZDIS

REAL :: ZMAXVALDIS

INTEGER :: JP, ID, ISIZE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN_1',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY)
!
KNEAR(:,:) = 0
!
ISIZE = MIN(KNEAR_NBR,KL)
!
! calcul de la distance de tous les points 2 à 2
!
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN_2',0,ZHOOK_HANDLE)
!
!$OMP PARALLEL DO PRIVATE(JP,ZDIS,ZMAXVALDIS,INDZDIS,ID)
!
DO JP=1,KL
  !
  IF (NINDEX(JP)==NRANK) THEN 
    !
    ID = NNUM(JP)
    !
    ! distance du point JP à tous les autres points
    ZDIS(:) = SQRT((ZX(:)-ZX(JP))**2 + (ZY(:)-ZY(JP))**2)
    ! distance maximale entre JP et les autres moints
    ZMAXVALDIS = 2. * MAXVAL(ZDIS)
    ZDIS(JP) = ZMAXVALDIS

    CALL QUICK_SORT(ZDIS, INDZDIS)
    KNEAR(ID,1:ISIZE) = INDZDIS(1:ISIZE)
    !
  ENDIF
  !
ENDDO
!$OMP END PARALLEL DO
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN_2',1,ZHOOK_HANDLE)
!
CONTAINS
!
RECURSIVE SUBROUTINE QUICK_SORT(PLIST, KORDER)

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.
!
IMPLICIT NONE
!
REAL, DIMENSION (:), INTENT(INOUT)  :: PLIST
INTEGER, DIMENSION (:), INTENT(OUT)  :: KORDER
!
! Local variable
INTEGER :: JI

DO JI = 1, SIZE(PLIST)
  KORDER(JI) = JI
END DO

CALL QUICK_SORT_1(1, SIZE(PLIST), PLIST, KORDER)

END SUBROUTINE QUICK_SORT


RECURSIVE SUBROUTINE QUICK_SORT_1(KLEFT_END, KRIGHT_END, PLIST1, KORDER1)

INTEGER, INTENT(IN) :: KLEFT_END, KRIGHT_END
REAL, DIMENSION (:), INTENT(INOUT)  :: PLIST1
INTEGER, DIMENSION (:), INTENT(INOUT)  :: KORDER1
!     Local variables
INTEGER             :: JI, JJ, ITEMP
REAL                :: ZREF, ZTEMP
INTEGER, PARAMETER  :: IMAX_SIMPLE_SORT_SIZE = 6

IF (KRIGHT_END < KLEFT_END + IMAX_SIMPLE_SORT_SIZE) THEN
  ! Use interchange sort for small PLISTs
  CALL INTERCHANGE_SORT(KLEFT_END, KRIGHT_END, PLIST1, KORDER1)
  !
ELSE
  !
  ! Use partition ("quick") sort
  ! valeur au centre du tableau
  ZREF = PLIST1((KLEFT_END + KRIGHT_END)/2)
  JI = KLEFT_END - 1
  JJ = KRIGHT_END + 1

  DO
    ! Scan PLIST from left end until element >= ZREF is found
    DO
      JI = JI + 1
      IF (PLIST1(JI) >= ZREF) EXIT
    END DO
    ! Scan PLIST from right end until element <= ZREF is found
    DO
      JJ = JJ - 1
      IF (PLIST1(JJ) <= ZREF) EXIT
    END DO


    IF (JI < JJ) THEN
      ! Swap two out-of-order elements
      ZTEMP = PLIST1(JI)
      PLIST1(JI) = PLIST1(JJ)
      PLIST1(JJ) = ZTEMP
      ITEMP = KORDER1(JI)
      KORDER1(JI) = KORDER1(JJ)
      KORDER1(JJ) = ITEMP
    ELSE IF (JI == JJ) THEN
      JI = JI + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (KLEFT_END < JJ) CALL QUICK_SORT_1(KLEFT_END, JJ, PLIST1, KORDER1)
  IF (JI < KRIGHT_END) CALL QUICK_SORT_1(JI, KRIGHT_END,PLIST1,KORDER1)
END IF

END SUBROUTINE QUICK_SORT_1


SUBROUTINE INTERCHANGE_SORT(KLEFT_END, KRIGHT_END, PLIST2, KORDER2)

INTEGER, INTENT(IN) :: KLEFT_END, KRIGHT_END
REAL, DIMENSION (:), INTENT(INOUT)  :: PLIST2
INTEGER, DIMENSION (:), INTENT(INOUT)  :: KORDER2
!     Local variables
INTEGER             :: JI, JJ, ITEMP
REAL                :: ZTEMP

! boucle sur tous les points
DO JI = KLEFT_END, KRIGHT_END - 1
  !
  ! boucle sur les points suivants le point JI
  DO JJ = JI+1, KRIGHT_END
    !
    ! si la distance de JI au point est plus grande que celle de JJ
    IF (PLIST2(JI) > PLIST2(JJ)) THEN
      ! distance de JI au point (la plus grande)
      ZTEMP = PLIST2(JI)
      ! le point JJ est déplacé à l'indice JI dans le tableau 
      PLIST2(JI) = PLIST2(JJ)
      ! le point JI est déplacé à l'indice JJ dans le tableau
      PLIST2(JJ) = ZTEMP
      ! indice du point JI dans le tableau
      ITEMP = KORDER2(JI)
      ! l'indice du point JJ est mis à la place JI
      KORDER2(JI) = KORDER2(JJ)
      ! l'indice du point JI est mis à la place JJ
      KORDER2(JJ) = ITEMP
    END IF
    !
  END DO
  !
END DO

END SUBROUTINE INTERCHANGE_SORT
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_NEAR_MESHES_IGN
