!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_PGD_FIELD
!
INTERFACE PGD_FIELD
!
!     #########
      SUBROUTINE PGD_FIELD_1D (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,OPRESENT,PVEGTYPE)
!     ##############################################################
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(INOUT) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PVEGTYPE
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELD_1D
!
!     #########
      SUBROUTINE PGD_FIELD_2D (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,OPRESENT,PVEGTYPE)
!     ##############################################################
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(INOUT) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:,:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PVEGTYPE
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELD_2D
!
END INTERFACE PGD_FIELD
!
END MODULE MODI_PGD_FIELD
!
!
!     #########
      SUBROUTINE PGD_FIELD_1D (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,OPRESENT,PVEGTYPE)
!     ##############################################################
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_PGD_FIELDIN
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(INOUT) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PVEGTYPE
!
LOGICAL :: GPRESENT
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZMASK
REAL, DIMENSION(SIZE(PFIELD),1) :: ZFIELD
!
IF (PRESENT(PVEGTYPE)) THEN
  ALLOCATE(ZMASK(SIZE(PVEGTYPE),1))
  ZMASK(:,1) = PVEGTYPE
  CALL PGD_FIELDIN(DTCO, UG, U, USS, &
                   HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,ZFIELD,GPRESENT,ZMASK)
  DEALLOCATE(ZMASK)
ELSE
  CALL PGD_FIELDIN(DTCO, UG, U, USS, &
                  HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,ZFIELD,GPRESENT)
ENDIF
!
PFIELD(:) = ZFIELD(:,1)
!
IF (PRESENT(OPRESENT)) OPRESENT = GPRESENT
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELD_1D
!
!     #########
      SUBROUTINE PGD_FIELD_2D (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,OPRESENT,PVEGTYPE)
!     ##############################################################
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODI_PGD_FIELDIN
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(INOUT) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:,:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PVEGTYPE
!
LOGICAL :: GPRESENT
!
IF (PRESENT(PVEGTYPE)) THEN
  CALL PGD_FIELDIN(DTCO, UG, U, USS, &
                   HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,GPRESENT,PVEGTYPE)
ELSE
  CALL PGD_FIELDIN(DTCO, UG, U, USS, &
                  HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,GPRESENT)
ENDIF
!
IF (PRESENT(OPRESENT)) OPRESENT = GPRESENT
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELD_2D
!
