!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
FUNCTION BLDCODE (BDD, KTYPE,KAGE) RESULT(KCODE)
!
!
!
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
!
IMPLICIT NONE
!
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
!
INTEGER, DIMENSION(:), INTENT(IN) :: KTYPE ! Type of building
INTEGER, DIMENSION(:), INTENT(IN) :: KAGE  ! date of construction (or total renovation) of building
INTEGER, DIMENSION(SIZE(KTYPE))   :: KCODE ! Building code (merges type & age info).
!
INTEGER :: JL        ! loop counter on points
INTEGER :: JAGE      ! loop counter on construction date ranges
INTEGER :: ICODE_AGE ! code for the adequate construction date range
!
DO JL=1,SIZE(KTYPE)
  ICODE_AGE=BDD%NDESC_AGE_LIST(BDD%NDESC_AGE) ! default value is the more recent building
  DO JAGE=BDD%NDESC_AGE,1,-1
    IF (BDD%NDESC_AGE_DATE(JAGE)>=KAGE(JL)) ICODE_AGE = BDD%NDESC_AGE_LIST(JAGE)
  END DO
  KCODE(JL) = 100*KTYPE(JL)+ICODE_AGE
END DO
!
END FUNCTION BLDCODE
