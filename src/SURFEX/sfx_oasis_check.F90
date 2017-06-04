!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_CHECK (IO, U, KLUOUT)
!###################################################
!
!!****  *SFX_OASIS_CHECK* - Definitions for exchange of coupling fields
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!    10/2016 B. Decharme : bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODN_SFX_OASIS, ONLY : CCALVING, LWATER
USE MODD_SFX_OASIS, ONLY : LCPL_LAKE, LCPL_CALVING, LCPL_GW
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
INTEGER, INTENT(IN) :: KLUOUT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_CHECK',0,ZHOOK_HANDLE)
!
IF(LCPL_LAKE)THEN
  IF(U%CWATER/='FLAKE ')THEN
    WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    WRITE(KLUOUT,*)'!!!    SFX - LAKE coupling is asked   !!!'
    WRITE(KLUOUT,*)'!!!     but CWATER /= FLAKE           !!!'
    WRITE(KLUOUT,*)'!!!                                   !!!'
    WRITE(KLUOUT,*)'!!!    Please check your namelist     !!!'
    WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    CALL ABOR1_SFX('SFX_OASIS_CHECK: SFX - LAKE coupling asked but CWATER /= FLAKE')
  ENDIF
ENDIF
!
!
IF(LCPL_CALVING)THEN
  IF(.NOT.IO%LGLACIER)THEN
    WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    WRITE(KLUOUT,*)'Calving flux is asked by SFX - OASIS coupling      '
    WRITE(KLUOUT,*)'CCALVING = '//TRIM(CCALVING)//' in NAM_SFX_LAND_CPL'
    WRITE(KLUOUT,*)'but LGLACIER in not activated in NAM_ISBAn         '
    WRITE(KLUOUT,*)'Please check your SURFEX namelist                  '
    WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    CALL ABOR1_SFX('SFX_OASIS_CHECK: Calving flux is asked by SFX - OASIS coupling')            
  ENDIF  
ENDIF  
!
!
IF(LCPL_GW.AND.IO%CISBA/='DIF')THEN
   WRITE(KLUOUT,*)'SFX_OASIS_CHECK: Water table depth / surface coupling requires ISBA-DF'
   CALL ABOR1_SFX('SFX_OASIS_CHECK: ISBA-DF is required for SFX - Groundwater coupling')
ENDIF   
IF(.NOT.LCPL_GW.AND.IO%CISBA=='DIF'.AND.IO%LWTD)THEN           
      WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*)'!!! A groundwater map is specified and LAND coupling    !!!'
      WRITE(KLUOUT,*)'!!!  is activated but not groundwater/surface coupling  !!!'
      WRITE(KLUOUT,*)'!!!                                                     !!!'
      WRITE(KLUOUT,*)'!!! ARE YOU SURE : YOU DO NOT WANT THIS COUPLING ?      !!!'
      WRITE(KLUOUT,*)'!!!                                                     !!!'
      WRITE(KLUOUT,*)'!!! Use NAM_SFX_LAND_CPL with CWTD and CFWTD            !!!'
      WRITE(KLUOUT,*)'!!!  if you want this coupling via OASIS                !!!'
      WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'  
ENDIF
!
!
IF(LWATER.AND.(U%CWATER=='NONE '.OR.U%CWATER=='FLAKE'))THEN
  WRITE(KLUOUT,*)'LWATER = ',LWATER,'   CWATER = ',U%CWATER
  WRITE(KLUOUT,*)'! Inland water should not be added to sea mask in case CWATER is NONE or FLAKE !!!'     
  WRITE(KLUOUT,*)'! Change CWATER or put LWATER=.FALSE. in NAM_SFX_SEA_CPL !!!'     
  CALL ABOR1_SFX('SFX_OASIS_READ_NAM: LWATER and CWATER not consistent')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_CHECK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_CHECK
