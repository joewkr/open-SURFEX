!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!     ######################
       MODULE MODD_CHS_AEROSOL
!!     ######################
!!
!!     PURPOSE
!!     -------
!!
!!     declaration of variables and types for the aerosol system in the
!!     surface meteo-france scheme
!!
!!     METHOD
!!     ------
!!
!!
!!     REFERENCE
!!     ---------
!!     none
!!
!!
!!     AUTHOR
!!     ------
!!     P. Tulet (CNRM)
!!
!!
!!     MODIFICATIONS
!!     -------------
!!
!!--------------------------------------------------------------------
!!     DECLARATIONS
!!     ------------
IMPLICIT NONE
!
! aerosol mode parameters
LOGICAL      :: LCH_AERO_FLUX     = .FALSE. ! switch to active pronostic aerosols
!
LOGICAL      :: LCO2PM            = .FALSE. ! switch to active primary emission derived from CO 

LOGICAL      :: LVARSIGI  = .FALSE.   ! switch to active pronostic dispersion for I mode
LOGICAL      :: LVARSIGJ  = .FALSE.   ! switch to active pronostic dispersion for J mode

!
INTEGER, PARAMETER         :: JPMODE=2      ! number of modes
INTEGER, PARAMETER         :: JPIN=JPMODE*3 ! number of differential equations
INTEGER, SAVE, DIMENSION(JPMODE) :: NM0,NM3,NM6   ! index of the moments in arrays
!
!* indices of Aerosol chemical parameters
!
INTEGER, PARAMETER :: NSP=4        ! number of chemical species
                                   ! for ARES or isorropia NSP=4 these are
INTEGER, PARAMETER :: JP_AER_SO4 = 1
INTEGER, PARAMETER :: JP_AER_NO3 = 2
INTEGER, PARAMETER :: JP_AER_NH3 = 3
INTEGER, PARAMETER :: JP_AER_H2O = 4
!
INTEGER, PARAMETER :: JP_AER_SO4g = JP_AER_SO4
INTEGER, PARAMETER :: JP_AER_NO3g = JP_AER_NO3
INTEGER, PARAMETER :: JP_AER_NH3g = JP_AER_NH3
!
INTEGER, PARAMETER :: NCARB=3     ! number of chemically inert species
                                  ! (like black carbon)
INTEGER, PARAMETER :: JP_AER_OC = 5
INTEGER, PARAMETER :: JP_AER_BC = 6
INTEGER, PARAMETER :: JP_AER_DST = 7

INTEGER            :: NSOA = 10    ! number of condensable species that may form
                                   ! secondary aerosols
INTEGER            :: NM6_AER = 2  ! number of condensable species that may form
                                   ! secondary aerosols
INTEGER            :: JP_AER_SOA1 = 8 
INTEGER            :: JP_AER_SOA2 = 9
INTEGER            :: JP_AER_SOA3 = 10
INTEGER            :: JP_AER_SOA4 = 11
INTEGER            :: JP_AER_SOA5 = 12
INTEGER            :: JP_AER_SOA6 = 13
INTEGER            :: JP_AER_SOA7 = 14
INTEGER            :: JP_AER_SOA8 = 15
INTEGER            :: JP_AER_SOA9 = 16 
INTEGER            :: JP_AER_SOA10 = 17

 CHARACTER(LEN=32),DIMENSION(:), ALLOCATABLE :: CAERONAMES

INTEGER            :: JP_CH_SO4I = 1  
INTEGER            :: JP_CH_SO4J = 2  
INTEGER            :: JP_CH_NO3I = 3  
INTEGER            :: JP_CH_NO3J = 4  
INTEGER            :: JP_CH_NH3I = 5  
INTEGER            :: JP_CH_NH3J = 6  
INTEGER            :: JP_CH_H2OI = 7  
INTEGER            :: JP_CH_H2OJ = 8  
INTEGER            :: JP_CH_OCI  = 9  
INTEGER            :: JP_CH_OCJ  = 10  
INTEGER            :: JP_CH_BCI  = 11  
INTEGER            :: JP_CH_BCJ  = 12 
INTEGER            :: JP_CH_DSTI  = 13 
INTEGER            :: JP_CH_DSTJ  = 14 
INTEGER            :: JP_CH_SOA1I  = 15  
INTEGER            :: JP_CH_SOA1J  = 16
INTEGER            :: JP_CH_SOA2I  = 17
INTEGER            :: JP_CH_SOA2J  = 18
INTEGER            :: JP_CH_SOA3I  = 19
INTEGER            :: JP_CH_SOA3J  = 20  
INTEGER            :: JP_CH_SOA4I  = 21  
INTEGER            :: JP_CH_SOA4J  = 22 
INTEGER            :: JP_CH_SOA5I  = 23  
INTEGER            :: JP_CH_SOA5J  = 24  
INTEGER            :: JP_CH_SOA6I  = 25  
INTEGER            :: JP_CH_SOA6J  = 26  
INTEGER            :: JP_CH_SOA7I  = 27  
INTEGER            :: JP_CH_SOA7J  = 28  
INTEGER            :: JP_CH_SOA8I  = 29  
INTEGER            :: JP_CH_SOA8J  = 30  
INTEGER            :: JP_CH_SOA9I  = 31  
INTEGER            :: JP_CH_SOA9J  = 32  
INTEGER            :: JP_CH_SOA10I = 33  
INTEGER            :: JP_CH_SOA10J = 34  
INTEGER            :: JP_CH_M0I = 35  
INTEGER            :: JP_CH_M0J = 36  
INTEGER            :: JP_CH_M6I = 37  
INTEGER            :: JP_CH_M6J = 38  

!INTEGER, PARAMETER :: JPNN=NSP+NSOA+NCARB
                                   
! Index for gas species which interact with aerosols
INTEGER :: JP_CH_HNO3,  JP_CH_H2SO4, JP_CH_NH3, JP_CH_O3, JP_CH_CO,  &
             JP_CH_URG1, JP_CH_URG2, JP_CH_RPG2, JP_CH_RP18, JP_CH_UR26,&
             JP_CH_RPG3, JP_CH_URG4, JP_CH_UR8, JP_CH_UR17, JP_CH_UR7, JP_CH_URG6, &
             JP_CH_ARAC, JP_CH_URG7, JP_CH_RPG7, JP_CH_RPR7, JP_CH_URG8, JP_CH_UR19, &
             JP_CH_URG9, JP_CH_URG10, JP_CH_PAN8, JP_CH_UR22, JP_CH_RPR4, JP_CH_AP7, &
             JP_CH_RPR3, JP_CH_UR21, JP_CH_UR28, JP_CH_UR29,  JP_CH_UR30, &
             JP_CH_RPR9, JP_CH_RP12, JP_CH_UR3, JP_CH_UR23, JP_CH_UR31, JP_CH_AP1, &
             JP_CH_AP6, JP_CH_ADAC, JP_CH_UR2, JP_CH_UR14, JP_CH_UR27, JP_CH_RP14, &
             JP_CH_RP19, JP_CH_UR11, JP_CH_UR15, JP_CH_AP10, JP_CH_UR20, JP_CH_UR34, &
             JP_CH_AP11, JP_CH_AP12, JP_CH_UR5, JP_CH_UR6, JP_CH_AP8  

INTEGER :: JP_CH_H2O2,  JP_CH_SO2, JP_CH_SO42M
!



! Molar mass of each aerosols parents (in kg/mol)
REAL, PARAMETER :: XHNO3=63.01287
REAL, PARAMETER :: XNH3 =17.03061
REAL, PARAMETER :: XURG1=88.
REAL, PARAMETER :: XURG2=1.76981E+02
REAL, PARAMETER :: XRPG2=1.68000E+02
REAL, PARAMETER :: XRP18=1.84000E+02
REAL, PARAMETER :: XRPG3=1.53772E+02
REAL, PARAMETER :: XURG4=1.95867E+02
REAL, PARAMETER :: XUR17=1.72000E+02
REAL, PARAMETER :: XRPR3=1.86000E+02
REAL, PARAMETER :: XAP7 =2.33000E+02
REAL, PARAMETER :: XURG6=1.89153E+02
REAL, PARAMETER :: XUR22=2.12000E+02
REAL, PARAMETER :: XURG7=1.56781E+02
REAL, PARAMETER :: XRPR4=1.67000E+02
REAL, PARAMETER :: XRPR7=1.50000E+02
REAL, PARAMETER :: XRPG7=1.96059E+02
REAL, PARAMETER :: XURG8=1.73777E+02
REAL, PARAMETER :: XURG9=2.61676E+02
REAL, PARAMETER :: XUR26=1.68000E+02
REAL, PARAMETER :: XURG10=2.14834E+02
REAL, PARAMETER :: XUR7=1.68000E+02
REAL, PARAMETER :: XUR8=1.84000E+02
REAL, PARAMETER :: XPAN8=2.63000E+02
REAL, PARAMETER :: XARAC=1.32000E+02
REAL, PARAMETER :: XUR19=1.70000E+02



!
REAL         :: XEMISRADIUSI  = 0.036   ! mean radius of primary aerosol
                                       ! emission for I mode
REAL         :: XEMISRADIUSJ  = 0.385    ! mean radius of primary aerosol
                                       ! emission for J mode
REAL         :: XEMISSIGI     = 1.86   ! dispersion of primary aerosol
                                       ! emission for I mode
REAL         :: XEMISSIGJ     = 1.29   ! dispersion of primary aerosol
                                       ! emission for J mode
 CHARACTER*4  :: CRGUNIT   = 'NUMB'    ! type of log-normal geometric mean radius given
!                                     ! in nameliste (mass on number)



!----------------------------------------------------------------------------
!
!*  constants
!
REAL, PARAMETER :: XPBOLTZ=1.380658e-23    ! Boltzmann constant (J/K)
REAL, PARAMETER :: XAVOGADRO=6.0221367E+23 ! Avogadro constant
REAL, PARAMETER :: XMD    = 28.9644E-3     ! Air mass molarity

!
END MODULE MODD_CHS_AEROSOL
