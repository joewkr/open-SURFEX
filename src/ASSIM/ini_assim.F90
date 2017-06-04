!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_ASSIM 
!     ####################
!
!!****  *INI_ASSIM * - routine to initialize the module MODD_ASSIM
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize variables in module MODD_ASSIM.
!      
!
!!**  METHOD
!!    ------
!!      The various constants are set to their numerical values 
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_ASSIM     : contains constants for soil assimilation
!!
!! 
!!    AUTHOR
!!    ------
!!      J.-F. Mahfouf       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/05/09 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ASSIM, ONLY : LFGEL, LCLIM, XSODELX, NTVGLA, XRD1, XRTINER, XWCRIN, &
                       XWPMX, XWSMX, XTMERGL, XRZHZ0G, NSEAICE, XRWPIA, &
                       XRWPIB, XRSNSA, XRSNSB, XSALBM, XSALBB, XSEMIM, XSEMIB, &
                       XSZZ0B, LHUMID, LLDHMT, LISSEW, NLISSEW, NMINDJ, NNEBUL, &
                       NNEIGT, NNEIGW, XANEBUL, XRCLIMN, XRCLIMTP, XRCLIMTS, &
                       XRCLIMV, XRCLIMWP, XRCLIMWS, XSCOEFH, XSCOEFT, XSEVAP, &
                       XSICE, XSNEIGT, XSNEIGW, XSPRECIP, XSWFC, XV10MX, XSMU0, &
                       L_SM_WP, NR_SM_WP, XRA_SM_WP, XSIGHP1, XSIGT2MR, XSIGH2MR, &
                       XRSABR, XRARGR, XGWFC, XEWFC, XGWWILT, XEWWILT, XG1WSAT, &
                       XG2WSAT, XADWR, XREPS1, XREPS2, XREPS3, NIDJ, XREPSM, &
                       XRCDTR
!
USE MODD_CSTS,  ONLY : XPI 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!  
!**-----------------------------------------------------------------------
!**  - 1 - Initialisations, diagnostics.
!**        ----------------------------

!*   1.1  Constantes
!
! NECHGU : ECHEANCE DU GUESS EN HEURES (0 A 30).
!
IF (LHOOK) CALL DR_HOOK('INI_ASSIM',0,ZHOOK_HANDLE)
!
!LFGEL   : CLE D'APPEL DU GEL DE L'EAU DU SOL AVEC ISBA (LSOLV)
!        : KEY FOR SOIL FREEZING WITH ISBA (LSOLV)
!          ( ACSOL, ACDROV)
LFGEL = .TRUE.
LCLIM = .TRUE.
!
!  Characteristics of ice and sea 
!SODELX(0:9): DISCRETISATION VERTICALE DU SOL (MAXI 10 COUCHES)
!           : SOIL VERTICAL DISCRETIZATION (MAX 10 LAYERS)
!GCONV      : FACTEUR DE CHANGEMENT D'UNITE (MASSE VOL. DE L'EAU LIQUIDE)
!RD1        : EPAISSEUR DU RESERVOIR SUPERFICIEL.
!           : UPPER RESERVOIR DEPTH.
!RTINER     : RAPPORT DES INERTIES THERMIQUES PROFONDEUR/SURFACE.
!           : RATIO OF THE DEEP/SURFACE THERMAL INERTIAS.
!WCRIN      : QUANTITE CRITIQUE POUR LA NEIGE MI-COUVRANTE.
!           : CRITICAL TRANSITION VALUE FOR SNOW DEPTH (HALF-COVER).
!WPMX       : VALEUR MAXIMUN POUR LE CONTENU EN EAU EN PROFONDEUR.
!               : MAXIMUM VALUE FOR DEEP SOIL WATER CONTENT.
!WSMX       : VALEUR MAXIMUN POUR LE CONTENU EN EAU DE SURFACE.
!           : MAXIMUM VALUE FOR SURFACE SOIL WATER CONTENT.
!NTVGLA     : INDICE DE VEGETATION SUR GLACE.
!TMERGL     : TEMPERATURE DE FONTE DE LA GLACE DE MER.
!           : MELTING TEMPERATURE OF FLOATING ICE.
!RZHZ0G     : RAPPORT LONGUEUR DE RUGOSITE THERMIQUE SUR LONGUEUR
!             RUGOSITE DYNAMIQUE DE LA BANQUISE
XSODELX(0) = 1.0/SQRT(1.0+2.0*XPI)
DO J=1,9
  XSODELX(J) = XSODELX(J-1)*2.0*XPI
ENDDO
!
NTVGLA = 2
!
XRD1    = 1.E-2
XRTINER = 5.
XWCRIN  = 10.
XWPMX   = 100.
XWSMX   = 20.
XTMERGL = 271.23 
XRZHZ0G = 1.0 
!
!nactex, canali
!  NSEAICE : utilsation limite glace SSM/I (et nb de jours de retard possible)
!  RSNSA   : Coefficient pour l'analyse de neige
!  RSNSB   : Coefficient pour l'analyse de neige
!  RWPIA   : Coefficient pour l'analyse de l'eau gelee
!  RWPIB   : Coefficient pour l'analyse de l'eau gelee
!
NSEAICE = 0
!
XRWPIA   = 0.025
XRWPIB   = 2.  
XRSNSA   = 0.025
XRSNSB   = 1.0 
!
!yomcli
!  SALBN,SALBX,SALBM,SALBG,SALBB,SALBD : albedo
!  SEMIN,SEMIX,SEMIM,SEMIG,SEMIB,SEMID : emissivity
!  (minimum,maximum,sea,ice,sea ice,desert)
!  SZZ0N,SZZ0M,SZZ0B,SZZ0U,SZZ0D : roughness length
!  (minimum,sea,sea-ice,urban areas,desert)
!
XSALBM = 0.07
XSALBB = 0.65
XSEMIM = 0.96
XSEMIB = 0.97
XSZZ0B = 0.001
!
! ecarts-type d'erreurs d'observation
! T2m  -> SIGT2MO
! Hu2m -> SIGH2MO
! LSGOBS : activee si l'une des valeurs ci-dessus differe de la reference
!
! seuils sur la force du vent a 10m , la duree du jour , les precipitations,
! ws/wfc, evaporation en (mm/jour), presence de glace
!   V10MX , MINDJ , SPRECIP , SWFC  , SEVAP , SICE , SMU0
!
! cles et coefficients de reglage
!     LHUMID : activation de la clef LIMVEG  ou du seuil SWFC limitee
!     LISSEW : activation du lissage des increments de wp (si NLISSEW=3)
!     NLISSEW: nombre de reseaux anterieurs utilisables pour le lissage
!     ANEBUL , NNEBUL : dependance par rapport a la nebulosite
!     SNEIGT , NNEIGT : dependance par rapport a la neige (T)
!     SNEIGW , NNEIGW : dependance par rapport a la neige (w)
! coefficients pour le retrait du biais de T2m et Hu2m
!     SCOEFT , SCOEFH : =0. pas de retrait / =1. increment mis a zero

! champs d'increments pour le lissage (transfert dans GPPBUF de RINCW)
!     RINCW(jrof,j)  j=1/2/3 pour reseau courant - 06h/12h/18h
!     CINCW(j) : nom des champs d'increments dans les fichiers ARPEGE
!
! biais moyen
!     RBIAT(jrof) : biais moyen de temperature }  tableaux supprimes et mis dans
!     RBIAH(jrof) : biais moyen d'humidite     }   GPPBUF (adresse: MCANRI0)
!
! reglage du rappel clim
!     RCLIMTS : coef. multiplicateur de RCLIMCA pour Ts
!     RCLIMTP : coef. multiplicateur de RCLIMCA pour Tp
!     RCLIMWS : coef. multiplicateur de RCLIMCA pour Ws
!     RCLIMWP : coef. multiplicateur de RCLIMCA pour Wp
!     RCLIMN  : impact de la fraction de neige
!     RCLIMV  : coef. multiplicateur sur desert pour Ws, Wp
!
!NACVEG
!
!***
!*** conditions d'analyse
!***  MINDJ   : duree du jour minimale (heure)
!***  V10MX   : seuil sur le module du vent (analyse) a 10m
!***  SPRECIP : seuil sur les precipitations (prevues) en mm
!***  SEVAP   : seuil l'evaporation inst. en (mm/jour)
!***  SICE    : seuil sur la quantite totale de glace (Kg/m2)
!***  SMU0    : seuil utilisant l'angle zenithal solaire
!***  SWFC    : seuil sur ws/wfc (pas d'analyse de ws si ws > SWFC*wfc)
!*** ponderations
!***  LHUMID  : humidification seulement si wp < veg*wwilt
!***          : assechement seulement si ws > SWFC*wfc (pour ws)
!***  LISSEW  : lissage des increments de wp (moyenne glissante sur 24h)
!***  ANEBUL  : reduction maximale par la nebulosite
!***  NNEBUL  : puissance de la nebulosite prise en compte
!***            nebulosite moyenne neb <--> poids 1-ANEBUL*neb**NNEBUL
!***  SNEIGT  : seuil sur la fraction de la maille recouverte de neige (T)
!***  NNEIGT  : coefficient de ponderation (T)
!***  SNEIGW  : seuil sur la fraction de la maille recouverte de neige (w)
!***  NNEIGW  : coefficient de ponderation (w)
!*** retrait du biais sur T2m et Hu2m
!***  SCOEFT  : coefficient pour le retrait du biais de temperature
!***  SCOEFH  : coefficient pour le retrait du biais d'humidite
!***            =0 pas de retrait. =1 increment mis a zero
!*** reglage du rappel clim
!***  RCLIMTS : coef. multiplicateur de RCLIMCA pour Ts
!***  RCLIMTP : coef. multiplicateur de RCLIMCA pour Tp
!***  RCLIMWS : coef. multiplicateur de RCLIMCA pour Ws
!***  RCLIMWP : coef. multiplicateur de RCLIMCA pour Wp
!***  RCLIMN  : impact de la fraction de neige
!***  RCLIMV  : coef. multiplicateur sur desert pour Ws, Wp
!*** lissage spatial du SWI et modification de Wp a partir du SWI lisse
!***  L_SM_WP : lissage spatial du SWI (Soil Wetness Index) et modif Wp
!***  NR_SM_WP: nombre d'appel de la routine de lissage spatial du SWI
!***  RA_SM_WP: distance caracteristique du lissage spatial (en metres)
!***
!***---------------------------------------------------------------------
!
LHUMID   = .TRUE.
LLDHMT   = .FALSE.
LISSEW   = .FALSE.
!
NLISSEW  = 0
NMINDJ    = 6
NNEBUL   = 1
NNEIGT   = 0
NNEIGW   = 1
!
XANEBUL   = 0.75
XRCLIMN   = 0.
XRCLIMTP  = 0.
XRCLIMTS  = 0.
XRCLIMV   = 1.
XRCLIMWP  = 0.1
XRCLIMWS  = 0.
XSCOEFH   = 0.
XSCOEFT   = 0.
XSEVAP    = 0.
XSICE     = 5
XSNEIGT   = 1.
XSNEIGW   = 0.
XSPRECIP  = .3
XSWFC     = 1.0
XV10MX    = 10.
XSMU0     = 7.
!
! PARAMETERS TO SWITCH ON CASMSWI - SPATIAL SMOOTHING OF SWI (SOIL WETNESS INDEX)
! THEN CHANGING OF Wp ( TOTAL SOIL WATER CONTENT) IN CANARI OI. 
! CASMSWI IS CALLED BY CANARI
!
L_SM_WP  = .FALSE.
!
NR_SM_WP = 1
!
XRA_SM_WP = 5000.0
!
! Standard deviation of background error
!
XSIGHP1 = 15.E-2
!
! Standard deviation of observation errors (referencce)
!
XSIGT2MR = 1.0
XSIGH2MR = 10.E-2
!
! Soil textural properties (reference = loam)
!
XRSABR   = 43.
XRARGR   = 18.
XGWFC    = 89.0467E-3
XEWFC    = 0.35
XGWWILT  = 37.1342E-3
XEWWILT  = 0.5
XG1WSAT  = -1.08E-3
XG2WSAT  = 494.31E-3
!
XADWR    = XGWFC*(XRARGR**XEWFC) - XGWWILT*(XRARGR**XEWWILT)
!
! Low threshold values
!
XREPS1   = 1.E-3 
XREPS2   = 1.E-1
XREPS3   = 1.E-13
!
! Astronomical and time constants
!
NIDJ     = 12                      ! day duration
!
XREPSM   = 0.409093                ! obliquity
XRCDTR   = 24./360.
! ITRAD (half assimilation window in sec) --> dependant from NECHGU --> set in OI_MAIN
!
IF (LHOOK) CALL DR_HOOK('INI_ASSIM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_ASSIM 
