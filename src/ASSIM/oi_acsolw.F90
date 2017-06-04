!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE OI_ACSOLW (KST,KNBPT,&
!-----------------------------------------------------------------------
! - INPUT  1D
   PARG,PD2,PWS,PIVEG,PSAB,&
! - INPUT  LOGIQUE
   ODHMT,&
! - OUTPUT 1D .
   PWFC,PWPMX,PWSAT,PWSMX,PWWILT)  
  

!**** *ACSOLW  * - DETERMINATION DES CONTENUS EN EAU CARACTERISTIQUES DU SOL

!     Sujet.
!     ------

!     - ROUTINE DE CALCUL INTERMEDIAIRE.
!       DETERMINATION DES CONTENUS EN EAU CARACTERISTIQUES DU SOL.

!    SUR MER      (PITM=0. , PIVEG=NTVMER    )
!       wwilt=0.        wfc=1.        wsat=1.
!    SUR BANQUISE (PITM=1. , PIVEG=NTVGLA*1.1) (MODE CLIMAT SEULEMENT)
!       wwilt=0.        wfc=1.        wsat=1.
!    SUR GLACIER  (PITM=1. , PIVEG=NTVGLA    )
!       wwilt=f1(arg)   wfc=f2(arg)   wsat=wfc
!    SUR TERRE    (PITM=1. , AUTRES PIVEG    )
!       wwilt=f1(arg)   wfc=f2(arg)   wsat=f3(sab)

!**   Interface.
!     ----------
!        *CALL* *ACSOLW*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KST      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
! KNBPT      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.


! - NOM DES CLES LOGIQUES

! LDHMT      : CALCULS REDUITS (POUR L'ANALYSE OU FULL-POS)
!              SEULS LES TABLEAUX (*) SONT UTILISES/CALCULES EN E/S

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 1D (GEOGRAPHIQUE) .

! PARG (*)   : POURCENTAGE D'ARGILE DANS LA MAILLE.
! PD2        : EPAISSEUR DU RESERVOIR PROFOND.
! PWS (*)    : CONTENU EN EAU SUPERFICIEL (pour identification NATURE)
! PIVEG      : TYPE DE SURFACE (MER, BANQUISE OU GLACIER, TERRE).
! PSAB       : POURCENTAGE DE SABLE DANS LA MAILLE.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 1D (DIAGNOSTIQUE) .

! PWFC   (*) : TENEUR EN EAU A LA CAPACITE AUX CHAMPS.
! PWPMX      : TENEUR EN EAU MAXIMALE DU RESERVOIR PROFOND.
! PWSAT      : TENEUR EN EAU A LA SATURATION
! PWSMX      : TENEUR EN EAU MAXIMALE DU RESERVOIR DE SURFACE.
! PWWILT (*) : TENEUR EN EAU CORRESPONDANT AU POINT DE FLETRISSEMENT.

!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMPHY1/
! the common that contains the parameters for the land surface part of the 
! physics of the model
!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------

!     Auteur.
!     -------
!        99-02, D. Giard, d'apres ACSOL

!     Modifications.
!     --------------
!-----------------------------------------------------------------------
!
!  
USE MODD_ASSIM, ONLY : NTVGLA, XG1WSAT, XG2WSAT, XEWFC, XGWFC, XEWWILT, XGWWILT, XRD1, XREPS2
USE MODD_CSTS,  ONLY : XRHOLW
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!  
REAL    ,INTENT(IN)    :: PARG(KNBPT)
REAL    ,INTENT(IN)    :: PD2(KNBPT)
REAL    ,INTENT(IN)    :: PWS(KNBPT)
REAL    ,INTENT(IN)    :: PIVEG(KNBPT) 
REAL    ,INTENT(IN)    :: PSAB(KNBPT)
!
LOGICAL, INTENT(IN)    :: ODHMT
!
REAL    ,INTENT(OUT)   :: PWFC(KNBPT) 
REAL    ,INTENT(OUT)   :: PWPMX(KNBPT) 
REAL    ,INTENT(OUT)   :: PWSAT(KNBPT) 
REAL    ,INTENT(OUT)   :: PWSMX(KNBPT) 
REAL    ,INTENT(OUT)   :: PWWILT(KNBPT)
!
REAL    :: ZARG, ZSAB
INTEGER    :: KNBPT, KST
INTEGER    :: JROF
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!  ** initialization of the parameters of the land surface part of 
!  ** the physics of the model
!
!     ------------------------------------------------------------------
!     I - CALCULS REDUITS DANS LE CAS LDHMT=.TRUE.
!         ----------------------------------------
!         REDUCED COMPUTATIONS IN CASE LDHMT=.T.
!         --------------------------------------
!
! ** WFC , WWILT **
!
IF (LHOOK) CALL DR_HOOK('OI_ACSOLW',0,ZHOOK_HANDLE)
!
DO JROF=KST,KNBPT
  IF ( PWS(JROF) == XUNDEF ) THEN
    PWWILT(JROF) = 0.0
    PWFC  (JROF) = 1.0
  ELSE
    ZARG = MAX(XREPS2,PARG(JROF))
    PWWILT(JROF) = XGWWILT*(ZARG**XEWWILT)
    PWFC  (JROF) = XGWFC  *(ZARG**XEWFC)
  ENDIF
ENDDO
!
IF (ODHMT .AND. LHOOK) CALL DR_HOOK('OI_ACSOLW',1,ZHOOK_HANDLE)
IF (ODHMT) RETURN
!
!     ------------------------------------------------------------------
!     II - CALCUL DES AUTRES CHAMPS
!          ------------------------
!          COMPUTING OTHER FIELDS
!          ----------------------
!
DO JROF=KST,KNBPT
!
! ** CAS DE LA BANQUISE EN MODE CLIMAT - SEA-ICE - (CLIMAT) **
!
  IF ( NINT(10.*PIVEG(JROF))==(10*NTVGLA+1) ) THEN
    PWWILT(JROF) = 0.0
    PWFC  (JROF) = 1.0
  ENDIF
!
! ** WSAT **
!
  IF ( PWS(JROF)==XUNDEF .OR. NINT(PIVEG(JROF))==NTVGLA ) THEN
    PWSAT(JROF) = PWFC(JROF)
  ELSE
    ZSAB = MAX(XREPS2,PSAB(JROF))
    PWSAT(JROF) = XG1WSAT*ZSAB + XG2WSAT
  ENDIF
!
! **  WSMX , WPMX **
!
  PWPMX(JROF) = PWSAT(JROF)*PD2(JROF)*XRHOLW
  PWSMX(JROF) = PWSAT(JROF)*XRD1*XRHOLW
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('OI_ACSOLW',1,ZHOOK_HANDLE)
!
END SUBROUTINE OI_ACSOLW
