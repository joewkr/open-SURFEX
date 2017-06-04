!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!GLT_LIC The GELATO model is a seaice model used in stand-alone or embedded mode. 
!GLT_LIC  It has been developed by Meteo-France. The holder of GELATO is Meteo-France.
!GLT_LIC  
!GLT_LIC  This software is governed by the CeCILL-C license under French law and biding
!GLT_LIC  by the rules of distribution of free software. See the CeCILL-C_V1-en.txt
!GLT_LIC  (English) and CeCILL-C_V1-fr.txt (French) for details. The CeCILL is a free
!GLT_LIC  software license, explicitly compatible with the GNU GPL
!GLT_LIC  (see http://www.gnu.org/licenses/license-list.en.html#CeCILL)
!GLT_LIC  
!GLT_LIC  The CeCILL-C licence agreement grants users the right to modify and re-use the
!GLT_LIC  software governed by this free software license. The exercising of this right
!GLT_LIC  is conditional upon the obligation to make available to the community the
!GLT_LIC  modifications made to the source code of the software so as to contribute to
!GLT_LIC  its evolution.
!GLT_LIC  
!GLT_LIC  In consideration of access to the source code and the rights to copy, modify
!GLT_LIC  and redistribute granted by the license, users are provided only with a limited
!GLT_LIC  warranty and the software's author, the holder of the economic rights, and the
!GLT_LIC  successive licensors only have limited liability. In this respect, the risks
!GLT_LIC  associated with loading, using, modifying and/or developing or reproducing the
!GLT_LIC  software by the user are brought to the user's attention, given its Free
!GLT_LIC  Software status, which may make it complicated to use, with the result that its
!GLT_LIC  use is reserved for developers and experienced professionals having in-depth
!GLT_LIC  computer knowledge. Users are therefore encouraged to load and test the
!GLT_LIC  suitability of the software as regards their requirements in conditions enabling
!GLT_LIC  the security of their systems and/or data to be ensured and, more generally, to
!GLT_LIC  use and operate it in the same conditions of security. 
!GLT_LIC  
!GLT_LIC  The GELATO sofware is cureently distibuted with the SURFEX software, available at 
!GLT_LIC  http://www.cnrm.meteo.fr/surfex. The fact that you download the software deemed that
!GLT_LIC  you had knowledge of the CeCILL-C license and that you accept its terms.
!GLT_LIC  Attempts to use this software in a way not complying with CeCILL-C license
!GLT_LIC  may lead to prosecution. 
!GLT_LIC 
!* Solves vertical heat diffusion for an ice + snow slab.
!
! Created : 02/2010 (D. Salas y Melia)
! Modified: 03/2010 (D. Salas y Melia) 
!    Now a single routine solves the heat diffusion scheme either for an ice
!    slab or for an ice + snow slab
! 
!THXS_SFX!MODULE modi_glt_vhdslab_r
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE glt_vhdslab_r  &
!THXS_SFX!        ( kit,pnsftop,pswtra,pderiv,  &
!THXS_SFX!          pcondb,ptsfa,pqtopmelt,pdh,ptsia,osmelt,osnow )
!THXS_SFX!  USE modd_glt_const_thm
!THXS_SFX!  USE modd_glt_param
!THXS_SFX!  USE modd_glt_vhd
!THXS_SFX!  USE modi_glt_invert
!THXS_SFX!  INTEGER, INTENT(in) ::  &
!THXS_SFX!    kit
!THXS_SFX!  REAL, INTENT(in) ::  &
!THXS_SFX!    pnsftop
!THXS_SFX!  REAL, DIMENSION(nl), INTENT(in) ::  &
!THXS_SFX!    pswtra
!THXS_SFX!  REAL, INTENT(inout) ::  &
!THXS_SFX!    pderiv
!THXS_SFX!  REAL, INTENT(out) ::  &
!THXS_SFX!    pcondb,ptsfa,pqtopmelt
!THXS_SFX!  REAL, DIMENSION(nl), INTENT(out) ::  &
!THXS_SFX!    pdh
!THXS_SFX!  REAL, DIMENSION(0:nilay), INTENT(out) ::  &
!THXS_SFX!    ptsia
!THXS_SFX!  LOGICAL, INTENT(out) ::  &
!THXS_SFX!    osmelt
!THXS_SFX!  LOGICAL, OPTIONAL, INTENT(in) ::  &
!THXS_SFX!    osnow
!THXS_SFX!END SUBROUTINE glt_vhdslab_r
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_glt_vhdslab_r
!
! --------------------- END MODULE modi_glt_vhdslab_r -----------------------
!
!
! -----------------------------------------------------------------------
! ----------------------- SUBROUTINE glt_vhdslab_r --------------------------
!
SUBROUTINE glt_vhdslab_r  &
        ( kit,pnsftop,pswtra,pderiv,  &
          pcondb,ptsfa,pqtopmelt,pdh,ptsia,osmelt,osnow )
!
  USE modd_glt_const_thm
  USE modd_glt_param, only : nilay, nl
  USE modd_glt_vhd
  USE modi_glt_invert
!
  IMPLICIT NONE
!
  INTEGER, INTENT(in) ::  &
    kit
  REAL, INTENT(in) ::  &
    pnsftop
  REAL, DIMENSION(nl), INTENT(in) ::  &
    pswtra
  REAL, INTENT(inout) ::  &
    pderiv
  REAL, INTENT(out) ::  &
    pcondb,ptsfa,pqtopmelt
  REAL, DIMENSION(nilay+1), INTENT(out) ::  &
    pdh
  REAL, DIMENSION(0:nilay), INTENT(out) ::  &
    ptsia
  LOGICAL, INTENT(out) ::  &
    osmelt
  LOGICAL, OPTIONAL, INTENT(in) ::  &
    osnow
!
!
! 1.2. Local variables
! --------------------
!
  REAL, PARAMETER ::  &
       pptsfmin=-50.
  LOGICAL ::  &
       llmeltop,gsnow
  LOGICAL, DIMENSION(0:nilay) ::  &
       llmelt
  INTEGER ::  &
       jl,itop
  REAL ::  &
       zderiv,ztsm,zmelt,zdht,zdqdt,zbl
  REAL, DIMENSION(0:nilay+1) ::  &
        zx,zy
  REAL, DIMENSION(0:nilay+1,0:nilay+1) ::  &
        zmat,zmatinv
!
!
!
! 1. Initializations 
! ===================
!
! 1.1. Build the M matrix of the system (M*x = y) and glt_invert it
! ------------------------------------------------------------- 
!
! Snow layer boolean
  IF ( PRESENT(osnow) ) THEN
      gsnow = osnow
    ELSE
      gsnow = .FALSE.
  ENDIF
  IF ( gsnow ) THEN
      itop = 0
    ELSE
      itop = 1
  ENDIF
!
! Surface melting temperature
  ztsm = ztsi_m0(itop)
!
! Surface melting boolean
! MAJOR MODIFICATION SINCE IPCC VERSION
! We assume the final temperature will probably correspond to melting 
! if current temperature is melting point AND top conduction flux
! is positive (in principle, a surface gradient condition would be more
! correct...)
  llmeltop = ( ztsfb>=ztsm .AND. zcondt_m>0. )
!!  lltempcond = ( llmelt .AND. ABS(znsftop0-zcondt_m)>epsil1 )
!
! Ice and snow slab melting boolean
!  IF ( kit>2 ) THEN
!      llmelt = ( ztsib>=ztsi_m0 )
!    ELSE
      llmelt(:) = .FALSE.
!  ENDIF
!
! Define the matrix
  zmat(:,:) = 0.
!
  IF ( .NOT. gsnow ) zmat(0,0) = 1.
!
  IF ( llmeltop ) THEN 
      IF (lp4) THEN
          WRITE(noutlu,*) '=================================='
          WRITE(noutlu,*) '   Temp. condition: ztsfb=',ztsfb
          WRITE(noutlu,*) '=================================='
      ENDIF
      zmat(itop,itop)   = 1.
      zmat(itop,itop+1) = 0.
      zmat(itop+1,itop) = 0.
    ELSE
      IF (lp4) THEN
          WRITE(noutlu,*) '=================================='
          WRITE(noutlu,*) '   Flux condition : ztsfb=',ztsfb
          WRITE(noutlu,*) '                    zcondt_m=',zcondt_m
          WRITE(noutlu,*) '=================================='
      ENDIF
      zmat(itop,itop)   = pderiv - zkodzi(itop)
      zmat(itop,itop+1) = zkodzi(itop)
      IF ( .NOT.llmelt(itop) ) zmat(itop+1,itop) = -zetaik(itop)
  ENDIF
!
  IF ( llmelt(itop) ) THEN
      zmat(itop+1,itop+1) = 1.
    ELSE
      zmat(itop+1,itop+1) = 1. + zetaik(itop) + zetaikp1(itop)
      zmat(itop+1,itop+2) = -zetaikp1(itop)
  ENDIF
!
  DO jl=itop+2,nilay
    IF ( llmelt(jl-1) ) THEN
        zmat(jl,jl) = 1.
      ELSE
        zmat(jl,jl-1) = -zetaik(jl-1)
        zmat(jl,jl)   = 1. + zetaik(jl-1) + zetaikp1(jl-1)
        zmat(jl,jl+1) = -zetaikp1(jl-1)
    ENDIF
  END DO
!
  IF ( llmelt(nilay) ) THEN
      zmat(nilay+1,nilay+1) = 1.
    ELSE
      zmat(nilay+1,nilay) = -zetaik(nilay) + zg2*zetaikp1(nilay)
      zmat(nilay+1,nilay+1) = 1. + zetaik(nilay) + zg1*zetaikp1(nilay)
  ENDIF
!
! .. Print the matrix on request
!
  IF ( lp5 .OR. llredo ) THEN
      WRITE(noutlu,*) &
        '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Initial matrix M'
      DO jl=itop,nilay+1
        WRITE(noutlu,*) zmat(jl,:)
      END DO
  ENDIF
!
! .. Invert the matrix 
!
  zmatinv = zmat
  CALL glt_invert( 1,zmatinv )
!
!
! 1.2. Build the y vector of the system to solve (M*x = y)
! --------------------------------------------------------
!
  IF ( .NOT. gsnow ) zy(0) = 0.
!
  IF ( llmeltop ) THEN 
      zy(itop)   = ztsm
      IF ( llmelt(itop) ) THEN
          zy(itop+1) = ztsi_m0(itop)
        ELSE
          zy(itop+1) = ztsi0(itop) + zetaik(itop)*ztsm +  &
            xswhdfr*pswtra(nilay+1-itop)*zetai(itop)
      ENDIF
    ELSE
      zy(itop)   = -znsftop0 + pderiv*ztsf0
      IF ( llmelt(itop) ) THEN
          zy(itop+1) = ztsi_m0(itop)
        ELSE
          zy(itop+1) = ztsi0(itop) +  &
            xswhdfr*pswtra(nilay+1-itop)*zetai(itop)
      ENDIF
  ENDIF
  DO jl=itop+2,nilay
    IF ( llmelt(jl-1) ) THEN
        zy(jl) = ztsi_m0(jl-1)
      ELSE
        zy(jl) = ztsi0(jl-1) + xswhdfr*pswtra(nilay+2-jl)*zetai(jl-1)
    ENDIF
  END DO
  IF ( llmelt(nilay) ) THEN
      zy(nilay+1) = ztsi_m0(nilay)
    ELSE
      zy(nilay+1) = (zg1+zg2)*zetaikp1(nilay)*zmlf + ztsi0(nilay) +  &
        xswhdfr*pswtra(1)*zetai(nilay)
  ENDIF 
!
!
! 1.3. Solve the system
! ----------------------
! 
! .. Now we update the vertical temperature profile
!
  IF ( lp4 .OR. llredo ) THEN
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Initial vector'
      WRITE(noutlu,*) zy
      WRITE(noutlu,*) 'Initial vtp'
      WRITE(noutlu,*) ztsi0(:)
  ENDIF
!
  DO jl=0,nilay+1
    zx(jl) = SUM( zmatinv(jl,:)*zy(:) )
  END DO
!
  IF ( lp4 .OR. llredo ) THEN
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Initial matrix by found vector:'
      DO jl=0,nilay+1
        WRITE(noutlu,*) SUM( zmat(jl,:)*zx(:) )
      END DO
      WRITE(noutlu,*)
  ENDIF
!
!
! 1.4. Update variables
! ---------------------
! 
! .. Surface temperature
  ptsfa = MAX( zx(itop),pptsfmin )
!
! .. Sea ice (and snow) vertical temperature profile
  DO jl=0,nilay
    ptsia(jl) = zx(jl+1)
  END DO
!
! .. Flux at the bottom of the slab
  pcondb =  &
    zkodzi(nilay+1)*( zg1*( zmlf-ptsia(nilay) )+  &
    zg2*( zmlf-ptsia(nilay-1) ) )
!
! .. (melting case): derive top conductive heat flux
  llmeltop = ( ptsfa>=ztsm )
  IF ( llmeltop ) THEN 
    zcondt_m = -zkodzi(itop)* ( ptsia(itop) - ptsfa )
  ELSE
    zcondt_m = pnsftop
  ENDIF
!    ( AMIN1(ptsia(itop),ztsm) - AMIN1(ptsfa,ztsm) )
!!  lltempcond = ( llmelt .AND. ABS(znsftop0-zcondt_m)>epsil1 )
!
! .. Additional surface heat flux (due to the sensitivity of surface heat flux
! to changes in surface temperature)
  IF ( llmeltop ) THEN
      zderiv = 0.
    ELSE
      zderiv = pderiv
  ENDIF
!
! .. Compute lost dQ/dT * Dt due to surface T limitation
! (and add it to bottom conduction)
  zdqdt = zderiv*MIN( zx(itop)-pptsfmin,0. )
  pcondb = pcondb + zdqdt
!
! .. Collect all the energy corresponding to temperatures higher than
! melting point
!  - in sea ice
  zmelt = SUM( zinvetai(:)*MAX(ptsia(:)-ztsi_m0(:),0.) )
!!  write(noutlu,*) 'zmelt =',zmelt
!!  write(noutlu,*) 'ptsia =',ptsia
  ptsia(:) = MIN( ptsia(:),ztsi_m0(:) )
!!  write(noutlu,*) 'ptsia =',ptsia
!    
! .. Deduce which part of the top non-solar flux is devoted to melting
  pqtopmelt = znsftop0-zcondt_m+zmelt
!!  pqtopmelt = zmelt
!!  write(noutlu,*) 'znsftop0 =',znsftop0
!!  write(noutlu,*) 'zcondt_m =',zcondt_m
!
  IF ( lp4 .OR. llredo ) THEN
      WRITE(noutlu,*) 'Previous surface temperature =',ztsf0
      WRITE(noutlu,*) 'New surface temperature      =',ptsfa
      WRITE(noutlu,*) 'Previous vtp:'
      WRITE(noutlu,*) ztsi0
      WRITE(noutlu,*) 'New vtp     :'
      WRITE(noutlu,*) ptsia
      WRITE(noutlu,*) 'zcondt   =',znsftop0
  ENDIF 
!
! .. Melting surface boolean
  osmelt = llmeltop
!
!
! 1.5. Energy conservation check
! -------------------------------
!
  IF ( lp3 .OR. llredo ) THEN
!
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Energy check'
      WRITE(noutlu,*) '============='
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Left hand-side (vhdslab):'
      WRITE(noutlu,*) '--------------------------'
      WRITE(noutlu,*) ' Non-solar heat flux at the top:',  &
        pnsftop
      WRITE(noutlu,*) ' Absorbed solar heat flux:',  &
        xswhdfr * SUM(pswtra(1:nilay+1-itop))
      WRITE(noutlu,*) ' Cond. heat flux at the bottom:',  &
        pcondb
      WRITE(noutlu,*) &
        ' Additional heat flux at the top dQ/dT*(Tnew-Told):',  &
        zderiv*( ptsfa-ztsf0 )
!!      WRITE(noutlu,*) ' Cond. heat flux at the top:',  &
!!        zcondt_m
      WRITE(noutlu,*)
      WRITE(noutlu,*) 'Right hand-side:'
      WRITE(noutlu,*) '-----------------'
      zdht = SUM( ( ptsia(:)-ztsi0(:) ) * zinvetai(:) )
      WRITE(noutlu,*) '  Enthalpy change per time unit:',  &
        zdht
      WRITE(noutlu,*) '  Top melting flux:',  &
        pqtopmelt
! 
!!      WRITE(noutlu,*) 'Enthalpy change per time unit (layers):'
!!      WRITE(noutlu,*) '----------------------------------------'
!!      WRITE(noutlu,*) ( ptsia(:)-ztsi0(:) ) * zinvetai(:)
!!      WRITE(noutlu,*)
      WRITE(noutlu,*)
      WRITE(noutlu,*)  &
        'Left hand-side minus right-hand side'
      WRITE(noutlu,*)  &
        '-------------------------------------'
      zbl = zdht + pqtopmelt -  &
        ( pcondb + xswhdfr*SUM(pswtra(1:nilay+1-itop)) +  &
          pnsftop + zderiv*( ptsfa-ztsf0 ) )
      WRITE(noutlu,*) zbl 
      IF ( ABS(zbl)>0.1 ) THEN
        IF (lwg) THEN
          WRITE(noutlu,*)'Imbalance'
          WRITE(noutlu,*)'llmeltop =',llmeltop
          WRITE(noutlu,*)'gsnow    =',gsnow
          WRITE(noutlu,*)'pqtopmelt=',pqtopmelt
          WRITE(noutlu,*)'znsftop0 =',znsftop0
          WRITE(noutlu,*)'-zcondt_m=',-zcondt_m
          WRITE(noutlu,*)'zmelt    =',zmelt
        ENDIF
      ENDIF 
!
  ENDIF
!
  IF ( lp5 .OR. llredo ) THEN
!
      WRITE(noutlu,*)
      WRITE(noutlu,*)  'Check the equations'
      WRITE(noutlu,*)   &
        '  (',itop,') ',  &
!!        ( pderiv-zkodzi(itop) )*ptsfa + zkodzi(itop)*ptsia(itop),  &
!!        pderiv*ztsf0 - pnsftop + pqtopmelt
        zmat(itop,itop)*ptsfa + zmat(itop,itop+1)*ptsia(itop),  &
!!        pderiv*ztsf0 - pnsftop + pqtopmelt
         zy(itop)
      WRITE(noutlu,*)   &
        '  (',itop+1,') ',  &
!!        - zetaik(itop)*ptsfa +  &
!!        ( 1.+zetaik(itop)+zetaikp1(itop) )*ptsia(itop)  &
!!        - zetaikp1(itop)*ptsia(itop+1),  &
        - zmat(itop+1,itop)*ptsfa +  &
        zmat(itop+1,itop+1)*ptsia(itop)  &
        - zmat(itop+1,itop+2)*ptsia(itop+1),  &
        ztsi0(itop)
!!        write(noutlu,*) '- zetaik(itop)*ptsfa',- zetaik(itop)*ptsfa
!!        write(noutlu,*) 'ptsfa =',ptsfa
!!        write(noutlu,*) '( 1.+zetaik(itop)+zetaikp1(itop) )*ptsia(itop)',  &
!!          ( 1.+zetaik(itop)+zetaikp1(itop) )*ptsia(itop)
!!        write(noutlu,*) 'ptsia(itop) =',ptsia(itop)
!!        write(noutlu,*) '- zetaikp1(itop)*ptsia(itop+1)', - zetaikp1(itop)*ptsia(itop+1)
!!        write(noutlu,*) 'ptsia(itop+1) =',ptsia(itop+1)
!
      DO jl=itop+2,nilay
        WRITE(noutlu,*)   &
          '  (',jl,') ',  &
          - zetaik(jl-1)*ptsia(jl-2) +  &
          ( 1.+zetaik(jl-1)+zetaikp1(jl-1) )*ptsia(jl-1)  &
          - zetaikp1(jl-1)*ptsia(jl),  &
          ztsi0(jl-1)
      END DO
!
      WRITE(noutlu,*)   &
        '  (',nilay+1,') ',  &
        ( -zetaik(nilay)+zg2*zetaikp1(nilay) )*ptsia(nilay-1) +  &
        ( 1.+zetaik(nilay)+zg1*zetaikp1(nilay) )*ptsia(nilay),  &
        (zg1+zg2)*zetaikp1(nilay)*zmlf + ztsi0(nilay)
!
  ENDIF
!
! .. Final update of pderiv
!
  pderiv = zderiv
!
! .. Convert gltools_enthalpy change to J.kg-1 of ice
!
  DO jl=1,nilay
    pdh(jl) = zrhocpsi(nilay+1-jl)/rhoice *  &
      ( ptsia(nilay+1-jl)-ztsi0(nilay+1-jl) )
  END DO
  IF ( gsnow ) THEN
      pdh(nilay+1) = cpice0 * ( ptsia(0)-ztsi0(0) )
    ELSE
      pdh(nilay+1) = 0.
  ENDIF
!
  END SUBROUTINE glt_vhdslab_r
