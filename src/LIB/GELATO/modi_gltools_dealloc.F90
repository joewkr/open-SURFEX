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
! =======================================================================
! ===================== MODULE modi_gltools_dealloc =======================
! =======================================================================
!
! Goal:
! -----
!   Deallocates glt_gelato main arrays and structures
!
! Created : 2004/01 (D. Salas y Melia)
! Modified: 2011/12 (D. Salas y Melia)
!           Collect the names of allocated arrays in a linked list, and
!           eventually dump this list in an array of strings, in order
!           to keep the allocation order in memory. This order will be 
!           necessary to deallocate the arrays in reverse order.
! Modified: 2012/11 (D. Salas y Melia)
!           Inverse order deallocation seems unuseful - simplify.
!           Only the main tglt super-structure is deallocated.
!
! ------------------- BEGIN MODULE modi_gltools_dealloc -------------------
!
!THXS_SFX!MODULE modi_gltools_dealloc
!THXS_SFX!INTERFACE
!THXS_SFX!!
!THXS_SFX!SUBROUTINE gltools_dealloc(tpglt)
!THXS_SFX!USE modd_types_glt 
!THXS_SFX!USE modd_glt_param
!THXS_SFX!TYPE(t_glt), INTENT(inout) ::  &
!THXS_SFX!    tpglt
!THXS_SFX!END SUBROUTINE gltools_dealloc
!THXS_SFX!!
!THXS_SFX!END INTERFACE
!THXS_SFX!END MODULE modi_gltools_dealloc
!
! ------------------- END MODULE modi_gltools_dealloc ---------------------
!
!
! -----------------------------------------------------------------------
! ------------------------- SUBROUTINE gltools_dealloc --------------------------
!
SUBROUTINE gltools_dealloc(tpglt)
!
USE modd_types_glt 
USE modd_glt_param
#if ! defined in_surfex
USE modd_CB_DynVariables
USE modd_CB_DynDim
USE modd_CB_const
USE modd_CB_mask
USE modd_CB_DynForcing
#endif
!
IMPLICIT NONE
!
TYPE(t_glt), INTENT(inout) ::  &
    tpglt
!
!
! 1. Initialisations 
! ===================
!
IF (lwg) THEN
  WRITE(noutlu,*) ' '
  WRITE(noutlu,*) '  ** LEVEL 3 - SUBROUTINE DEALLOC'
  WRITE(noutlu,*) ' '
ENDIF
!
!
#if ! defined in_surfex
!
! 2. Deallocate arrays for McGill's dynamics
! ===========================================
!
IF ( ndynami==2 ) THEN
! modd_CB_dynvariables.F90
    DEALLOCATE( h )
    DEALLOCATE( A )
    DEALLOCATE( uice )
    DEALLOCATE( vice )
    DEALLOCATE( ut1 )
    DEALLOCATE( vt1 )
    DEALLOCATE( p )
    DEALLOCATE( etaC )
    DEALLOCATE( etaB )
    DEALLOCATE( zetaC )
    DEALLOCATE( etaCf )
    DEALLOCATE( etaBf )
    DEALLOCATE( zetaCf )
    DEALLOCATE( tracer )
! modd_CB_Dyndim.F90 )
    DEALLOCATE( CdwC1 )
    DEALLOCATE( CdwC2 )
    DEALLOCATE( CdwC1f )
    DEALLOCATE( CdwC2f )
! modd_CB_const.F90 )
    DEALLOCATE( sinlat )
    DEALLOCATE( coslat )
! modd_CB_mask.F90 )
    DEALLOCATE( maskB )
    DEALLOCATE( maskC )
! modd_CB_DynForcing.F90 )
    DEALLOCATE( uwatnd )
    DEALLOCATE( vwatnd )
    DEALLOCATE( speediw )
    DEALLOCATE( R1 )
    DEALLOCATE( R2 )
    DEALLOCATE( R1n )
    DEALLOCATE( R2n )
    DEALLOCATE( bu_ind )
    DEALLOCATE( bv_ind )
    DEALLOCATE( bu )
    DEALLOCATE( bv )
ENDIF
#endif
!
!
!
! 3. Deallocate all tpglt arrays and structures
! =============================================
!
DEALLOCATE( tpglt%bat )
DEALLOCATE( tpglt%dom )  
!
#if ! defined in_surfex
IF ( ndynami==1 .OR. nadvect==1 ) THEN
    DEALLOCATE( tpglt%dxtr )
    DEALLOCATE( tpglt%dytr )
    DEALLOCATE( tpglt%dxtr4 )
    DEALLOCATE( tpglt%dytr4 )
    DEALLOCATE( tpglt%fcor )
    DEALLOCATE( tpglt%tarear )
!
    DEALLOCATE( tpglt%HTN )
    DEALLOCATE( tpglt%HTE )
    DEALLOCATE( tpglt%HTS )
    DEALLOCATE( tpglt%HTW )
    DEALLOCATE( tpglt%hm )
    DEALLOCATE( tpglt%um )
    DEALLOCATE( tpglt%dxta )
    DEALLOCATE( tpglt%dyta )
    DEALLOCATE( tpglt%dxt2r )
    DEALLOCATE( tpglt%dyt2r )
    DEALLOCATE( tpglt%HTNa )
    DEALLOCATE( tpglt%HTEa )
    DEALLOCATE( tpglt%cx )
    DEALLOCATE( tpglt%cy )
    DEALLOCATE( tpglt%cxx )
    DEALLOCATE( tpglt%cxy )
    DEALLOCATE( tpglt%cyy )
    DEALLOCATE( tpglt%cxxx )
    DEALLOCATE( tpglt%cxxy )
    DEALLOCATE( tpglt%cxyy )
    DEALLOCATE( tpglt%cyyy )
!
    DEALLOCATE( tpglt%mne )
    DEALLOCATE( tpglt%mnw )
    DEALLOCATE( tpglt%msw )
    DEALLOCATE( tpglt%mse )
ENDIF
!
! Arrays for McGill's dynamics (all other arrays declared in 3. should
! end up in this part !)
!
IF ( ndynami==2 ) THEN
    DEALLOCATE( tpglt%index2d )
    DEALLOCATE( tpglt%mskice )
ENDIF
#endif
!
DEALLOCATE( tpglt%oce_all )
DEALLOCATE( tpglt%atm_all )
DEALLOCATE( tpglt%all_oce )
!
IF ( nnflxin==0 ) THEN
    DEALLOCATE( tpglt%atm_mix )
    DEALLOCATE( tpglt%mix_atm )
  ELSE
    DEALLOCATE( tpglt%atm_ice )
    DEALLOCATE( tpglt%atm_wat )
    DEALLOCATE( tpglt%ice_atm )
ENDIF
!
IF ( ntd/=0 ) DEALLOCATE( tpglt%sit_d )
!
#if ! defined in_surfex
IF ( ndynami==1 ) THEN
    DEALLOCATE( tpglt%evp ) 
ELSE IF ( ndynami==2 ) THEN
    DEALLOCATE( tpglt%jfn ) 
ENDIF
#endif
!
DEALLOCATE( tpglt%sit )
DEALLOCATE( tpglt%sil ) 
DEALLOCATE( tpglt%tml ) 
!
DEALLOCATE( tpglt%ust )
DEALLOCATE( tpglt%cdia0 )
DEALLOCATE( tpglt%cdia )
DEALLOCATE( tpglt%blkw ) 
DEALLOCATE( tpglt%blki ) 
DEALLOCATE( tpglt%tfl ) 
DEALLOCATE( tpglt%bud )
DEALLOCATE( tpglt%dia ) 
!
!
!
! 4. The end
! ===========
!
IF (lwg) THEN
  WRITE(noutlu,*) ' '
  WRITE(noutlu,*) '  ** LEVEL 3 - END SUBROUTINE DEALLOC'
  WRITE(noutlu,*) ' '
ENDIF
!
END SUBROUTINE gltools_dealloc
!
! ----------------------- END SUBROUTINE gltools_dealloc ------------------------
! -----------------------------------------------------------------------
