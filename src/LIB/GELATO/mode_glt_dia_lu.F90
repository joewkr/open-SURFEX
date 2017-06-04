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
MODULE mode_glt_dia_lu 
!INTERFACE
!
!SUBROUTINE opndia
!END SUBROUTINE opndia
!
!SUBROUTINE clsdia
!END SUBROUTINE clsdia
!!
!END INTERFACE
!END MODULE mode_glt_dia_lu
!
CONTAINS
!
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE OPNDIA --------------------------
!
SUBROUTINE OPNDIA()
!
  USE modd_glt_param
  USE modd_types_glt
  IMPLICIT NONE
!
  CHARACTER(80) ::  &
        yfname
!
!
! .. Welcome message
!
  IF (lwg) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE OPNDIA'
    WRITE(noutlu,*) ' '
  ENDIF
!
! .. Open diagnostics files. 
!
  IF ( lwg ) THEN
      SELECT CASE ( TRIM(cdiafmt) )
        CASE( 'GELATO' )
          IF ( ndiap1==1 .OR. ndiap2==1 ) THEN
              IF ( ninsdia==1 ) THEN
                  yfname = '2d.ins.vairmer'
                  OPEN(UNIT=n2vilu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
              IF ( navedia==1 ) THEN
                  yfname = '2d.ave.vairmer'
                  OPEN(UNIT=n2valu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
          ENDIF
          IF ( ndiap3==1 ) THEN
              yfname = '0d.ins.vairmer'
              OPEN(UNIT=n0vilu,FILE=TRIM(yfname),FORM='UNFORMATTED')
          ENDIF
        CASE( 'VMAR5' )
          IF ( ndiap1==1 .OR. ndiap2==1 .OR. ndiap3==1 ) THEN
              IF ( ninsdia==1 ) THEN
                  yfname = '2d.ins.ar5'
                  OPEN(UNIT=n2vilu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
              IF ( navedia==1 ) THEN
                  yfname = '2d.ave.ar5'
                  OPEN(UNIT=n2valu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
          ENDIF
          IF ( ndiap1==1 ) THEN
              IF ( ninsdia==1 ) THEN
                  yfname = '0d.ins.ar5'
                  OPEN(UNIT=n0vilu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF
              IF ( navedia==1 ) THEN
                  yfname = '0d.ave.ar5'
                  OPEN(UNIT=n0valu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF
          ENDIF
          IF ( ndiapx==1 ) THEN
              IF ( ninsdia==1 ) THEN
                  yfname = '2d.ins.x'
                  OPEN(UNIT=nxvilu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
              IF ( navedia==1 ) THEN
                  yfname = '2d.ave.x'
                  OPEN(UNIT=nxvalu,FILE=TRIM(yfname),FORM='UNFORMATTED')
              ENDIF 
          ENDIF
      END SELECT
  ENDIF
!
! .. Farewell message
!
  IF (lp1) THEN
    WRITE(noutlu,*) ' '
    WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE OPNDIA'
    WRITE(noutlu,*) ' '
  ENDIF
!
END SUBROUTINE OPNDIA
!
! ------------------------ END SUBROUTINE OPNDIA ------------------------
! -----------------------------------------------------------------------
!
!
! -----------------------------------------------------------------------
! -------------------------- SUBROUTINE CLSDIA --------------------------
!
SUBROUTINE CLSDIA()
!
  USE modd_glt_param
  IMPLICIT NONE
!
!
! .. Welcome message
!
  if (lp1) WRITE(noutlu,*) ' '
  if (lp1) WRITE(noutlu,*) ' *** LEVEL 3 - SUBROUTINE CLSDIA'
  if (lp1) WRITE(noutlu,*) ' '
!
! ..Close diagnostic files.
!
  IF ( lwg ) THEN
      SELECT CASE ( TRIM(cdiafmt) )
        CASE( 'GELATO' )
          IF ( ndiap1==1 .OR. ndiap2==1 ) THEN
              IF ( ninsdia==1 ) CLOSE(n2vilu)
              IF ( navedia==1 ) CLOSE(n2valu)
          ENDIF
          IF ( ndiap3==1 ) CLOSE(n0vilu)
        CASE( 'VMAR5' )
          IF ( ndiap1==1 .OR. ndiap2==1 .OR. ndiap3==1 ) THEN
              IF ( ninsdia==1 ) CLOSE(n2vilu)
              IF ( navedia==1 ) CLOSE(n2valu)
          ENDIF 
          IF ( ndiapx==1 ) THEN
              IF ( ninsdia==1 ) CLOSE(nxvilu)
              IF ( navedia==1 ) CLOSE(nxvalu)
          ENDIF
      END SELECT
  ENDIF
!
! .. Farewell message
!
  if (lp1) WRITE(noutlu,*) ' '
  if (lp1) WRITE(noutlu,*) ' *** LEVEL 3 - END SUBROUTINE CLSDIA'
  if (lp1) WRITE(noutlu,*) ' '
!
END SUBROUTINE CLSDIA
!
! ------------------------ END SUBROUTINE CLSDIA ------------------------
! -----------------------------------------------------------------------
END MODULE mode_glt_dia_lu
! ---------------------- END MODULE mode_glt_dia_lu -------------------------
