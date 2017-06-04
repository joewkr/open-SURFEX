SUBROUTINE UNCOMPRESS_FIELD(KLONG,PSEUIL,PFIELD_IN,PFIELD_OUT)

IMPLICIT NONE
 
INTEGER*4, INTENT(IN) :: KLONG
REAL, INTENT(IN) :: PSEUIL
REAL, DIMENSION(:), INTENT(IN) :: PFIELD_IN
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
INTEGER :: ICPT, I, K

ICPT = 0

I = 1

PFIELD_OUT(:) = 0.
!
! boucle sur les colonnes
DO 
           
  ! si on a dépassé la dernière colonne, on sort de la boucle
  IF (ICPT>=KLONG) EXIT

  ! si la valeur est valide
  IF (PFIELD_IN(I)<PSEUIL) THEN

    ! on la met dans lwrite à l'indice icpt
    ICPT = ICPT + 1
    PFIELD_OUT(ICPT) = PFIELD_IN(I)
!!!!!!!!!!!!!!!!!!!!test temporary: to remove after
    IF (MOD(PFIELD_OUT(ICPT),100.)==0) PFIELD_OUT(ICPT)=0.
!!!!!!!!!!!!!!!!!!!!!! to remove after
    ! on incrémente i
    I = I+1

  ELSE

    !ideb = icpt + 1
    DO K = 1,NINT(PFIELD_IN(I)-(PSEUIL))
      ICPT = ICPT + 1
      IF (ICPT>KLONG) EXIT
      PFIELD_OUT(ICPT) = 0.
    ENDDO

    I = I+1

  ENDIF

ENDDO

END SUBROUTINE UNCOMPRESS_FIELD
