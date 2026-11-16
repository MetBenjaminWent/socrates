! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE set_n_source_coeff_mod
IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SET_N_SOURCE_COEFF_MOD'
CONTAINS

! Function to set number of source coefficients.
!
! Method:
!   The two-stream approximation is examined and the number
!   of coefficients is set accordingly.
!- ---------------------------------------------------------------------
FUNCTION set_n_source_coeff(isolir, l_ir_source_quad)

  USE rad_pcf,  ONLY: ip_solar
  USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
  USE vernier_timing_mod, ONLY: LPROF_MOCI
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


  INTEGER, INTENT(IN) :: isolir
!   Spectral region
  LOGICAL, INTENT(IN) :: l_ir_source_quad
!   Flag for quadratic infra-red source

  INTEGER :: set_n_source_coeff
!   Returned number of source coefficients

  INTEGER(KIND=vik) :: vnr_handle_1

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SET_N_SOURCE_COEFF'


  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

  IF (isolir == ip_solar) THEN
    set_n_source_coeff=2
  ELSE
    IF (l_ir_source_quad) THEN
      set_n_source_coeff=2
    ELSE
      set_n_source_coeff=1
    END IF
  END IF

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_1)

END FUNCTION set_n_source_coeff
END MODULE set_n_source_coeff_mod
