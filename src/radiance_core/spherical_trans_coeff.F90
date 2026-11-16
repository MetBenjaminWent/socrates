! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE spherical_trans_coeff_mod
IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'SPHERICAL_TRANS_COEFF_MOD'
CONTAINS
!
! Subroutine to calculate transmission coefficients for spherical geometry.
!
!------------------------------------------------------------------------------
SUBROUTINE spherical_trans_coeff(                                              &
    n_profile, n_layer, n_cloud_top,                                           &
    ss_prop, sph)

  USE realtype_rd,            ONLY: RealK
  USE def_ss_prop,            ONLY: str_ss_prop
  USE def_spherical_geometry, ONLY: StrSphGeo
  USE vectlib_mod,            ONLY: exp_v
  USE parkind1,               ONLY: jprb, jpim
  USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
  USE vernier_timing_mod, ONLY: LPROF_SOCRATES

  IMPLICIT NONE

! Dummy variables.
  INTEGER, INTENT(IN) ::                                                       &
      n_profile,                                                               &
!       Number of profiles
      n_layer,                                                                 &
!       Number of layers
      n_cloud_top
!       Top cloudy layer

! Optical properties
  TYPE(str_ss_prop), INTENT(IN) :: ss_prop
!       Single scattering properties of the atmosphere

  TYPE(StrSphGeo), INTENT(INOUT) :: sph
!       Spherical geometry fields
  

! Local variables
  INTEGER ::                                                                   &
      l, i, ii
!       Loop variables
  REAL (RealK) ::                                                              &
      tau_sph(n_profile),                                                      &
!       (Minus) Optical depth along path through spherical geometry
      tau_sph_layer(n_profile)
!       (Minus) Optical depth for layer using spherical geometry

  INTEGER :: path_base

  INTEGER(KIND=vik) :: vnr_handle_1

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SPHERICAL_TRANS_COEFF'


  IF (LPROF_SOCRATES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

  DO ii=0, n_layer+1
    DO l=1, n_profile
      path_base = MIN(sph%common%path_base(l, ii), n_cloud_top-1)
      tau_sph(l) = SUM( &
        - sph%common%path(l, 1:path_base, ii) &
        * ss_prop%tau_clr(l, 1:path_base) )
    END DO
    CALL exp_v(n_profile, tau_sph, sph%allsky%trans_0(1:n_profile, ii))
    DO i=n_cloud_top, n_layer
      DO l=1, n_profile
        tau_sph_layer(l) = -sph%common%path(l,i,ii)*ss_prop%tau(l,i,0)
        tau_sph(l) = tau_sph(l) + tau_sph_layer(l)
      END DO
      CALL exp_v(n_profile, tau_sph_layer, &
                 sph%common%trans_0_cloud(1:n_profile, i, ii))
    END DO
    IF (n_cloud_top <= n_layer) THEN
      CALL exp_v(n_profile, tau_sph, sph%clear%trans_0(1:n_profile, ii))
    END IF
  END DO
  
  IF (LPROF_SOCRATES) CALL vernier_stop(vnr_handle_1)

END SUBROUTINE spherical_trans_coeff
END MODULE spherical_trans_coeff_mod
