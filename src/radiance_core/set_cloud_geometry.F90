! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to set geometry of clouds.
!
! Method:
!   For use in multi-column mode arrays are set for each layer
!   pointing to profiles which have non-negligible clear or
!   cloudy fractions. The topmost cloudy layers are also
!   detected.
!
!- ---------------------------------------------------------------------
MODULE set_cloud_geometry_mod
IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'SET_CLOUD_GEOMETRY_MOD'
CONTAINS
SUBROUTINE set_cloud_geometry(n_profile, n_layer                        &
    , l_global_cloud_top, w_cloud                                       &
    , n_cloud_top, n_cloud_profile, i_cloud_profile                     &
    , nd_profile, nd_layer, id_ct                                       &
    )


  USE realtype_rd, ONLY: RealK
  USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
  USE vernier_timing_mod, ONLY: lprof_socrates
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Maximum number of profiles
    , nd_layer                                                          &
!       Maximum number of layers
    , id_ct
!       Topmost declared cloudy layer


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of atmospheric profiles
    , n_layer
!       Number of layers

  LOGICAL, INTENT(IN) ::                                                &
      l_global_cloud_top
!       Flag to use a global value for the topmost cloudy layer
  REAL (RealK), INTENT(IN) ::                                           &
      w_cloud(nd_profile, id_ct: nd_layer)
!       Amounts of cloud

  INTEGER, INTENT(OUT) ::                                               &
      n_cloud_top                                                       &
!       Topmost cloudy layer
    , n_cloud_profile(id_ct: nd_layer)                                  &
!       Number of cloudy profiles
    , i_cloud_profile(nd_profile, id_ct: nd_layer)
!       Profiles containing clouds


! Local variables.
  INTEGER                                                               &
      l                                                                 &
!       Loop variable
    , i
!       Loop variable

  INTEGER(KIND=vik) :: vnr_handle_1

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SET_CLOUD_GEOMETRY'


  IF (lprof_socrates) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

  DO i=id_ct, n_layer
    n_cloud_profile(i)=0
    DO l=1, n_profile
      IF (w_cloud(l, i) >  0.0e+00_RealK) THEN
        n_cloud_profile(i)=n_cloud_profile(i)+1
        i_cloud_profile(n_cloud_profile(i), i)=l
      END IF
    END DO
  END DO

  IF (l_global_cloud_top) THEN
    n_cloud_top=id_ct
  ELSE
    n_cloud_top=id_ct
    DO WHILE ( (n_cloud_top <  n_layer).AND.                            &
               (n_cloud_profile(n_cloud_top) == 0) )
      n_cloud_top=n_cloud_top+1
    END DO
  END IF


  IF (lprof_socrates) CALL vernier_stop(vnr_handle_1)

END SUBROUTINE set_cloud_geometry
END MODULE set_cloud_geometry_mod
