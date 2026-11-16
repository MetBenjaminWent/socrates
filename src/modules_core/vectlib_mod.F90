! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE vectlib_mod

! Description:
!   This routine acts as an interface to vector versions of intrinsic
!   functions on a platform. This currently defaults to a DO loop over
!   the array. Platform specific functions should be added as required.

USE realtype_rd, ONLY: RealK
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_MOCI
USE parkind1, ONLY: jprb, jpim

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VECTLIB_MOD'

CONTAINS

SUBROUTINE exp_v(n,x,y)

  IMPLICIT NONE

! Sets y(i) to the exponential function of x(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: y(n), x(n)

  INTEGER(KIND=vik) :: vnr_handle_1

  CHARACTER(LEN=*), PARAMETER :: RoutineName='EXP_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

  DO i=1, n
    y(i) = EXP(x(i))
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_1)
  RETURN
END SUBROUTINE exp_v

!-----------------------------------------------------------

SUBROUTINE powr_v(n, x, power, z)

  IMPLICIT NONE

! Sets z(i) to x(i) raised to the power y(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: z(n), x(n), y(n), power

  INTEGER(KIND=vik) :: vnr_handle_2

  CHARACTER(LEN=*), PARAMETER :: RoutineName='POWR_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_2, ModuleName//':'//RoutineName)

  DO i=1, n
    y(i)=power
  END DO

  DO i=1, n
    z(i) = x(i)**y(i)
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_2)
  RETURN
END SUBROUTINE powr_v

!-----------------------------------------------------------

SUBROUTINE rtor_v(n, x, y, z)

  IMPLICIT NONE

! Sets z(i) to x(i) raised to the power y(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: z(n), x(n), y(n)

  INTEGER(KIND=vik) :: vnr_handle_3

  CHARACTER(LEN=*), PARAMETER :: RoutineName='RTOR_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_3, ModuleName//':'//RoutineName)

  DO i=1, n
    z(i) = x(i)**y(i)
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_3)
  RETURN
END SUBROUTINE rtor_v

!-----------------------------------------------------------

SUBROUTINE sqrt_v(n, x, y)

  IMPLICIT NONE

! Sets y(i) to the square root of x(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: x(n), y(n)

  INTEGER(KIND=vik) :: vnr_handle_4

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SQRT_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_4, ModuleName//':'//RoutineName)

  DO i=1, n 
    y(i) = SQRT(x(i))
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_4)
  RETURN
END SUBROUTINE sqrt_v

!-----------------------------------------------------------

SUBROUTINE oneover_v(n, x, y)

  IMPLICIT NONE

! Sets y(i) to the reciprocal of x(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: x(n), y(n)

  INTEGER(KIND=vik) :: vnr_handle_5

  CHARACTER(LEN=*), PARAMETER :: RoutineName='ONEOVER_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_5, ModuleName//':'//RoutineName)
  
  DO i=1, n
    y(i) = 1/x(i) 
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_5)
  RETURN
END SUBROUTINE oneover_v

!-----------------------------------------------------------

SUBROUTINE log_v (n, x, y)

  IMPLICIT NONE

! Sets y(i) to the natural logarithm of x(i), for i=1,..,n

  INTEGER :: n, i
  REAL (KIND=RealK) :: x(n), y(n)

  INTEGER(KIND=vik) :: vnr_handle_6

  CHARACTER(LEN=*), PARAMETER :: RoutineName='LOG_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_6, ModuleName//':'//RoutineName)

  DO i=1, n 
    y(i) = LOG(x(i))
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_6)
  RETURN
END SUBROUTINE log_v

!-----------------------------------------------------------

SUBROUTINE sin_v(n,x,y)

  IMPLICIT NONE

! Sets y(i) to the sin function of x(i), for i=1,..,n

  INTEGER :: n, i 
  REAL (KIND=RealK) :: y(n), x(n)

  INTEGER(KIND=vik) :: vnr_handle_7

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SIN_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_7, ModuleName//':'//RoutineName)

  DO i=1, n
    y(i) = SIN(x(i)) 
  END DO
  
  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_7)
  RETURN
END SUBROUTINE sin_v

!-----------------------------------------------------------

SUBROUTINE cos_v(n,x,y)

  IMPLICIT NONE

! Sets y(i) to the cos function of x(i), for i=1,..,n

  INTEGER :: n, i 
  REAL (KIND=RealK) :: y(n), x(n)

  INTEGER(KIND=vik) :: vnr_handle_8

  CHARACTER(LEN=*), PARAMETER :: RoutineName='COS_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_8, ModuleName//':'//RoutineName)

  DO i=1, n 
    y(i) = COS(x(i)) 
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_8)
  RETURN
END SUBROUTINE cos_v

!-----------------------------------------------------------

SUBROUTINE asin_v(n,x,y)

  IMPLICIT NONE

! Sets y(i) to the asin function of x(i), for i=1,..,n

  INTEGER :: n, i 
  REAL (KIND=RealK) :: y(n), x(n)

  INTEGER(KIND=vik) :: vnr_handle_9

  CHARACTER(LEN=*), PARAMETER :: RoutineName='ASIN_V'

  IF (LPROF_MOCI) CALL vernier_start(vnr_handle_9, ModuleName//':'//RoutineName)

  DO i=1, n
    y(i) = ASIN(x(i)) 
  END DO

  IF (LPROF_MOCI) CALL vernier_stop(vnr_handle_9)
  RETURN
END SUBROUTINE asin_v

!-----------------------------------------------------------

END MODULE vectlib_mod
