module airplane
use types, only: dp
use pid, only: pid_loop, pid_output
use constants, only: rho, s, cl_0, cd_0, k, mass
implicit none
private
public aero_forces, control_forces, control_torques, mass
real(dp) :: pi = 3.1415926535897931_dp
contains
function control_forces(vbody) result(f)
    real(dp), intent(in) :: vbody(3)
    real(dp) :: f(3)
    !axial/longitudinal force, positive -> forward
    f(1) = thrust(vbody(1))
    !lateral force, positive -> right
    f(2) = 0
    !normal force, positive -> down
    f(3) = 0
end function
function aero_forces(vbody) result(f)
    real(dp), intent(in) :: vbody(3)
    real(dp) :: alpha, d, l
    real(dp) :: f(3)
    alpha = atan2(vbody(3),vbody(1))
    d = drag(vbody(1), alpha)
    l = lift(vbody(1), alpha)
    !axial/longitudinal force, positive -> forward
    f(1) = -d*cos(alpha) + l*sin(alpha)
    !lateral force, positive -> right
    f(2) = 0 !zero for no sideslip
    !normal force, positive -> down
    f(3) = -d*sin(alpha) - l*cos(alpha)
end function
function lift(v, alpha)
    real(dp), intent(in) :: v, alpha
    real(dp) :: lift
    lift = coeff_l(alpha)*(.5_dp * rho * (v**2) * s)
end function
function drag(v, alpha)
    real(dp), intent(in) :: v, alpha
    real(dp) :: drag
    drag = coeff_d(alpha)*(.5_dp * rho * (v**2) * s)
end function
function thrust(v)
    real(dp), intent(in) :: v
    real(dp) :: thrust
    type(pid_loop), save :: pid_v = pid_loop(4.0_dp, 1.0_dp, 0.0_dp)
    thrust = pid_output(pid_v, 6.3_dp, v)
end function
function coeff_l(alpha)
    real(dp), intent(in) :: alpha
    real(dp) :: coeff_l
    coeff_l = cl_0 + alpha * 2 * pi
end function
function coeff_d(alpha)
    real(dp), intent(in) :: alpha
    real(dp) :: coeff_d
    coeff_d = cd_0 + k*(coeff_l(alpha) - cl_0)**2
end function
function control_torques(x, v, vbody, ang, ang_v) result(m)
    real(dp), intent(in) :: x(3), v(3), vbody(3), ang(3), ang_v(3)
    real(dp) :: pitch_setpoint
    real(dp) :: m(3)
    type(pid_loop), save :: pid_roll = pid_loop(1.0_dp, 0.0_dp, 1.0_dp)
    type(pid_loop), save :: pid_alt = pid_loop(-.05_dp, 100.0_dp, 0.0_dp)
    type(pid_loop), save :: pid_pitch = pid_loop(1.0_dp, 0.0_dp, 1.0_dp)
    type(pid_loop), save :: pid_yaw = pid_loop(1.0_dp, 0.0_dp, 0.0_dp)
    m(1) = pid_output(pid_roll, 0.0_dp, ang(1))
    pitch_setpoint = pid_output(pid_alt, 0.0_dp, x(3)) !cascaded pid loop, outer loop = altitude, inner = pitch
    m(2) = pid_output(pid_pitch, pitch_setpoint, ang(2))
    m(3) = pid_output(pid_yaw, 0.0_dp, ang(3))
end function
end module
