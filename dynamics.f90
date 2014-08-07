program dynamics
use types, only: dp
use sim_properties, only: dt, t_print
use airplane, only: forces, torques, mass
use euler_rotate, only: inert2body, body2inert
use dot_cross, only: dot, cross
implicit none
real(dp), parameter :: gravity = 9.80665_dp ! m / s^2
!principle moments of inertia along x, y, z body axes
real(dp), parameter :: inertia(3) = (/1.0_dp,1.0_dp,2.0_dp/)
!inertial reference frame (x-forward, y-right, z-down at no rotation)
real(dp) :: x(3)=(/0,0,0/), v(3)=(/6,0,0/), a(3)=(/0,0,0/)
!body frame (x'''-forward, y'''-right, z'''-down)
real(dp) :: vbody(3), abody(3)
!yaw, pitch, roll (right-handed, rotated through x-y'-z'')
real(dp) :: ang(3)=(/0,0,0/), ang_v(3)=(/0,0,0/), ang_a(3)=(/0,0,0/)
!angular momentum vector (I . w)
real(dp) :: ang_mom(3)
!term for euler rigid body equations (M - w x (I . w))
real(dp) :: euler_term(3)
real(dp) :: t = 0
integer, parameter :: print_loops = nint(t_print/dt)
integer :: i = 0
character(len=*),parameter::fmt1="(f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2)"
!todo: use rk4 instead of Euler's method
do while (t<7200.0_dp)
    t = t + dt
    vbody(:) = inert2body(v, ang)
    abody(:) = forces(vbody)/mass
    ang_mom(:) = inertia(:)*ang_v(:)
    euler_term(:) = torques(x, v, vbody, ang, ang_v)-cross(ang_v, ang_mom)
    ang_a(:) = euler_term(:)/inertia(:)
    ang_v(:) = ang_v(:) + ang_a(:)*dt
    ang(:) = ang(:) + ang_v(:)*dt
    a(:) = body2inert(abody, ang)+(/0.0_dp,0.0_dp,gravity/)
    v(:) = v(:) + a(:)*dt
    x(:) = x(:) + v(:)*dt
    i = i + 1
    if (mod(i,print_loops) .EQ. 0) then
        write(*,fmt1) t, x(1), x(2), x(3), v(1), v(2), v(3), ang(1)*180/3.14, ang(2)*180/3.14, atan2(vbody(3), vbody(1))*180/3.14
    end if
end do
end program
