program dynamics
use types, only: dp
use sim_properties, only: dt, t_print
use airplane, only: aero_forces, control_forces, control_torques, mass
use euler_rotate, only: inert2body, body2inert
use dot_cross, only: dot, cross
implicit none
real(dp), parameter :: gravity = 9.80665_dp ! m / s^2
!principle moments of inertia along x, y, z body axes
real(dp), parameter :: inertia(3) = (/1.0_dp,1.0_dp,2.0_dp/)
!inertial reference frame (x-forward, y-right, z-down at no rotation)
real(dp) :: x(3) = (/0,0,0/)
real(dp) :: v(3) = (/6,0,0/)
real(dp) :: a(3) = (/0,0,0/)
!body frame (x'''-forward, y'''-right, z'''-down)
real(dp) :: vbody(3), abody(3)
!yaw, pitch, roll (right-handed, rotated through x-y'-z'')
real(dp) :: ang(3) = (/0,0,0/)
real(dp) :: ang_v(3) = (/0,0,0/)
real(dp) :: ang_a(3) = (/0,0,0/)
!angular momentum vector (I . w)
real(dp) :: ang_mom(3)
!force and torque from control surfaces (updated once per time step)
real(dp) :: control_force(3)
real(dp) :: torque(3)
!term for euler rigid body equations (M - w x (I . w))
real(dp) :: euler_term(3)
real(dp) :: t = 0
!RK4
real(dp) :: k(3,4,4) !k1 through k4 for ang_v, ang, v, x in that order
integer, parameter :: print_loops = nint(t_print/dt)
integer :: i = 0
character(len=*),parameter::fmt1="(f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2,1x,f7.2)"
do while (t<600.0_dp)
    t = t + dt
    !vbody for control and k1
    vbody(:) = inert2body(v(:), ang(:))
    !calculate control inputs
    control_force = control_forces(vbody(:))
    torque(:) = control_torques(x(:), v(:), vbody(:), ang(:), ang_v(:))
    !k1
    abody(:) = (aero_forces(vbody(:))+control_force)/mass
    ang_mom(:) = inertia(:)*ang_v(:)
    euler_term(:) = torque(:) - cross(ang_v(:), ang_mom(:))
    ang_a(:) = euler_term(:)/inertia(:)
    a(:) = body2inert(abody(:), ang(:))+(/0.0_dp,0.0_dp,gravity/)
    k(:,1,1) = ang_a(:)*dt
    k(:,2,1) = ang_v(:)*dt
    k(:,3,1) = a(:)*dt
    k(:,4,1) = v(:)*dt
    !k2
    vbody(:) = inert2body( (v(:)+.5_dp*k(:,3,1)), (ang(:)+.5_dp*k(:,2,1)))
    abody(:) = (aero_forces(vbody(:))+control_force)/mass
    ang_mom(:) = inertia(:)*(ang_v(:)+.5_dp*k(:,1,1))
    euler_term(:) = torque(:) - cross((ang_v(:)+.5_dp*k(:,1,1)), ang_mom(:))
    ang_a(:) = euler_term(:)/inertia(:)
    a(:) = body2inert(abody(:), (ang(:)+.5_dp*k(:,2,1)))+(/0.0_dp,0.0_dp,gravity/)
    k(:,1,2) = ang_a(:)*dt
    k(:,2,2) = (ang_v(:)+.5_dp*k(:,1,1))*dt
    k(:,3,2) = a(:)*dt
    k(:,4,2) = (v(:)+.5_dp*k(:,3,1))*dt
    !k3
    vbody(:) = inert2body( (v(:)+.5_dp*k(:,3,2)), (ang(:)+.5_dp*k(:,2,2)))
    abody(:) = (aero_forces(vbody(:))+control_force)/mass
    ang_mom(:) = inertia(:)*(ang_v(:)+.5_dp*k(:,1,2))
    euler_term(:) = torque(:) - cross((ang_v(:)+.5_dp*k(:,1,2)), ang_mom(:))
    ang_a(:) = euler_term(:)/inertia(:)
    a(:) = body2inert(abody(:), (ang(:)+.5_dp*k(:,2,2)))+(/0.0_dp,0.0_dp,gravity/)
    k(:,1,3) = ang_a(:)*dt
    k(:,2,3) = (ang_v(:)+.5_dp*k(:,1,2))*dt
    k(:,3,3) = a(:)*dt
    k(:,4,3) = (v(:)+.5_dp*k(:,3,2))*dt
    !k4
    vbody(:) = inert2body( (v(:)+k(:,3,3)), (ang(:)+k(:,2,3)))
    abody(:) = (aero_forces(vbody(:))+control_force)/mass
    ang_mom(:) = inertia(:)*(ang_v(:)+k(:,1,3))
    euler_term(:) = torque(:) - cross((ang_v(:)+k(:,1,3)), ang_mom(:))
    ang_a(:) = euler_term(:)/inertia(:)
    a(:) = body2inert(abody(:), (ang(:)+k(:,2,3)))+(/0.0_dp,0.0_dp,gravity/)
    k(:,1,4) = ang_a(:)*dt
    k(:,2,4) = (ang_v(:)+k(:,1,3))*dt
    k(:,3,4) = a(:)*dt
    k(:,4,4) = (v(:)+k(:,3,3))*dt
    !calculate changes based on k1 through k4
    ang_v(:) = ang_v(:) + (k(:,1,1) + 2*k(:,1,2) + 2*k(:,1,3) + k(:,1,4))/6
    ang(:) = ang(:) + (k(:,2,1) + 2*k(:,2,2) + 2*k(:,2,3) + k(:,2,4))/6
    v(:) = v(:) + (k(:,3,1) + 2*k(:,3,2) + 2*k(:,3,3) + k(:,3,4))/6
    x(:) = x(:) + (k(:,4,1) + 2*k(:,4,2) + 2*k(:,4,3) + k(:,4,4))/6

    i = i + 1
    if (mod(i,print_loops) .EQ. 0) then
        write(*,fmt1) t, x(1), x(2), x(3), v(1), v(2), v(3), ang(1)*180/3.14, ang(2)*180/3.14
    end if
end do
end program
