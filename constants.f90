module constants
use types, only: dp
implicit none
private
public rho, s, mass, cd_0, cl_0, k
!atmospheric properties
real(dp), parameter :: rho = 1.225_dp ! kg / m^3
!aircraft properties
real(dp), parameter :: mass = 1.0_dp ! kg (mass of airplane)
real(dp), parameter :: s = 1.0_dp ! m^2 (planform area of wings)
real(dp), parameter :: cd_0 = .017_dp ! (drag coefficient of aircraft)
real(dp), parameter :: cl_0 = .0_dp ! (drag coefficient of aircraft)
real(dp), parameter :: k = .075_dp ! (drag coefficient of aircraft)
end module
