module sim_properties
use types, only: dp
implicit none
private
public dt, t_print
real(dp), parameter :: dt = .001_dp
real(dp), parameter :: t_print = 10.0_dp
end module
