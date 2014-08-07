module dot_cross
use types, only: dp
implicit none
private
public dot, cross
contains
function dot(u, v) result(s)
    real(dp), intent(in) :: u(3), v(3)
    real(dp) :: s
    s = u(1)*v(1)+u(2)*v(2)+u(3)*v(3)
end function
function cross(u, v) result(s)
    real(dp), intent(in) :: u(3), v(3)
    real(dp) :: s(3)
    s(1) = u(2)*v(3)-u(3)*v(2)
    s(2) = u(3)*v(1)-u(1)*v(3)
    s(3) = u(1)*v(2)-u(2)*v(1)
end function
end module


