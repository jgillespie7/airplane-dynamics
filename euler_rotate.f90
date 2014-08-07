module euler_rotate
use types, only: dp
implicit none
private
public inert2body, body2inert
contains
function inert2body(inert, euler) result(body)
    real(dp), intent(in) :: inert(3), euler(3)
    real(dp) :: c(3), s(3)
    real(dp) :: z_gamma(3,3), y_beta(3,3), x_alpha(3,3)
    real(dp) :: body(3)
    c(:) = cos(euler(:))
    s(:) = sin(euler(:))
    z_gamma(1,1) = c(1)
    z_gamma(1,2) = -s(1)
    z_gamma(1,3) = 0
    z_gamma(2,1) = s(1)
    z_gamma(2,2) = c(1)
    z_gamma(2,3) = 0
    z_gamma(3,1) = 0
    z_gamma(3,2) = 0
    z_gamma(3,3) = 1
    y_beta(1,1) = c(2)
    y_beta(1,2) = 0
    y_beta(1,3) = s(2)
    y_beta(2,1) = 0
    y_beta(2,2) = 1
    y_beta(2,3) = 0
    y_beta(3,1) = -s(2)
    y_beta(3,2) = 0
    y_beta(3,3) = c(2)
    x_alpha(1,1) = 1
    x_alpha(1,2) = 0
    x_alpha(1,3) = 0
    x_alpha(2,1) = 0
    x_alpha(2,2) = c(3)
    x_alpha(2,3) = -s(3)
    x_alpha(3,1) = 0
    x_alpha(3,2) = s(3)
    x_alpha(3,3) = c(3)
    body(:) = matmul(matmul(matmul(inert,z_gamma),y_beta),x_alpha)
end function
function body2inert(body, euler) result(inert)
    real(dp), intent(in) :: body(3), euler(3)
    real(dp) :: c(3), s(3)
    real(dp) :: z_gamma(3,3), y_beta(3,3), x_alpha(3,3)
    real(dp) :: inert(3)
    c(:) = cos(euler(:))
    s(:) = sin(euler(:))
    z_gamma(1,1) = c(1)
    z_gamma(1,2) = s(1)
    z_gamma(1,3) = 0
    z_gamma(2,1) = -s(1)
    z_gamma(2,2) = c(1)
    z_gamma(2,3) = 0
    z_gamma(3,1) = 0
    z_gamma(3,2) = 0
    z_gamma(3,3) = 1
    y_beta(1,1) = c(2)
    y_beta(1,2) = 0
    y_beta(1,3) = -s(2)
    y_beta(2,1) = 0
    y_beta(2,2) = 1
    y_beta(2,3) = 0
    y_beta(3,1) = s(2)
    y_beta(3,2) = 0
    y_beta(3,3) = c(2)
    x_alpha(1,1) = 1
    x_alpha(1,2) = 0
    x_alpha(1,3) = 0
    x_alpha(2,1) = 0
    x_alpha(2,2) = c(3)
    x_alpha(2,3) = s(3)
    x_alpha(3,1) = 0
    x_alpha(3,2) = -s(3)
    x_alpha(3,3) = c(3)
    inert(:) = matmul(matmul(matmul(body,x_alpha),y_beta),z_gamma)
end function
end module
