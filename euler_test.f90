program euler_test
use types, only: dp
use euler_rotate, only: inert2body, body2inert
implicit none
real(dp) :: input_vector(3), body_vector(3), inert_vector(3)
real(dp) :: ang(3)
write(*,*) "Input the vector to be rotated"
read(*,*) input_vector(1), input_vector(2), input_vector(3)
write(*,*) "Input the angles to be rotated through"
read(*,*) ang(1), ang(2), ang(3)
body_vector(:) = inert2body(input_vector, ang)
inert_vector(:) = body2inert(body_vector, ang)
write(*,*) "Convert input to body frame and back to inertial"
write(*,*) body_vector(1), body_vector(2), body_vector(3)
write(*,*) inert_vector(1), inert_vector(2), inert_vector(3)
inert_vector(:) = body2inert(input_vector, ang)
body_vector(:) = inert2body(inert_vector, ang)
write(*,*) "Convert input to inertial frame and back to body"
write(*,*) inert_vector(1), inert_vector(2), inert_vector(3)
write(*,*) body_vector(1), body_vector(2), body_vector(3)
end program
