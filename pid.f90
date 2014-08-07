module pid
    use types, only: dp
    use sim_properties, only: dt
    implicit none
    private
    public pid_loop, pid_output
    type pid_loop
        real(dp) :: kp, ti, td
        real(dp) :: cum_error = 0
        real(dp) :: last_pv = 0
    end type pid_loop
contains
    function pid_output(this, setpoint, pv) result(output)
        type(pid_loop), intent(inout) :: this
        real(dp), intent(in) :: setpoint, pv
        real(dp) :: error, output
        error = setpoint - pv
        this%cum_error = this%cum_error + error
        if (this%ti .EQ. 0) then
            output = this%kp * (error + this%td * (this%last_pv - pv) / dt)
        else
            output = this%kp * (error + this%cum_error * dt / this%ti + this%td * (this%last_pv - pv) / dt)
        end if
        this%last_pv = pv
    end function
end module
