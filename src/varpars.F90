module varpars
    use, intrinsic :: iso_fortran_env, only: real32,&
                                            &real64,&
                                            &input_unit,&
                                            &error_unit,&
                                            &output_unit
    use, intrinsic :: iso_c_binding, only: c_float, c_double
    implicit none
    public

#ifdef USE_DOUBLE
    integer, parameter :: c_rk = c_double
    integer, parameter :: rk = real64
#else
    integer, parameter :: c_rk = c_float
    integer, parameter :: rk = real32
#endif
    integer, parameter :: r4 = real32
    integer, parameter :: r8 = real64

    integer, parameter :: filename_len = 128
    integer, parameter :: line_len = 1024

    integer, parameter :: dimn = 3

    character(len=*), parameter :: mode_read = 'r'
    character(len=*), parameter :: mode_write = 'w'

    interface num2str
        module procedure int2str, real2str
    end interface

contains
    function int2str(i) result(str)
        implicit none
        character(len=line_len) :: str
        integer, intent(in) :: i

        write(str, *) i
    end function

    function real2str(r) result(str)
        implicit none
        character(len=line_len) :: str
        real(rk), intent(in) :: r

        write(str, *) r
    end function
end module
