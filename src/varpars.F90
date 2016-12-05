module varpars
    use, intrinsic :: iso_fortran_env, only: real32,&
                                            &real64,&
                                            &input_unit,&
                                            &error_unit,&
                                            &output_unit
    implicit none
    public

#ifdef XDR_DOUBLE
    integer, parameter :: rk = real64
#else
    integer, parameter :: rk = real32
#endif
    integer, parameter :: r4 = real32
    integer, parameter :: r8 = real64
end module
