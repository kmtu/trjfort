! XDR Fortran Interface with Wrappers
! 2014 (c) James W. Barnett <jbarnet4@tulane.edu>
! https://github.com/wesbarnett/

! Modified by Kai-Min Tu (2014)
! Based on Barnett's work, the interface for TRR file is incorporated
! https://github.com/kmtu/

module xdr
    use varpars, only: rk, c_rk, r4, filename_len, dimn, mode_read, error_unit
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_float, c_int
    implicit none
    private

    type, abstract :: trjfile
        character(len=filename_len) :: file
        type(xdrfile), pointer :: xd
        character :: mode
        integer(c_int) :: natoms, step, stat
    contains
        procedure :: init => init_xdr
        procedure :: close => close_xdr
    end type

    ! *** xtcfile type
    ! box     - triclinic pbc box of the configuration
    ! natoms  - number of atoms in the configuration.
    ! pos     - positions read in (3, natoms)
    ! prec    - precision of the coordinates read in
    ! step    - step number of configuration.
    ! stat    - status of operation. 0 = good
    ! time    - time of the configuration
    ! xd      - pointer from libxdrfile.

    ! should always call init first. then call read in a loop and do your
    ! calculations. after the loops call close.
    type, extends(trjfile), public :: xtcfile
        real(c_float) :: box(dimn, dimn), time
        real(c_float), allocatable :: pos(:, :)
        real(c_float) :: prec
    contains
        procedure :: read => read_xtcfile
        procedure :: write => write_xtcfile
    end type

    ! *** trrfile type
    ! box     - triclinic pbc box of the configuration
    ! natoms  - number of atoms in the configuration.
    ! pos     - positions read in (3, natoms)
    ! vel     - velocities read in (3, natoms)
    ! frc     - forces read in (3, natoms)
    ! lambda  - lambda value for free energy perturbation calculations
    ! step    - step number of configuration.
    ! stat    - status of operation. 0 = good
    ! time    - time of the configuration
    ! xd      - pointer from libxdrfile.
    type, extends(trjfile), public :: trrfile
        real(c_rk) :: box(dimn, dimn), time
        real(c_rk), allocatable :: pos(:, :), vel(:, :), frc(:, :)
        real(c_rk) :: lambda
    contains
        procedure :: read => read_trrfile
        procedure :: write => write_trrfile
    end type

    ! the data type located in libxdrfile
    type, bind(c) :: xdrfile
        type(c_ptr) :: fp, xdr
        character(c_char) :: mode
        type(c_ptr) :: buf1, buf2
        integer(c_int) :: buf1size, buf2size
    end type

    ! interface with libxdrfile
    interface 
        type(c_ptr) function xdrfile_open(filename, mode) bind(c, name='xdrfile_open')
            import
            character(c_char), intent(in) :: filename(*), mode(*)
        end function

        integer(c_int) function xdrfile_close(xd) bind(c, name='xdrfile_close')
            import
            type(xdrfile), intent(in) :: xd
        end function

        ! xtc
        integer(c_int) function read_xtc_natoms(filename, natoms) bind(c)
            import
            character(c_char), intent(in) :: filename
            integer(c_int), intent(out) :: natoms
        end function

        integer(c_int) function read_xtc(xd, natoms, step, time, box, x, prec) bind(c)
            import
            type(xdrfile), intent(in) :: xd
            integer(c_int), intent(in), value :: natoms
            integer(c_int), intent(out) :: step
            real(c_float), intent(out) :: time, prec, box(*), x(*)
        end function

        integer(c_int) function write_xtc(xd, natoms, step, time, box, x, prec) bind(c)
            import
            type(xdrfile), intent(in) :: xd
            integer(c_int), intent(in), value :: natoms, step
            real(c_float), intent(in), value :: time, prec
            real(c_float), intent(in) :: box(*), x(*)
        end function

        ! trr
        integer(c_int) function read_trr_natoms(filename, natoms) bind(c)
            import
            character(c_char), intent(in) :: filename
            integer(c_int), intent(out) :: natoms
        end function

        integer(c_int) function read_trr(xd, natoms, step, time, lambda, box, x, v, f) bind(c)
            import
            type(xdrfile), intent(in) :: xd
            integer(c_int), intent(in), value :: natoms
            integer(c_int), intent(out) :: step
            real(c_rk), intent(out) :: time, lambda
            real(c_rk), intent(out), optional :: box(*), x(*), v(*), f(*)
        end function

        integer(c_int) function write_trr(xd, natoms, step, time, lambda, box, x, v, f) bind(c)
            import
            type(xdrfile), intent(in) :: xd
            integer(c_int), intent(in), value :: natoms, step
            real(c_rk), intent(in), value :: time, lambda
            real(c_rk), intent(in), optional :: box(*), x(*), v(*), f(*)
        end function

    end interface

contains
    ! our wrappers for the trjfile class
    subroutine init_xdr(this, filename_in, mode_opt)
        use, intrinsic :: iso_c_binding, only: c_null_char, c_f_pointer
        implicit none
        class(trjfile), intent(inout) :: this
        type(c_ptr) :: xd_c
        character (len=*), intent(in) :: filename_in
        character (len=filename_len) :: filename
        logical :: ex
        character, optional, intent(in) :: mode_opt

        this%file = filename_in
        if (present(mode_opt)) then
            this%mode = mode_opt
        else
            this%mode = mode_read
        end if

        ! set the file name to be read in for c.
        filename = trim(filename_in)//c_null_char

        if (this%mode == mode_read) then
            inquire(file=trim(filename_in), exist=ex)

            if (.not. ex) then
                write(error_unit, *)
                write(error_unit, '(a)') " error: "//trim(filename_in)//" does not exist."
                write(error_unit, *)
                stop
            end if

            select type (this)
            type is (xtcfile)
                ! get number of atoms in system and allocate position array.
                this%stat = read_xtc_natoms(filename, this%natoms)

                if (this%stat /= 0) then
                    write(error_unit, *)
                    write(error_unit, '(a)') "Error reading in "//trim(filename_in)//". is it really an xtc file?"
                    write(error_unit, *)
                    stop
                end if

                allocate(this%pos(dimn, this%natoms))

            type is (trrfile)
                ! get number of atoms in system and allocate position, velocity, frc arrays.
                this%stat = read_trr_natoms(filename, this%natoms)

                if (this%stat /= 0) then
                    write(error_unit, *)
                    write(error_unit, '(a)') "Error reading in "//trim(filename_in)//". is it really an trr file?"
                    write(error_unit, *)
                    stop
                end if

                allocate(this%pos(dimn, this%natoms))
                allocate(this%vel(dimn, this%natoms))
                allocate(this%frc(dimn, this%natoms))
            end select
        end if

        ! open the file for reading or writing. convert c pointer to fortran pointer.
        xd_c = xdrfile_open(filename, this%mode)
        call c_f_pointer(xd_c, this%xd)
    end subroutine

    subroutine read_xtcfile(xtc)
        implicit none
        class(xtcfile), intent(inout) :: xtc

        xtc%stat = read_xtc(xtc%xd, xtc%natoms, xtc%step, xtc%time, xtc%box, xtc%pos, xtc%prec)
    end subroutine

    subroutine read_trrfile(trr)
        implicit none
        class(trrfile), intent(inout) :: trr

        trr%stat = read_trr(trr%xd, trr%natoms, trr%step, trr%time, trr%lambda, trr%box, &
                           &trr%pos, trr%vel, trr%frc)
    end subroutine

    subroutine write_xtcfile(xtc, natoms, step, time, box, pos, prec)
        implicit none
        class(xtcfile), intent(inout) :: xtc
        integer, intent(in) :: natoms, step
        real(r4), intent(in) :: time, box(dimn, dimn), pos(:, :), prec

        xtc%stat = write_xtc(xtc%xd, natoms, step, time, box, pos, prec)
    end subroutine

    subroutine write_trrfile(trr, natoms, step, time, lambda, box, pos, vel, frc)
        implicit none
        class(trrfile), intent(inout) :: trr
        integer, intent(in) :: natoms, step
        real(rk), intent(in) :: time, lambda
        real(rk), intent(in), optional :: box(dimn, dimn), pos(*), vel(*), frc(*)

        trr%stat = write_trr(trr%xd, natoms, step, time, lambda, box, pos, vel, frc)
    end subroutine

    subroutine close_xdr(this)
        implicit none
        class(trjfile), intent(inout) :: this

        select type (this)
        type is (xtcfile)
            if (allocated(this%pos)) deallocate(this%pos)
        type is (trrfile)
            if (allocated(this%pos)) deallocate(this%pos)
            if (allocated(this%vel)) deallocate(this%vel)
            if (allocated(this%frc)) deallocate(this%frc)
        end select

        this%stat = xdrfile_close(this%xd)
    end subroutine
end module
