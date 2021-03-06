module xyz
    use varpars, only: rk, filename_len, line_len, dimn, mode_read,&
                      &mode_write

    implicit none
    private
    integer, parameter :: info_len = line_len
    integer, parameter :: atom_name_len = 10

    type, public :: xyzfile
        character(len=filename_len) :: file
        integer :: unit
        character :: mode
        integer :: natoms, stat
        character(len=info_len) :: info
        real(rk), allocatable :: pos(:, :)
        real(rk), allocatable :: optdata(:, :)  ! (:, natoms)
        character(len=atom_name_len), allocatable :: atom_name(:)

    contains
        procedure :: open
        procedure :: read
        procedure :: write
        procedure :: close
    end type

contains
    subroutine open(this, file, mode, optdata_dim)
        implicit none
        class(xyzfile) :: this
        character (len=*), intent(in), optional :: file
        character, intent(in), optional :: mode
        integer, intent(in), optional :: optdata_dim
        
        this%file = file
        if (present(mode)) then
            this%mode = mode
        else
            this%mode = mode_read
        end if

        select case (this%mode)
        case (mode_read)
            this%natoms = read_natoms_xyz(file)
            call open_xyz(this%unit, file=file, status='old', iostat=this%stat)
            allocate(this%pos(dimn, this%natoms))
            allocate(this%atom_name(this%natoms))
            if (present(optdata_dim)) allocate(this%optdata(optdata_dim, this%natoms))
        case (mode_write)
            call open_xyz(this%unit, file=file, status='unknown', iostat=this%stat)
        end select
    end subroutine

    subroutine read(this)
        implicit none
        class(xyzfile), intent(inout) :: this

        if (allocated(this%optdata)) then
            call read_xyz(this%unit, this%pos, this%natoms, this%info, this%atom_name, this%stat, this%optdata)
        else
            call read_xyz(this%unit, this%pos, this%natoms, this%info, this%atom_name, this%stat)
        end if
    end subroutine

    subroutine write(this, pos, info, atom_name, optdata)
        use varpars, only: num2str
        implicit none
        class(xyzfile), intent(inout) :: this
        real(rk), intent(in) :: pos(:, :)  !(3, natoms)
        character(len=*), intent(in), optional :: info, atom_name(:)
        real(rk), intent(in), optional :: optdata(:, :)  ! (:, natoms)
        integer :: natoms
        character(len=info_len) :: info_
        character(len=atom_name_len) :: atom_name_(size(pos, 2))
        character(len=line_len) :: format_opt, optdata_dim_c, format
        integer :: i, unit
        character(len=*), parameter :: format_end = ")"
        character(len=line_len) :: format_begin
#ifdef USE_DOUBLE
        character(len=*), parameter :: format_rk = "F22.16"
#else
        character(len=*), parameter :: format_rk = "F13.7"
#endif

        format_begin = "(A, "//trim(num2str(dimn))//format_rk

        unit = this%unit
        natoms = size(pos, 2)

        if (present(info)) then
            info_ = info
        else
            info_ = "XYZ"
        end if

        if (present(atom_name)) then
            atom_name_ = atom_name
        else
            atom_name_ = "X"
        end if

        write(unit, *) natoms
        write(unit, "(A)") trim(info_)

        if (present(optdata)) then
            write(optdata_dim_c, *) size(optdata, 1)
            format_opt = ", " // trim(adjustl(optdata_dim_c)) // format_rk
            format = trim(format_begin) // trim(format_opt) // format_end
            do i = 1, natoms
                write(unit, format, iostat=this%stat) trim(atom_name_(i)), pos(:, i), optdata(:, i)
            end do
        else
            format = trim(format_begin) // format_end
            do i = 1, natoms
                write(unit, format, iostat=this%stat) trim(atom_name_(i)), pos(:, i)
            end do
        end if
    end subroutine

    subroutine close(this)
        implicit none
        class(xyzfile) :: this

        if (allocated(this%pos)) deallocate(this%pos)
        if (allocated(this%optdata)) deallocate(this%optdata)
        if (allocated(this%atom_name)) deallocate(this%atom_name)
        close(this%unit)
    end subroutine

    integer function read_natoms_xyz(file) result(natoms)
        implicit none
        character(len=*), intent(in) :: file
        integer :: unit

        call open_xyz(unit, file, 'old')
        read(unit, *) natoms
        close(unit)
    end function read_natoms_xyz

    subroutine open_xyz(unit, file, status, iostat)
        implicit none
        integer, intent(out) :: unit
        character(len=*), intent(in) :: file, status
        integer, intent(out), optional :: iostat

        if (present(iostat)) then
#ifdef F2008
            open(newunit=unit, file=file, status=status, iostat=iostat)
#else
            open(newunit(unit), file=file, status=status, iostat=iostat)
#endif
        else
#ifdef F2008
            open(newunit=unit, file=file, status=status)
#else
            open(newunit(unit), file=file, status=status)
#endif
        end if
    end subroutine

    subroutine read_xyz(unit, pos, natoms, info, atom_name, iostat, optdata)
        implicit none
        integer, intent(in) :: unit
        real(rk), intent(out) :: pos(:, :)  !(3, natoms)
        integer, intent(out) :: natoms
        character(len=*), intent(out) :: info, atom_name(:)
        integer, intent(out) :: iostat
        real(rk), intent(out), optional :: optdata(:, :)  ! (:, natoms)
        integer :: i

        read(unit, *) natoms
        read(unit, "(A)") info

        if (present(optdata)) then
            do i = 1, natoms
            read(unit, *, iostat=iostat) atom_name(i), pos(:, i), optdata(:, i)
            end do
        else
            do i = 1, natoms
            read(unit, *, iostat=iostat) atom_name(i), pos(:, i)
            end do
        end if
    end subroutine

#ifndef F2008
    integer function newunit(unit) result(n)
        ! http://www.fortran90.org/src/best-practices.html
        ! returns lowest i/o unit number not in use
        integer, intent(out), optional :: unit
        logical inuse
        integer, parameter :: nmin=10   ! avoid lower numbers which are sometimes reserved
        integer, parameter :: nmax=999  ! may be system-dependent
        do n = nmin, nmax
            inquire(unit=n, opened=inuse)
            if (.not. inuse) then
                if (present(unit)) unit=n
                return
            end if
        end do
        stop "newunit ERROR: available unit not found."
    end function
#endif /* FORTRAN2008 */
end module xyz
