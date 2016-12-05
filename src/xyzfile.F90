module trf_xyzfile
    use trf_trjfile, only: trjfile
    implicit none
    private

    integer, parameter :: info_len = 1024
    integer, parameter :: atom_name_len = 10

    type, extends(trjfile), public :: xyzfile
        integer, private :: unit
        character(len=info_len) :: info
        character(len=atom_name_len), allocatable :: atom_name(:)
    contains
        procedure, public :: open
        procedure, public :: read
        procedure, public :: close
        procedure, private :: openfile
        procedure, private :: closefile
        procedure, private :: read_natoms
    end type

contains
    subroutine openfile(this, file, mode)
        use trf_trjfile, only: trf_mode_read, trf_mode_write
        implicit none
        class(xyzfile) :: this
        character(len=*), intent(in) :: file
        character, intent(in) :: mode
        character(len=3) :: status

        select case (mode)
        case (trf_mode_read)
            status = 'old'
        case (trf_mode_write)
            status = 'new'
        end select
#ifdef F2008
        open(newunit=this%unit, file=file, status=status, iostat=this%stat)
#else
        open(newunit(this%unit), file=file, status=status, iostat=this%stat)
#endif
    end subroutine

    subroutine closefile(this)
        implicit none
        class(xyzfile) :: this

        close(this%unit)
    end subroutine

    subroutine open(this, file, mode)
        use trf_trjfile, only: trf_mode_read
        implicit none
        class(xyzfile) :: this
        character(len=*), intent(in) :: file
        character, intent(in) :: mode

        if (mode == trf_mode_read) then
            call this%read_natoms(file)
            allocate(this%atom_name(this%natoms))
        end if
        call this%trjfile%open(file, mode)
        call this%openfile(file, mode)
    end subroutine open

    subroutine read_natoms(this, file)
        use trf_trjfile, only: trf_mode_read
        implicit none
        class(xyzfile) :: this
        character(len=*), intent(in) :: file

        call this%openfile(file, trf_mode_read)
        read(this%unit, *) this%natoms
        call this%closefile()
    end subroutine

    subroutine read(this)
        implicit none
        class(xyzfile) :: this
        integer :: i

        read(this%unit, *) this%natoms
        read(this%unit, "(A)", iostat=this%stat) this%info
        do i = 1, this%natoms
            read(this%unit, *) this%atom_name(i), this%pos(:, i)
        end do
    end subroutine

    subroutine close(this)
        implicit none
        class(xyzfile) :: this

        call this%trjfile%close
        deallocate(this%atom_name)
        call this%closefile()
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
end module trf_xyzfile
