module trf_trjfile
    implicit none
    private
    
    integer, parameter, public :: trf_dim = 3
    character(len=*), parameter, public :: trf_mode_read = 'r'
    character(len=*), parameter, public :: trf_mode_write = 'w'

    integer, parameter, private :: filename_len = 128

    type, public :: trjfile
        character(len=filename_len) :: file
        integer :: natoms, step, stat
        real :: box(trf_dim, trf_dim)
        real, allocatable :: pos(:, :)
        real, allocatable :: vel(:, :)
        real, allocatable :: frc(:, :)
        
    contains
        procedure, public :: open
        procedure, public :: read
        procedure, public :: close
    end type

contains
    subroutine open(this, file, mode)
        implicit none
        class(trjfile) :: this
        character(len=*), intent(in) :: file
        character, intent(in) :: mode

        this%file = file
        select case (mode)
        case ('r')
            allocate(this%pos(trf_dim, this%natoms))
            allocate(this%vel(trf_dim, this%natoms))
            allocate(this%frc(trf_dim, this%natoms))
        case ('w')
        end select
    end subroutine

    subroutine read(this)
        implicit none
        class(trjfile) :: this
    end subroutine

    subroutine close(this)
        implicit none
        class(trjfile) :: this

        if (allocated(this%pos)) deallocate(this%pos)
        if (allocated(this%vel)) deallocate(this%vel)
        if (allocated(this%frc)) deallocate(this%frc)
    end subroutine
end module
