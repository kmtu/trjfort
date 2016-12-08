program test
    use varpars, only: num2str, rk, dimn
    implicit none
    character(len=*), parameter :: xyz_testfile = 'test.xyz'
    character(len=*), parameter :: trr_testfile = 'test.trr'
    character(len=*), parameter :: info = "xyz info"
    character(len=*), parameter :: atom_name(*) = ['A  ', 'BB ', 'CCC', 'DDD']
    integer, parameter :: natoms = size(atom_name)
    integer, parameter :: optdata_dim = 6
    real(rk) :: box(dimn, dimn)
    real(rk) :: pos(dimn, natoms), vel(dimn, natoms), frc(dimn, natoms)
    real(rk) :: optdata(optdata_dim, natoms)

    call prep_test
    call test_xyzfile_write
    call test_xyzfile_read_natoms
    call test_xyzfile_read
    call test_xyzfile_write2
    call test_xyzfile_read2

    call test_trrfile_write
    call test_trrfile_read
    call test_trrfile_write2
    call test_trrfile_read2

contains
    subroutine prep_test()
        implicit none
        integer :: i, j
        do i = 1, natoms
            do j = 1, dimn
                pos(j, i) = real((i-1)*dimn + j, rk)
            end do
        end do

        do i = 1, natoms
            do j = 1, optdata_dim
                optdata(j, i) = real((i-1)*optdata_dim + j, rk) * 2
            end do
        end do

        do i = 1, dimn
            do j = 1, dimn
                if (i == j) then
                    box(j, i) = 1.23456789123456789
                else
                    box(j, i) = 0.0
                end if
            end do
        end do

        do i = 1, natoms
            do j = 1, dimn
                vel(j, i) = real((i-1)*dimn + j, rk) * 3
            end do
        end do

        do i = 1, natoms
            do j = 1, dimn
                frc(j, i) = real((i-1)*dimn + j, rk) * 4
            end do
        end do
    end subroutine

    subroutine assert(test, name, errmsg)
        implicit none
        logical, intent(in) :: test
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: errmsg

        if (test) then
            write(*, *) name//": passed"
        else
            write(*, *) name//": failed"
            if (present(errmsg)) write(*, *) errmsg
        end if
    end subroutine

    subroutine test_xyzfile_write()
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_write"

        call f%init(xyz_testfile, 'w')
        call f%write(pos)
        call assert(f%stat == 0, name, "f%stat = "//trim(num2str(f%stat)))
        call f%close
    end subroutine

    subroutine test_xyzfile_read_natoms()
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_read_natoms"

        call f%init(xyz_testfile, mode_read)
        call assert(f%natoms == natoms, name)
        call f%close
    end subroutine

    subroutine test_xyzfile_read()
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_read"

        call f%init(xyz_testfile, mode_read)
        call f%read
        call assert(trim(f%info) == "XYZ", name//" - info", "info: "//trim(f%info))

        call assert(f%pos(1, 1) == pos(1, 1), name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == pos(1, 2), name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == pos(3, 3), name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))
        call f%close
    end subroutine

    subroutine test_xyzfile_write2()
        use varpars, only: dimn
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_write2"

        call f%init(xyz_testfile, 'w')
        call f%write(pos, info=info, atom_name=atom_name,&
                    &optdata=optdata)
        call assert(f%stat == 0, name, "f%stat = "//trim(num2str(f%stat)))
        call f%close
    end subroutine

    subroutine test_xyzfile_read2()
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_read2"

        call f%init(xyz_testfile, mode_read, optdata_dim)
        call f%read
        call assert(f%info == info, name//" - info", "info: "//trim(f%info))

        call assert(trim(f%atom_name(1)) == atom_name(1), name//" - atom_name(1)", &
                   &"atom(1): "//trim(f%atom_name(1)))
        call assert(trim(f%atom_name(2)) == atom_name(2), name//" - atom_name(2)", &
                   &"atom(2): "//trim(f%atom_name(2)))
        call assert(trim(f%atom_name(3)) == atom_name(3), name//" - atom_name(3)", &
                   &"atom(3): "//trim(f%atom_name(3)))

        call assert(f%pos(1, 1) == pos(1, 1), name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == pos(1, 2), name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == pos(3, 3), name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))

        call assert(f%optdata(1, 1) == optdata(1, 1), name//" - optdata(1, 1) ", &
                   &"optdata(1, 1): "//trim(num2str(f%optdata(1, 1))))
        call assert(f%optdata(1, 2) == optdata(1, 2), name//" - optdata(1, 2) ", &
                   &"optdata(1, 2): "//trim(num2str(f%optdata(1, 2))))
        call assert(f%optdata(3, 3) == optdata(3, 3), name//" - optdata(3, 3) ", &
                   &"optdata(3, 3): "//trim(num2str(f%optdata(3, 3))))

        call f%close
    end subroutine

    subroutine test_trrfile_write()
        use trjfort, only: trrfile, mode_write
        implicit none
        type(trrfile) :: f
        integer :: step
        real(rk) :: time, lambda
        character(len=*), parameter :: name = "trrfile_write"

        step = 5
        time = 0.010
        lambda = 0.8

        call f%init(trr_testfile, mode_write)
        call f%write(natoms, step, time, lambda, box, pos)
        call assert(f%stat == 0, name, "f%stat = "//trim(num2str(f%stat)))
        call f%close
    end subroutine

    subroutine test_trrfile_read()
        use trjfort, only: trrfile, mode_read
        implicit none
        type(trrfile) :: f
        integer :: step
        real(rk) :: time, lambda
        character(len=*), parameter :: name = "trrfile_read"

        step = 5
        time = 0.010
        lambda = 0.8

        call f%init(trr_testfile, mode_read)
        call f%read
        call assert(f%step == step, name//" - step")
        call assert(f%time == time, name//" - time")
        call assert(f%lambda == lambda, name//" - lambda")
        call assert(f%pos(1, 1) == pos(1, 1), name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == pos(1, 2), name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == pos(3, 3), name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))
        call f%close
    end subroutine

    subroutine test_trrfile_write2()
        use trjfort, only: trrfile, mode_write
        implicit none
        type(trrfile) :: f
        integer :: step
        real(rk) :: time, lambda
        character(len=*), parameter :: name = "trrfile_write2"

        step = 5
        time = 0.010
        lambda = 0.8

        call f%init(trr_testfile, mode_write)
        call f%write(natoms, step, time, lambda, box, vel=vel, frc=frc)
        call assert(f%stat == 0, name, "f%stat = "//trim(num2str(f%stat)))
        call f%close
    end subroutine

    subroutine test_trrfile_read2()
        use trjfort, only: trrfile, mode_read
        implicit none
        type(trrfile) :: f
        integer :: step
        real(rk) :: time, lambda
        character(len=*), parameter :: name = "trrfile_read"

        step = 5
        time = 0.010
        lambda = 0.8

        call prep_test
        call f%init(trr_testfile, mode_read)
        call f%read
        call assert(f%vel(1, 1) == vel(1, 1), name//" - vel(1, 1) ", &
                   &"vel(1, 1): "//trim(num2str(f%vel(1, 1))))
        call assert(f%vel(2, 3) == vel(2, 3), name//" - vel(2, 3) ", &
                   &"vel(2, 3): "//trim(num2str(f%vel(2, 3))))
        call assert(f%frc(1, 1) == frc(1, 1), name//" - frc(1, 1) ", &
                   &"frc(1, 1): "//trim(num2str(f%frc(1, 1))))
        call assert(f%frc(2, 3) == frc(2, 3), name//" - frc(2, 3) ", &
                   &"frc(2, 3): "//trim(num2str(f%frc(2, 3))))
        call f%close
    end subroutine
end program
