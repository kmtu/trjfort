program test
    use varpars, only: num2str, rk
    implicit none
    character(len=*), parameter :: xyz_testfile = 'test.xyz'
    character(len=*), parameter :: info = "xyz info"
    character(len=*), parameter :: atom_name(*) = ['A  ', 'BB ', 'CCC']
    integer, parameter :: optdata_dim = 6

    call test_xyzfile_write
    call test_xyzfile_read_natoms
    call test_xyzfile_read
    call test_xyzfile_write2
    call test_xyzfile_read2

contains
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
        use varpars, only: dimn
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        integer, parameter :: natoms = 3
        real(rk) :: pos(dimn, natoms)
        character(len=*), parameter :: name = "xyzfile_write"
        integer :: i, j

        do i = 1, natoms
            do j = 1, dimn
                pos(j, i) = real((i-1)*dimn + j, rk)
            end do
        end do
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
        call assert(f%natoms == 3, name)
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

        call assert(f%pos(1, 1) == 1.0_rk, name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == 4.0_rk, name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == 9.0_rk, name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))
        call f%close
    end subroutine

    subroutine test_xyzfile_write2()
        use varpars, only: dimn
        use trjfort, only: xyzfile, mode_read
        implicit none
        type(xyzfile) :: f
        integer, parameter :: natoms = 3
        real(rk) :: pos(dimn, natoms), optdata(optdata_dim, natoms)
        character(len=*), parameter :: name = "xyzfile_write2"
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

        call assert(f%pos(1, 1) == 1.0_rk, name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == 4.0_rk, name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == 9.0_rk, name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))

        call assert(f%optdata(1, 1) == 2.0_rk, name//" - optdata(1, 1) ", &
                   &"optdata(1, 1): "//trim(num2str(f%optdata(1, 1))))
        call assert(f%optdata(1, 2) == 14.0_rk, name//" - optdata(1, 2) ", &
                   &"optdata(1, 2): "//trim(num2str(f%optdata(1, 2))))
        call assert(f%optdata(3, 3) == 30.0_rk, name//" - optdata(3, 3) ", &
                   &"optdata(3, 3): "//trim(num2str(f%optdata(3, 3))))

        call f%close
    end subroutine
end program
