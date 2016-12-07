program test
    use varpars, only: num2str, rk
    implicit none
    character(len=*), parameter :: xyz_testfile = 'test.xyz'

    call test_xyzfile_write
    call test_xyzfile_read_natoms
    call test_xyzfile_read

contains
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
        call f%write(pos, atom_name=['  A', ' BB', 'CCC'])
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

        call assert(trim(f%atom_name(1)) == "A", name//" - atom_name(1)", &
                   &"atom(1): "//trim(f%atom_name(1)))
        call assert(trim(f%atom_name(2)) == "BB", name//" - atom_name(2)", &
                   &"atom(2): "//trim(f%atom_name(2)))
        call assert(trim(f%atom_name(3)) == "CCC", name//" - atom_name(3)", &
                   &"atom(3): "//trim(f%atom_name(3)))

        call assert(f%pos(1, 1) == 1.0_rk, name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(num2str(f%pos(1, 1))))
        call assert(f%pos(1, 2) == 4.0_rk, name//" - pos(1, 2) ", &
                   &"pos(1, 2): "//trim(num2str(f%pos(1, 2))))
        call assert(f%pos(3, 3) == 9.0_rk, name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(num2str(f%pos(3, 3))))
        call f%close
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
end program
