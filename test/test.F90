program test
    implicit none

    integer, parameter :: line_len = 1024
    call test_xyzfile_read_natoms
    call test_xyzfile_read

contains
    subroutine test_xyzfile_read_natoms()
        use trjfort, only: xyzfile
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_read_natoms"

        call f%init('test.xyz', 'r')
        call assert(f%natoms == 3, name)
        call f%close
    end subroutine

    subroutine test_xyzfile_read()
        use trjfort, only: xyzfile
        implicit none
        type(xyzfile) :: f
        character(len=*), parameter :: name = "xyzfile_read"
        character(len=line_len) :: tmp_str

        call f%init('test.xyz', 'r')
        call f%read
        call assert(trim(f%info) == "info line", name//" - info", "info: "//f%info)

        call assert(trim(f%atom_name(1)) == "A", name//" - atom_name(1)", &
                   &"atom(1): "//trim(f%atom_name(1)))
        call assert(trim(f%atom_name(2)) == "BB", name//" - atom_name(2)", &
                   &"atom(2): "//trim(f%atom_name(2)))
        call assert(trim(f%atom_name(3)) == "CCC", name//" - atom_name(3)", &
                   &"atom(3): "//trim(f%atom_name(3)))

        write(tmp_str, *) f%pos(1, 1)
        call assert(f%pos(1, 1) == 1.0, name//" - pos(1, 1) ", &
                   &"pos(1, 1): "//trim(tmp_str))
        write(tmp_str, *) f%pos(3, 3)
        call assert(f%pos(3, 3) == 9.0, name//" - pos(3, 3) ", &
                   &"pos(3, 3): "//trim(tmp_str))
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
