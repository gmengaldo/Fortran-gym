program construction

    implicit none
    real   , dimension(2,3) :: tsource
    real   , dimension(2,3) :: fsource
    real   , dimension(2,3) :: result
    logical, dimension(2,3) :: mask
    integer                 :: m(6)
    integer                 :: n(4)
    integer                 :: r1(2)
    integer                 :: r2(4)

    interface
        subroutine write_array (a)
            real :: a(:,:)
        end subroutine write_array

        subroutine write_l_array (a)
            logical :: a(:,:)
        end subroutine write_l_array
    end interface

    ! Merge two arrays
    tsource = reshape((/  35,  23,  18,  28,  26,  39 /), (/ 2, 3 /))
    fsource = reshape((/ -35, -23, -18, -28, -26, -39 /), (/ 2, 3 /))
    mask    = reshape((/.true.,.false.,.false.,.true.,.false.,.false./),(/2,3/))

    result = merge(tsource, fsource, mask)
    call write_array(tsource)
    call write_array(fsource)
    call write_l_array(mask)
    call write_array(result)

    ! Pack arrays: gathering nonzero elements
    m  = (/ 1, 0, 0, 0, 5, 0 /)
    r1 = pack(m, m /= 0)
    write(*, fmt="(6(I0, ' '))") r1

    ! Pack arrays: gathering nonzero elements and appending from vector
    n = (/ 1, 0, 0, 2 /)
    r2 = pack(n, n /= 0, (/0, 0, 3, 4/))
    write(*, FMT="(4(I0, ' '))") r2

end program construction



subroutine write_array(a)
    implicit none
    real :: a(:,:)
    integer :: i, j

    do i = lbound(a,1), ubound(a,1)
        write(*,*) (a(i, j), j = lbound(a,2), ubound(a,2))
    end do
    return

end subroutine write_array



subroutine write_l_array(a)
    implicit none
    logical :: a(:,:)
    integer :: i, j

    do i = lbound(a,1), ubound(a,1)
        write(*,*) (a(i, j), j = lbound(a,2), ubound(a,2))
    end do
    return

end subroutine write_l_array
