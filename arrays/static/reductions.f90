program reductions

    implicit none
    real(kind=4), dimension(10) :: v
    integer                     :: i

    do i = 1, size(v)
        v(i) = size(v) + 1 - i
    end do

    write(*,*) "shape of original v: ", shape(v);
    write(*,*) "original v = ", v
    v = reshape((/1,2,3,4,5,6,7,8,9,10/), (/10/))
    write(*,*) "shape of v reshaped: ", shape(v);
    write(*,*) "v reshaped = ", v

    ! all, any, count
    print *, all(v > 5)
    print *, all(v > 5, 1)
    print *, any(v > 50)
    print *, any(v > 50, 1)
    print *, count(v > 5)
    print *, count(v > 5, 1)

    ! maxval, minval
    print *, maxval(v)
    print *, maxval(v, 1)
    print *, maxval(v, v > 5)
    print *, maxval(v, 1, v > 5)
    print *, minval(v)
    print *, minval(v, 1)
    print *, minval(v, v > 5)
    print *, minval(v, 1, v > 5)

    ! product, sum
    print *, product(v)
    print *, product(v, 1)
    print *, product(v, v > 5)
    print *, product(v, 1, v > 5)
    print *, sum(v)
    print *, sum(v, 1)
    print *, sum(v, v > 5)
    print *, sum(v, 1, v > 5)

end program reductions
