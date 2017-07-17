program shape_main

    use :: shape_module

    implicit none

    type(shape_type) :: s1
    real(kind=4)     :: dim1
    real(kind=4)     :: dim2
    real(kind=4)     :: area

    dim1 = 20
    dim2 = 10

    ! Call shape constructor
    s1 = shape_type(dim1, dim2)

    ! Calculate area or volume
    call s1%area(dim1, dim2, area)

    print *, "area = ", area

end program shape_main
