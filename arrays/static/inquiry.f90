program inquiry

    implicit none
    real(kind=4), dimension(10,2) :: v
    real(kind=4), allocatable     :: w(:,:)
    integer                       :: i
    integer                       :: j

    ! Assign values to array v
    do i = 1, size(v, 1)
        do j = 1, size(v, 2)
            v(i,j) = i
        end do
    end do

    ! Inquiry operations
    print *, "allocated? ", allocated(w)
    print *, "lbound? "   , lbound(v)
    print *, "lbound 1? " , lbound(v, 1)
    print *, "lbound 2? " , lbound(v, 2)
    print *, "ubound? "   , ubound(v)
    print *, "ubound 1? " , ubound(v, 1)
    print *, "ubound 2? " , ubound(v, 2)
    print *, "shape? "    , shape(v)
    print *, "size? "     , size(v)
    print *, "size 1? "   , size(v, 1)
    print *, "size 2? "   , size(v, 2)

end program inquiry
