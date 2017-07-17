program static_arrays

    implicit none
    real(kind=4), dimension(10)       :: array
    real(kind=4), dimension(10)       :: array1D
    real(kind=4), dimension(10,12)    :: array2D
    real(kind=4), dimension(10,12,15) :: array3D
    integer                           :: i, j, k

    interface
        subroutine setArray1D(array1D)
            real(kind=4), dimension(:), intent(inout) :: array1D
            integer                                   :: i
        end subroutine setArray1D

        subroutine setArray2D(array2D)
            real(kind=4), dimension(:,:), intent(inout) :: array2D
            integer                                     :: i, j
        end subroutine setArray2D

        subroutine setArray3D(array3D)
            real(kind=4), dimension(:,:,:), intent(inout) :: array3D
            integer                                       :: i, j, k
        end subroutine setArray3D
    end interface

    array(1:5)  = 3.0
    array(6:10) = 6.0
    print *, "array = ["
    do i = 1, size(array(:))
        print *, array(i)
    end do
    print *, "]"

    call setArray1D(array1D)
    call setArray2D(array2D)
    call setArray3D(array3D)

    print *, "array1D = [ "
    do i = 1, size(array1D(:))
        print *, array1D(i)
    end do
    print *, " ]"

    print *, "Dimensions array2D = ", &
        size(array2D(:,1)), trim("x"), &
        size(array2D(1,:))

    print *, "array2D = [ "
    do i = j, size(array2D(1,:))
        print *, array2D(1:10,j)
    end do
    print *, " ]"

    print *, "Dimensions array3D = ", &
        size(array3D(:,1,1)), trim("x"), &
        size(array3D(1,:,1)), trim("x"), &
        size(array3D(1,1,:))

    do k = 1, size(array3D(1,1,:))
        print *, "array3D(",k,") = [ "
        do j = 1, size(array3D(1,:,k))
            print *, array3D(1:10,j,k)
        end do
        print *, " ]"
    end do

    ! Memory requirements
    print *, "Memory occupied array   = ", sizeof(array)  , " bytes"
    print *, "Memory occupied array1D = ", sizeof(array1D), " bytes"
    print *, "Memory occupied array2D = ", sizeof(array2D), " bytes"
    print *, "Memory occupied array3D = ", sizeof(array3D), " bytes"

end program static_arrays



subroutine setArray1D(array1D)
    implicit none
    real(kind=4), dimension(:), intent(inout) :: array1D
    integer                                   :: i

    do i = 1, size(array1D)
        array1D(i) = i
    end do

end subroutine setArray1D



subroutine setArray2D(array2D)
    implicit none
    real(kind=4), dimension(:,:), intent(inout) :: array2D
    integer                                     :: i, j

    do i = 1, size(array2D(:,1))
        do j = 1, size(array2D(1,:))
            array2D(i,j) = i
        end do
    end do

end subroutine setArray2D



subroutine setArray3D(array3D)
    implicit none
    real(kind=4), dimension(:,:,:), intent(inout) :: array3D
    integer                                       :: i, j, k

    do i = 1, size(array3D(:,1,1))
        do j = 1, size(array3D(1,:,1))
            do k = 1, size(array3D(1,1,:))
                array3D(i,j,k) = i
            end do
        end do
    end do

end subroutine setArray3D
