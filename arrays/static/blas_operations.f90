program operations

    implicit none
    real(kind=4)                 :: blas1_result
    real(kind=4), dimension(3)   :: v, w, blas2_result
    real(kind=4), dimension(3,3) :: A, B, blas3_result
    integer                      :: i, j, k
    real                         :: start
    real                         :: finish

    interface
        subroutine vector_vector_multiply(v, w, result)
            implicit none
            real(kind=4), dimension(:), intent(in) :: v
            real(kind=4), dimension(:), intent(in) :: w
            real(kind=4), intent(out)              :: result
        end subroutine

        subroutine matrix_vector_multiply(A, v, result)
            implicit none
            real(kind=4), dimension(:,:), intent(in)  :: A
            real(kind=4), dimension(:)  , intent(in)  :: v
            real(kind=4), dimension(:)  , intent(out) :: result
        end subroutine

        subroutine matrix_matrix_multiply(A, B, result)
            implicit none
            real(kind=4), dimension(:,:), intent(in)  :: A
            real(kind=4), dimension(:,:), intent(in)  :: B
            real(kind=4), dimension(:,:), intent(out) :: result
        end subroutine
    end interface

    do i = 1, size(v)
        v(i) = i
        w(i) = i+1
    end do

    do i = 1, size(A(:,1))
        do j = 1, size(A(1,:))
            A(i,j) = i
            B(i,j) = i+1
        end do
    end do

    ! Memory requirements
    print *, "Memory occupied v = ", sizeof(v), " bytes"
    print *, "Memory occupied A = ", sizeof(A), " bytes"

    ! Values stored
    print *, "v = [ "
    do i = 1, size(v)
        print *, v(i)
    end do
    print *, " ]."

    print *, "w = [ "
    do i = 1, size(w)
        print *, w(i)
    end do
    print *, " ]."

    print *, "A = [ "
    do i = 1, size(A(:,1))
        print *, A(i,:)
    end do
    print *, " ]."

    print *, "B = [ "
    do i = 1, size(B(:,1))
        print *, B(i,:)
    end do
    print *, " ]."

    ! Intrinsic Fortran operations
    start  = 0.
    finish = 0.
    call cpu_time(start)
    blas1_result = dot_product(v, w)
    blas3_result = matmul(A, B)
    call cpu_time(finish)

    print '("CPU time dot_product, matmul = ",f12.10," seconds.")', finish-start
    print *, "intrinsic dot_product(v, w) = ", blas1_result
    print *, "intrinsic matmul(A, B) = [ "
    do i = 1, size(blas3_result(:,1))
        print *, blas3_result(i,:)
    end do
    print *, " ]"

    ! Coded BLAS operations
    start  = 0.
    finish = 0.
    call cpu_time(start)
    call vector_vector_multiply(v, w, blas1_result)
    call cpu_time(finish)
    print '("CPU time blas 1 = ",f12.10," seconds.")', finish-start

    start  = 0.
    finish = 0.
    call cpu_time(start)
    call matrix_vector_multiply(A, v, blas2_result)
    call cpu_time(finish)
    print '("CPU time blas 2 = ",f12.10," seconds.")', finish-start

    start  = 0.
    finish = 0.
    call cpu_time(start)
    call matrix_matrix_multiply(A, B, blas3_result)
    call cpu_time(finish)
    print '("CPU time blas 3 = ",f12.10," seconds.")', finish-start

    print *, "BLAS 1 = ", blas1_result
    print *, "BLAS 2 = [ "
    do i = 1, size(blas2_result)
        print *, blas2_result(i)
    end do
    print *, " ]."
    print *, "BLAS 3 = [ "
    do i = 1, size(blas3_result(:,1))
        print *, blas3_result(i,:)
    end do
    print *, " ]"

end program operations



!! BLAS 1 operation
subroutine vector_vector_multiply(v, w, result)
    implicit none
    real(kind=4), dimension(:), intent(in) :: v
    real(kind=4), dimension(:), intent(in) :: w
    real(kind=4), intent(out)              :: result
    integer                                :: i

    result = 0.
    do i = 1, size(v)
        result = result + v(i) * w(i)
    end do

end subroutine



!! BLAS 2 operation
subroutine matrix_vector_multiply(A, v, result)
    implicit none
    real(kind=4), dimension(:,:), intent(in)  :: A
    real(kind=4), dimension(:)  , intent(in)  :: v
    real(kind=4), dimension(:)  , intent(out) :: result
    integer                                   :: i, j

    result = 0.
    do i = 1, size(A(:,1))
        do j = 1, size(A(1,:))
            result(i) = result(i) + A(i,j) * v(j)
        end do
    end do

end subroutine



!! BLAS 3 operation
subroutine matrix_matrix_multiply(A, B, result)
    implicit none
    real(kind=4), dimension(:,:), intent(in)  :: A
    real(kind=4), dimension(:,:), intent(in)  :: B
    real(kind=4), dimension(:,:), intent(out) :: result
    integer                                   :: i, j, k

    result = 0.
    do k = 1, size(A(:,1))
        do i = 1, size(A(:,1))
            do j = 1, size(A(1,:))
                result(i,k) = result(i,k) + A(i,j) * B(j,i)
            end do
        end do
    end do

end subroutine
