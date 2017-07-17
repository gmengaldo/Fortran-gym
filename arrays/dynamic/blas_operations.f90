program operations

    implicit none
    real(kind=4)              :: blas1_result
    real(kind=4), allocatable :: v(:)
    real(kind=4), allocatable :: w(:)
    real(kind=4), allocatable :: blas2_result(:)
    real(kind=4), allocatable :: A(:,:)
    real(kind=4), allocatable :: B(:,:)
    real(kind=4), allocatable :: blas3_result(:,:)
    integer                   :: s1, s2
    integer                   :: i, j, k
    real                      :: start
    real                      :: finish
    character(len=1)          :: str
    integer                   :: valueRSS

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

    print *, "Enter size of arrays: "; read *, s1, s2
    print *, "Print values and results? [y/n]"; read *, str

    ! Allocate dynamic arrays
    allocate(v(s1))
    allocate(w(s1))
    allocate(blas2_result(s1))
    allocate(A(s1,s2))
    allocate(B(s1,s2))
    allocate(blas3_result(s1,s2))

    call system_mem_usage(valueRSS)
    print *, "memory usage = ", valueRSS

    ! Assign values
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
    if (str == "y") then
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
    end if

    ! Intrinsic Fortran operations
    start  = 0.
    finish = 0.
    call cpu_time(start)
    blas1_result = dot_product(v, w)
    blas3_result = matmul(A, B)
    call cpu_time(finish)

    print '("CPU time dot_product, matmul = ",f15.10," seconds.")', finish-start

    ! Print results intrinsic operations
    if (str == "y") then
        print *, "intrinsic dot_product(v, w) = ", blas1_result
        print *, "intrinsic matmul(A, B) = [ "
        do i = 1, size(blas3_result(:,1))
            print *, blas3_result(i,:)
        end do
        print *, " ]"
    end if

    ! Coded BLAS operations
    start  = 0.
    finish = 0.
    call cpu_time(start)
    call vector_vector_multiply(v, w, blas1_result)
    call cpu_time(finish)
    print '("CPU time blas 1 = ",f15.10," seconds.")', finish-start

    start  = 0.
    finish = 0.
    call cpu_time(start)
    call matrix_vector_multiply(A, v, blas2_result)
    call cpu_time(finish)
    print '("CPU time blas 2 = ",f15.10," seconds.")', finish-start

    start  = 0.
    finish = 0.
    call cpu_time(start)
    call matrix_matrix_multiply(A, B, blas3_result)
    call cpu_time(finish)
    print '("CPU time blas 3 = ",f15.10," seconds.")', finish-start

    ! Print results implemented BLAS operations
    if (str == "y") then
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
    end if

    deallocate(v, w, A, B, blas2_result, blas3_result)

end program operations



!! BLAS 1 operation
subroutine vector_vector_multiply(v, w, result)
    implicit none
    real(kind=4), dimension(:), intent(in) :: v
    real(kind=4), dimension(:), intent(in) :: w
    real(kind=4), intent(out)              :: result
    integer                                :: i

    result = 0.
    !$OMP PARALLEL
    do i = 1, size(v)
        result = result + v(i) * w(i)
    end do
    !$OMP END PARALLEL

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
    !$OMP PARALLEL
    do k = 1, size(A,1)
        do i = 1, size(A,2)
            do j = 1, size(A,1)
                result(i,k) = result(i,k) + A(i,j) * B(j,i)
            end do
        end do
    end do
    !$OMP END PARALLEL

end subroutine


!! Calculate runtime memory usage
subroutine system_mem_usage(valueRSS)
    implicit none
    !use ifport ! --> if on intel compiler
    integer, intent(out) :: valueRSS
    character(len=200)   :: filename=' '
    character(len=80)    :: line
    character(len=8)     :: pid_char=' '
    integer              :: pid
    logical              :: ifxst

    ! return negative number if not found
    valueRSS = -1

    ! get process ID
    pid=getpid()
    write(pid_char,'(I8)') pid
    filename='/proc/'//trim(adjustl(pid_char))//'/status'

    ! read system file
    inquire (file=filename, exist=ifxst)
    if (.not. ifxst) then
        write (*,*) 'system file does not exist'; return
    endif

    open(unit=100, file=filename, action='read')
    do
        read (100, '(a)', end = 120) line
        if (line(1:6) .eq. 'VmRSS:') then
            read (line(7:),*) valueRSS; exit
        endif
    enddo

    120 continue
    close(100)

    return

end subroutine system_mem_usage
