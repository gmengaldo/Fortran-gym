program pointer_simple
    implicit none
    integer, pointer :: a
    integer, pointer :: b
    integer, target  :: t
    integer          :: n

    ! Assign value 1 to target t
    t = 1

    ! Associate pointer a to target t
    a => t

    ! Re-assign new value for target t 
    ! note that this will change the value of pointer a too
    t = 2

    ! Associate pointer b to target t
    b => t

    ! sum the two pointers together
    n = a + b

    ! Print the four values
    print *, a, b, t, n

end program pointer_simple
