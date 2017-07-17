program derived_types

    implicit none
    type book
        character(len=40) :: title     = "Fr"
        character(len=40) :: author    = "MT"
        integer           :: year      = 1964
        integer           :: ID        = 46
        character(len=40) :: publisher = "Sa"
    end type book

    type(book), pointer             :: book1
    type(book), target              :: book2
    type(book), allocatable, target :: book3(:)
    integer                         :: i

    allocate(book1)
    allocate(book3(10))

    ! Association - note that can be only on one of the list: 
    ! i.e. always the last association holds.
    do i = 1, size(book3, 1)
        book1 => book3(i)
    end do

    ! Disassociates the pointer book1 from the target book3(size(book3, 1))
    nullify(book1)

    ! Associates the pointer book1 to the target book2
    ! Note that it would have been overridden if not nullified
    book1 => book2

    ! Assign values to pointer DT book1
    book1%title     = "World"
    book1%author    = "Me"
    book1%year      = 1985
    book1%ID        = 12
    book1%publisher = "Ve"

    ! Assign new title to target DT book2
    book2%title     = "Hello"

    ! Check allocation and association
    print *, "book1 allocated? ", allocated(book3)
    print *, "book1 associated? ", associated(book1)

    ! Print values
    print *, book1%title
    print *, book1%author
    print *, book1%year
    print *, book1%ID
    print *, book1%publisher

    print *, book2%title
    print *, book2%author
    print *, book2%year
    print *, book2%ID
    print *, book2%publisher

    do i = 1, size(book3, 1)
        print *, "book3 = ", book3(i)
    end do

end program derived_types
