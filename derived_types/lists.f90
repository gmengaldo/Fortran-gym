program lists

    implicit none
    type book
        character(len=40) :: title
        character(len=40) :: author
        integer           :: year
        integer           :: ID
        character(len=40) :: publisher
    end type book

    integer                 :: i, j
    type(book), allocatable :: book1(:,:)

    print *, allocated(book1)
    allocate(book1(20,2))
    print *, allocated(book1)

    do i = 1, size(book1, 2)
        do j = 1, size(book1, 1)
            book1(i,j)%title     = "World"
            book1(i,j)%author    = "Me"
            book1(i,j)%year      = 1985+i*j
            book1(i,j)%ID        = i*j
            book1(i,j)%publisher = "Venezia"
        end do
    end do

    do i = 1, size(book1, 2)
        do j = 1, size(book1, 1)
            print *, book1(i,j)%title
            print *, book1(i,j)%author
            print *, book1(i,j)%year
            print *, book1(i,j)%ID
            print *, book1(i,j)%publisher
        end do
    end do

end program lists
