program strings

    implicit none

    character(len=40) :: name
    character(len=40) :: surname
    character(len=40) :: title

    name    = "John"
    surname = "Doe"
    title   = "Dr."

    ! No trimming (i.e removing trailing edge characters)
    print *, "Non-trimmed version:    ", &
              title//" "//name//" "//surname

    ! Trimming (i.e. removing trailing edge characters)
    print *, "Trimmed version:        ", &
              trim(title)//" "//trim(name)//" "//trim(surname)

    ! Finding substring inside string
    if (index(name, "Jo") == 0) then
        print *, "String Jo not found in ", name
    else
        print *, "String Jo found at ", index(name, "Jo")
        print *, "Total string = ", trim(name(index(name,"Jo"):len(name)))
    end if

end program strings

