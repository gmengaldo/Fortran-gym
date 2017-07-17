program circle_test

    use class_Circle

    implicit none
    type(Circle) :: c     ! Declare a variable of type Circle

    ! Use the implicit constructor, radius = 1.5
    c = Circle(1.5)

    ! Call a class subroutine
    call circle_print(c)
end program circle_test
