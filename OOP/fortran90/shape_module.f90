module class_Circle
    implicit none
    private
    public :: Circle
    public :: circle_area
    public :: circle_print

    ! Class-wide private constant
    real :: pi = 3.1415926535897931d0

    type Circle
        real :: radius
    end type Circle

contains

    function circle_area(this) result(area)
        type(Circle), intent(in) :: this
        real                     :: area

        area = pi * this%radius**2

    end function circle_area

    subroutine circle_print(this)
        type(Circle), intent(in) :: this
        real                     :: area

        ! Call the circle_area function
        area = circle_area(this)

        print *, 'Circle'
        print *, '  r   = ', this%radius, &
                 ' area = ', area

    end subroutine circle_print

end module class_Circle
