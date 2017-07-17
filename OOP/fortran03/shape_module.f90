module shape_module
    !! This module contains the base class for various geometric shape
    implicit none

    private
    public :: shape_type

    ! Class shape
    type shape_type
        real(kind=4) :: dim1 = 1.
        real(kind=4) :: dim2 = 1.
        real(kind=4) :: dim3 = 0.
    contains
        procedure, public  :: area   => calculate_area
    end type shape_type

    ! Definition of the constructor
    interface shape_type
        module procedure shape_constructor
    end interface shape_type

contains

! Constructor for class shape
function shape_constructor() result(shape)
type(shape_type) :: shape
real(kind=4)     :: dim1
real(kind=4)     :: dim2
real(kind=4)     :: dim3

shape%dim1   = dim1
shape%dim2   = dim2
shape%dim3   = dim3

end function shape_constructor



subroutine calculate_area(this, dim1, dim2, area)
class(shape_type), intent(inout) :: this

real(kind=4), intent(in)  :: dim1
real(kind=4), intent(in)  :: dim2
real(kind=4), intent(out) :: area
real(kind=4), parameter   :: pi = 3.14

    area = this%dim1 * this%dim2

end subroutine calculate_area

end module shape_module
