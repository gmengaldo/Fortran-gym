module shape_module
    !! This module contains the base class for various geometric shapes
    implicit none

    private

    real(kind=4) :: pi = 3.14

    public :: shape_type

    ! Class shape
    type shape_type
        real(kind=4) :: dim1 = 1.
        real(kind=4) :: dim2 = 1.
        real(kind=4) :: dim3 = 0.
    contains
        procedure, public  :: area   => calculate_area
        procedure, public  :: volume => calculate_volume
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

    area = dim1 * dim2

end subroutine calculate_area



subroutine calculate_volume(this, dim1, dim2, dim3, volume)
class(shape_type), intent(inout) :: this

real(kind=4), intent(in)  :: dim1
real(kind=4), intent(in)  :: dim2
real(kind=4), intent(in)  :: dim3
real(kind=4), intent(out) :: volume

volume = this%dim1 * this%dim2 * this%dim3

end subroutine calculate_volume

end module shape_module
