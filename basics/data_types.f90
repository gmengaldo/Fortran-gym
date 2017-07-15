program data_types

      implicit none
      integer           :: i
      integer(kind=2)   :: i2
      integer(kind=4)   :: i4
      integer(kind=8)   :: i8
      integer(kind=16)  :: i16
      integer(kind=16)  :: i_result1
      integer(kind=16)  :: i_result2

      real          :: r
      real(kind=4)  :: r4
      real(kind=8)  :: r8
      real(kind=16) :: r16
      real(kind=16) :: r_result1
      real(kind=16) :: r_result2

      character(len=40) :: str
      logical           :: bool
      complex           :: compl

      ! Executions
      print *, "maximum integer     = ", huge(i)
      print *, "maximum integer 2   = ", huge(i2)
      print *, "maximum integer 4   = ", huge(i4)
      print *, "maximum integer 8   = ", huge(i8)
      print *, "maximum integer 16  = ", huge(i16)

      print *, "maximum real    = ", huge(r)
      print *, "maximum real 4  = ", huge(r4)
      print *, "maximum real 8  = ", huge(r8)
      print *, "maximum real 16 = ", huge(r16)

      print *, "bool default = ", bool
      bool = .false.; print *, "bool defined = ", bool

      print *, "character default = ", str
      str = "Ciao"; print *, "character str(1:2) = ", str(1:2)

      print *, "complex default = ", compl
      compl = (1, 5);           print *, "complex defined 1 = ", compl
      compl = cmplx(0.1, -7.0); print *, "complex defined 2 = ", compl


      ! Calculations for integers
      i   = 101
      i2  = 2
      i4  = 3
      i8  = 30
      i16 = 300
      i_result1 = i / i2
      i_result2 = i2 / i4

      print *, "i_result1 = ", i_result1, "i_result2 = ", i_result2

      ! Calculations for reals
      r = 1.0
      r4 = 2.0
      r8 = 3.0
      r16 = 300.0
      r_result1 = r / r4
      r_result2 = r4 / r8

      print *, "r_result1 = ", r_result1, "r_result2 = ", r_result2

end program data_types
