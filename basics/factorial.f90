program factorial

      implicit none
      integer :: n
      integer :: nmax
      integer :: fact

      fact = 1
      nmax = 10

      ! compute factorial version 1
      do n = 1, nmax
          fact = fact * n;
      end do
      print *, "Version 1. Factorial of ", nmax, "is:    ", fact

      fact = 1
      n    = 1
      do while(n <= nmax)
          fact = fact * n;
          print *, "n = ", n, "  ", "factorial = ", fact
          n = n + 1
      end do

end program factorial

