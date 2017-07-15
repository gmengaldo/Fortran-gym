program trajectory
      !! This program calculates vertical motion under gravity
      implicit none 

      real, parameter :: g = 9.81 ! gravity (m/s^2)
      real            :: s
      real            :: u0
      real            :: t

      u0 = 100 ! ! initial velocity (m/s)
      t  = 2   ! time (s)

      s = u0 * t - g * (t * t) / 2 ! distance (m)

      print *, "distance travelled [m] = ", s
      print *, "after time [s]         = ", t

 end program trajectory 
