C**** FNDINT.F 
c
      function fndint(
     &                ivalin, nval, iarray)
      integer*4 fndint
c
c-----------------------------------------------------------------------
c
c     This routine searches through an array of integers for a match
c     with the input integer.  The return value is the index of the
c     matched code in the input array. A return value of zero indicates
c     no match was made.
c
c   Arguments:
c
c     Inputs:
c       ivalin   I   integer to match
c       nval     I   number of values in the array to search
c       iarray   I   array of integers to search
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 ivalin
      integer*4 nval
      integer*4 iarray(*)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4   i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      fndint = 0
c
c   ---- loop through the array ---
c
      do i=1,nval
c
c   ---- if match, set return value and return right away ---
c
         if( ivalin .EQ. iarray(i) ) then
             fndint = i
             goto 9999
         endif
      enddo
c
c   ---- no match, return zero ----
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
