C**** JULCAL.F 
c
      function julcal(idate)
      integer*4 julcal
c
c-----------------------------------------------------------------------
c
c   Converts julian date to calender date
c
c        Return value:   calender date.
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
c
c-----------------------------------------------------------------------
c   Argument declerations:
c-----------------------------------------------------------------------
c
      integer*4 idate
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 modays(2,12)
      integer*4 iyr, imon, iday, jday, jdate, ipt, ileap, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data modays /31, 31, 28, 29, 31, 31, 30, 30, 31, 31, 30, 30,
     &             31, 31, 31, 31, 30, 30, 31, 31, 30, 30, 31, 31/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ----- get year, month, day ----
c
      julcal = IMISS
      jdate = 0
      iyr = idate/1000
      jday = idate - iyr*1000
c
c   ----- determine if it is leap year -----
c
      ileap = MOD(iyr,4)
      if( ileap .EQ. 0 ) then
         ipt = 2
      else
         ipt = 1
      end if
c
c   ----- add up days in months, before this month ----
c
      do i=1,12
         jdate = jdate + modays(ipt,i)
         if( jdate .GT. jday ) then
             imon = i
             jdate = jdate - modays(ipt,i)
             iday = jday - jdate
             julcal = iyr*10000 + imon*100 + iday
             goto 9999
         elseif( jdate .EQ. jday ) then
             imon = i
             iday = modays(ipt,i)
             julcal = iyr*10000 + imon*100 + iday
             goto 9999
          endif
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
