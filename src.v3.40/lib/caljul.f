C**** CALJUL.F 
c
      function caljul(idate)
      integer*4 caljul
c
c-----------------------------------------------------------------------
c
c    Converts calander date to Julian date
c
c        Return value:   Julian date.
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
      integer*4 iyr, imon, iday, jdate, ipt, ileap, i
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
      caljul = IMISS
      jdate = 0
      iyr = idate/10000
      imon = (idate - iyr*10000)/100
      iday = idate - iyr*10000 - imon*100
c
c  --- make sure the parts make sense ---
c
      if( imon .LE. 0 .OR. imon .GT. 12 ) goto 9999
      if( iday .LE. 0 .OR. iday .GT. 31 ) goto 9999
c
c   ----- determine if it is leap year -----
c
      ileap = MOD(iyr,4)
      if( ileap .EQ. 0 ) then
         ipt = 2
      else
         ipt = 1
      endif
c
c   ----- add up days in months, before this month ----
c
      do i=1,imon-1
         jdate = jdate + modays(ipt,i)
      enddo
c
c   ----- add days from this month ----
c
      jdate = jdate + iday
c
c   ----- return Julian date ----
c
      caljul = iyr * 1000 + jdate
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
