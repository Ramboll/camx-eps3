C**** GETIME.F 
c
      subroutine getime( cdate )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine returns the actual length of a string, i.e. with no
c     trailing blanks.
c
c   Arguments:
c
c     Outputs:
c       cdate    C   date as character string
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) cdate
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 imon, iday, iyear, ihr, imin, isec
      integer*4 iarray_date(3), iarray_time(3)
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- call routine to get date/time as integers ---
c
      call idate( iarray_date )
      iday = iarray_date(1)
      imon = iarray_date(2)
      iyear = iarray_date(3)
      call itime( iarray_time )
      isec = iarray_time(3)
      imin = iarray_time(2)
      ihr = iarray_time(1)
      iyear = MOD(iyear,100)
      write(cdate,9000,ERR=9999) imon, iday, iyear, ihr, imin, isec
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(I2.2,'/',I2.2,'/',I2.2,1X,I2.2,':',I2.2,':',I2.2)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
