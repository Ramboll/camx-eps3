C**** CLSCHM.F 
c
      subroutine clschm( )
c
c-----------------------------------------------------------------------
c
c    Closes the filenames used in the CHMSPL program
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iochmspl.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      close(unit=IORINP)
c
      close(unit=IOREMB)
c
      close(unit=IORSPL)
c
      close(unit=IORXRF)
c
      close(unit=IOWMSG)
c
      close(unit=IOWEMB)
c
      close(unit=IOWERR)
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
