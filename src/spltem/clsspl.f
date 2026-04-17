C**** CLSSPL.F
c
      subroutine clsspl()
c
c-----------------------------------------------------------------------
c
c    Closes the filenames used in the SPLTEM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/11/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      close(unit=IORINP)
c
      close(unit=IORMBR)
c
      close(unit=IORPRW)
c
      close(unit=IORXRF)
c
      close(unit=IORCDB)
c
      close(unit=IOWMSG)
c
      close(unit=IOWDAT)
c
      close(unit=IOWELE)
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
