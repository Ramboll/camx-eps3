C**** CLSGRD.F 
c
      subroutine clsgrd()
c
c-----------------------------------------------------------------------
c
c    Closes all files used by the GRDEM program
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
      include 'iogrdem.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- close files ---
c
      close(IORINP)
c
      close(IORMBR)
c
      close(IORXRF)
c
      close(IORSRG)
c
      close(IORLNK)
c
      close(IOWMSG)
c
      if( loutfile ) close(IOWEMS)
c
      close(IOWERR)
c
      close(IOWASC)
c
      if( lnetcdf_out ) ierr = nf_close(ncf_iowemis)
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
