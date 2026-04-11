C***** CLSPIG.F version 2.0 09/01/2007
c
      subroutine clspig()
c
c-----------------------------------------------------------------------
c
c    Closes all files used by the PIGEMS program
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
      include 'iopigems.inc'
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
      if( lmaster ) close(IORMSTR)
c
      if( lmaskfl ) close(IORMSK)
c
      close(IOWMSG)
c
      close(IOWEMS)
c
      if( .NOT. lmaster ) close(IOWSTK)
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
