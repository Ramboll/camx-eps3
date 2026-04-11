C**** WRTOUT.F 
c
      subroutine wrtout( ierr ,
     &                         ihour)
c
c-----------------------------------------------------------------------
c
c    Writes the merged emissions to the output file.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I  error flag
c     Inputs:
c       ihour  I  number between 1 and 24 for hour
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'iomrguam.inc'
      include 'mrguemis.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 ihour
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 nseg, ispc, i, j, ibeg, iend
      real*4    bt, et
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
c
c   --- initialize ---
c
      nseg = 1
c
c   --- time record ---
c
      bt = ihour - 1.0
      et = ihour
      ibeg = ibgdat
      iend = ibgdat
      write(IOWUAM,ERR=7000) ibeg, bt, iend, et
c
c   --- loop through species.
c       the order is from the region packet ---
c
      do ispc=1,nspecs
          write(IOWUAM,ERR=7000) nseg, (intnam(ispc,i),i=1,10),
     &                      ((emisout(i,j,ispc),i=1,nxcell),j=1,nycell)
      enddo
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Writing hourly data of ',
     &                                        'input emissions file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',ouamfl(:strmin(ouamfl))
      write(ioerr,'(A,I5,//)',ERR=9999) 'Writing hour: ',ihour
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
