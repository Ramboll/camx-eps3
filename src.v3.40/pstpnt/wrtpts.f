C**** WRTPTS.F 
c
      subroutine wrtpts( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the header stack information file
c
c    Argument Declaration.
c     Outputs:
c       ierr     I error flag
c     Inputs:
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'iopstpnt.inc'
      include 'pstpntstk.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c    strmin  I   returns the true length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) strtmp, string, ptsrce
      character*10        fiptmp, keypts
      integer*4           jerr, irec, icount, i
      real*4              xcoord, ycoord
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keypts /'ORIGINAL '/    
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- write out stack data ---
c
      icount = 0
      do 10 i=1,nstack
         if( ptflag(i) .NE. 'E' ) goto 10
         icount = icount + 1
         fiptmp = infips(i)
         call lftjst( fiptmp )
         call lftjst( inplnt(i) )
         call lftjst( stckin(i) )
         if( izone .GE. 0 ) then
            xcoord = xloc(i) * 1000
            ycoord = yloc(i) * 1000
            write(IOWHDR,9000,ERR=7000) keypts, fiptmp, inplnt(i), 
     &                     stckin(i), stkht(i), stdiam(i), stextp(i),
     &                                         stexvl(i), xcoord, ycoord
         else
            xcoord = xloc(i)
            ycoord = yloc(i)
            write(IOWHDR,9001,ERR=7000) keypts, fiptmp, inplnt(i), 
     &                     stckin(i), stkht(i), stdiam(i), stextp(i),
     &                                         stexvl(i), xcoord, ycoord
         endif
  10  continue
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Writing output stack header file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',ohdrfl(:strmin(ohdrfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9000 format(4(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X))
 9001 format(4(A10,1X),F10.1,1X,F10.3,1X,2(F10.0,1X),2(F10.4,1X))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
