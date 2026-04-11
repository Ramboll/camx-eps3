C**** RDCTLF.F 
c
      subroutine rdctlf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the control factors file and stores the data in common
c    blocks to be used by the EMSCVT program.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'ioemscvt.inc'
      include 'usremcvt.inc'
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
c   strmin   I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) string
      integer*4         irec, icodtmp
      real*4            tmpfac
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nctlfac = 0
c
c  --- increase the counter and check for overflow
c
  111 continue
c
c   --- read a line from the file ---
c
      read(IORCTL,'(A)',ERR=7000,END=222) string
      read(string,'(I5,1X,F10.0)',ERR=7001) icodtmp, tmpfac
c
c  --- store in global array ---
c
      irec = irec + 1
      nctlfac = nctlfac + 1
      if( nctlfac .GT. MXCTL ) goto 7002
      ictlcod(nctlfac) = icodtmp
      ctlfac(nctlfac) = tmpfac
      goto 111
c
c  --- echo data to message file ---
c
  222 continue
      write(IOWMSG,'(/,A,I8)',ERR=7003)
     &    'Number of control factors read from control factors file: ',
     &                                                 nctlfac
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading control',
     &                               ' factors file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',ctlfl(:strmin(ctlfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading control',
     &                            ' factors file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',ctlfl(:strmin(ctlfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading control',
     &                                                 ' factors file.'
      write(ioerr,'(A)',ERR=9999) 
     &                     'Number of control factors exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCTL
      write(ioerr,'(3A,//)',ERR=9999) 'Increase the parameter MXCTL ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
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
