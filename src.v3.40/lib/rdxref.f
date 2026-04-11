C**** RDXREF.F 
c
      subroutine rdxref( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the ASC-SCC/profiles cross refernce file and stores the
c    data in common variables to be used by many of the core EPS
c    modules.
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I unit number of file to read
c
c-----------------------------------------------------------------------
c    LOG:
c
c         1. gwilson      Tue Apr 26 10:56:37 PDT 2005
c            Added point ID to lookup table
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'sccxref.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
c
c-----------------------------------------------------------------------
c    Subroutines called:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) string
      character*10        keywrd
      integer*4           nrec, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- initialize the array counter ---
c
      npfxrf = 0
      nrec = 0
      lxrfpnt = .FALSE.
c
c   --- increment counter and check for array overflow ---
c
  111 continue
      nrec = nrec + 1
      npfxrf = npfxrf + 1
      if( npfxrf .GT. MXREF ) goto 7000
c
c   --- read next record ---
c
      read(iounit,'(A)',ERR=7002,END=222) string
c
c   --- if this is first record, 
c       check to see if this file has point ID's ---
c
      if( npfxrf .EQ. 1 ) then
          keywrd = string(1:10)
          call lftjst( keywrd )
          call low2up( keywrd )
          if( keywrd(1:8) .EQ. 'FACILITY' ) then
             lxrfpnt = .TRUE.
             read(iounit,'(A)',ERR=7002,END=222) string
          endif
      endif
c
c   --- get the data for this record ----
c
      if( lxrfpnt ) then
         read(string,'(A5,1X,4(A10,1X),A)',ERR=7004,END=222) 
     &        fipxrf(npfxrf), facxrf(npfxrf), stkxrf(npfxrf), 
     &            pntxrf(npfxrf),sccxrf(npfxrf), prfxrf(npfxrf)
      else
         read(string,'(A5,1X,3(A10,1X),A)',ERR=7004,END=222) 
     &        fipxrf(npfxrf), facxrf(npfxrf), stkxrf(npfxrf), 
     &                           sccxrf(npfxrf), prfxrf(npfxrf)
      endif

      call rgtjst( sccxrf(npfxrf) )
      call lftjst( facxrf(npfxrf) )
      call lftjst( stkxrf(npfxrf) )
      call lftjst( pntxrf(npfxrf) )
      call lftjst( prfxrf(npfxrf) )
c
c  --- if this is the first record, write out the note about
c      profile code length ---
c
      if( nrec .EQ. 1 ) goto 6000
      goto 111
c
c   --- processed entire file, echo to message file ---
c
  222 continue
      npfxrf = npfxrf - 1
      if( npfxrf .LE. 0 ) goto 7003
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(/,5X,70A)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,3A,I2,T75,A)',ERR=9999) '*','  NOTE: This ',
     &   'version is compiled for profile codes of length: ',LENPRF,'*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  Please verify ',
     &             'that this is the correct length for your data.','*'
      write(ioerr,'(5X,3A,T75,A)',ERR=9999) '*','  The first record ',
     &                        'of your XREF file is shown below: ','*'
      write(ioerr,'(5X,A,T75,A)',ERR=9999) '*','*'
      write(ioerr,'(5X,70A,/)',ERR=9999) ('*',i=1,70)
      write(ioerr,'(/,A,/)',ERR=9999) string(:strmin(string))
      goto 111
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading the ',
     &                     'ASC-SCC/profile code cross reference file.'
      write(ioerr,'(A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading the ',
     &  'ASC-SCC/profile code cross reference file at line: ',npfxrf
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading the ',
     &                     'ASC-SCC/profile code cross reference file.'
      write(ioerr,'(A,//)',ERR=9999)
     &                   'No records found in ASC-SCC/ profiles file.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading the ',
     &  'ASC-SCC/profile code cross reference file at line: ',npfxrf
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
