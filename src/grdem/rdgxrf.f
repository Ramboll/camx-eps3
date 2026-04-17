C**** RDGXRF.F 
c
      subroutine rdgxrf( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the ASC-SCC/surrogate cross refernce file and stores
c    the data in common variables to be used by the GRDEM program.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    May 2008  mjimenez  Changed the surrogate codes from integer to
c                        character field. isgxrf now called srgxrf
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemsrg.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c    rgtjst  --  right justifies a character string (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin      I  returns actual length of string (min of 1)
c   fndhash_xrf I returns the index of the record in the hash arrays
c
      integer*4 strmin
      integer*4 fndhash_xrf
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXSTR) string
      character*5       fiptmp, subtmp
      character*10      scctmp
      character*3       srgtmp(3)
      integer*4         idxsrg, jerr, i, irec
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   ---- initialize the hash table entries ---
c
      do i=1,MXHASH_PAIR
         idxhsh_xrf_first_pair(i) = IMISS
      enddo
      do i=1,MXREF
         idxhsh_xrf_next_pair(i) = IMISS
      enddo
c
c   --- initialize the array counter ---
c
      irec = 0
      nsgxrf = 0
  111 continue
c
c   --- read next record ---
c
      read(IORXRF,'(A)',ERR=7001,END=222) string
      irec = irec + 1
      if ( string(1:1) .EQ. '#' )  goto 111
c
      if( lsrgmult ) then 
          read(string,'(2(A5,1X),3A3,1X,A10)',ERR=7005) fiptmp,
     &                           subtmp, (srgtmp(i),i=1,3), scctmp
          call lftjst( srgtmp(1) )
          call low2up( srgtmp(1) )
          call lftjst( srgtmp(2) )
          call low2up( srgtmp(2) )
          call lftjst( srgtmp(3) )
          call low2up( srgtmp(3) )
          call rgtjst( scctmp )
      else
          read(string,'(2(A5,1X),A3,1X,A10)',ERR=7002) fiptmp,
     &                                   subtmp, srgtmp(1), scctmp
          call lftjst( srgtmp(1) )
          call low2up( srgtmp(1) )
          call rgtjst( scctmp )
      endif
c
c   --- get hash entry for this pair, it will be added
c       to the array of not found  ---
c
      idxsrg = fndhash_xrf( jerr, fiptmp, subtmp, scctmp ) 
      if( jerr .NE. ISUCES ) goto 9999
c
      fipxrf(idxsrg)(1:5) = fiptmp
      fipxrf(idxsrg)(6:10) = subtmp
      srgxrf(idxsrg,1) = srgtmp(1)
      srgxrf(idxsrg,2) = srgtmp(2)
      srgxrf(idxsrg,3) = srgtmp(3)
      sccxrf(idxsrg) = scctmp
c
c   --- read next record ---
c
      goto 111
c
c   --- processed entire file, echo to message file ---
c
  222 continue
c
      if( nsgxrf .LE. 0 ) goto 7003
      write(IOWMSG,'(/,T10,A,I8)',ERR=7004) 
     &        'Number of ASC-SCC/surrogate XREF records read: ',nsgxrf
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
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &                       'ASC-SCC/surrogate cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &           'ASC-SCC/surrogate cross reference records exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     &                 'ERROR:  Reading ASC-SCC/surrogate cross ',
     &                               'reference file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',xrffl(:strmin(xrffl))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     &                 'ERROR:  Reading ASC-SCC/surrogate cross ',
     &                               'reference file at line:',irec
      write(ioerr,'(2A)',ERR=9999) 'File: ',xrffl(:strmin(xrffl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &                       'ASC-SCC/surrogate cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'No ASC-SCC/surrogate ',
     &                              'cross reference records found.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',xrffl(:strmin(xrffl))
      goto 9999
c
 7004 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7005 continue
      write(ioerr,'(/,2A,I8)',ERR=9999)
     &                 'ERROR:  Reading ASC-SCC/surrogate cross ',
     &                               'reference file at line:',irec
      write(ioerr,'(2A)',ERR=9999) 'File: ',xrffl(:strmin(xrffl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      write(ioerr,'(2X,2A,/,A)',ERR=9999) 'The format for the ',
     &          'surrogate cross reference file is different when ',
     &                         'using multiple surrogate codes.'
      write(ioerr,'(2X,2A,/,A)',ERR=9999) 'You need a field for the ',
     &                                   'secondary and tertiary codes.'           
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
