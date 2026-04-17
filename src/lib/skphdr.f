C**** SKPHDR.F 
c
      subroutine skphdr( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads and skips the header data in an EMBR file.  No data is stored
c    written.
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4          ierr
      integer*4          iounit
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   KEYDAT  C   character string for /DATA/ keyword
c
       character*(KEYLEN) KEYDAT
c
       parameter( KEYDAT = '/DATA/' )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c
      character*(LENREC) string
      integer*4          irec
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
c
c   --- read rest of header until an /DATA/ is found ---
c
  111 continue
      irec = irec + 1
      read( iounit,END=7001,ERR=7000) string
      if( string(:KEYLEN) .EQ. KEYDAT ) goto 222
      goto 111
c
c   --- set error flag to sucess ---
c
  222 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999) 
     &            'ERROR: Reading header of EMBR file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Premature end-of-file encountered.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
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
