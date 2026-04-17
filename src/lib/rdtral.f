C**** RDTRAL.F 
c
      subroutine rdtral( ierr,
     &                         inunit, iounit, nrec )
c
c-----------------------------------------------------------------------
c
c    Reads the TRAILER data from an EMBR file and writes all records
c    to an output EMBR file
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       inunit I logical unit number of file to read
c       iounit I logical unit number of file to write
c       nrec   I number of records in file
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
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
      integer*4 inunit
      integer*4 nrec
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   string  C   character string for temporary storage
c   keywrd  C   character string for /TRAILER/ keywrd
c
      character*(LENREC) string
      character*(KEYLEN) keywrd
      integer*4          irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/TRAILER/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
      irec = 1
c
c   --- write keyword for TRAILER section ---
c
      string =  keywrd(:strmin(keywrd))
      write(iounit,ERR=7001) string
c
c   --- write the number of records in file ---
c
      write(string,'(I10)',ERR=7001) nrec
      write(iounit,ERR=7001) string
      read(inunit,END=222,ERR=7000) string
c
c   --- read and write each record until end of file is found ---
c
  111 continue
      irec = irec + 1
      read(inunit,END=222,ERR=7000) string
      write(iounit,ERR=7001) string
      goto 111
c
c   --- set error flag to success ----
c
 222  continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading TRAILER of ',
     &                                     'EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curifl(:strmin(curifl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Writing TRAILER of ',
     &                                     'EMBR file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',curofl(:strmin(curofl))
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
