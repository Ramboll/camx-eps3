C***** HDRLBS.F 
c
      subroutine hdrlbs( ierr )
c
c-----------------------------------------------------------------------
c
c    Sets the header variables and writes the EMBR header for the
c    LBASE program
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
      include 'embrdat.inc'
      include 'uamregn.inc'
      include 'iolbase.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   cdate   C   character string for date and time
c   jerr    I   error flag for subroutine calls
c   string  C   character string for temporary storage
c
      character*(5*MXSTR) line
      character*(LENREC)  string
      character*40        cdate
      character*10        name
      integer*4           jerr, idxpol
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c  --- initialize some variables ----
c
      filtyp = 'MOBILE'
      curofl = embrfl
      curifl = linkfl
c
c   --- call routine to get date and time string, and create header
c       string ---
c
      call getime(cdate)
      string = VERSON // cdate
c
c   --- set the rest of the header variables ----
c
      filtyp = 'MOBILE'
      rgutme = xorig
      rgutmn = yorig
      irgzn = izone
      nxrgn = nxcell
      nyrgn = nycell
      dxrgn = dxcell
      dyrgn = dycell
c
c   --- read the first record of the links file and parse it ---
c
      read(IORLNK,'(A)',ERR=7000,END=7001) line
      call token( jerr, name, line )
      call low2up( name )
      call lftjst( name )
      if( name .NE. 'SPECIES   ' ) goto 7002
 111  continue
      call token( jerr, name, line )
      if( jerr .EQ. IEOD ) goto 222
      idxpol = fndchr( name, 10, crtnam, ncrthd )
      if( idxpol .LE. 0 ) then
         ncrthd = ncrthd + 1
         if( ncrthd .GT. MXCRIT ) goto 7003
         crtnam(ncrthd) = name
         call low2up( crtnam(ncrthd) )
      endif
      goto 111
c
c   --- call routine to write the the header ---
c
  222 continue
      call wrhdr(jerr,
     &                 string, IOWMBR )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header record ',
     &                                      'of link emissions file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',linkfl(:strmin(linkfl))
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header record ',
     &                                      'of link emissions file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',linkfl(:strmin(linkfl))
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header record ',
     &                                      'of link emissions file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',linkfl(:strmin(linkfl))
      write(ioerr,'(2A)',ERR=9999) 'This version requires that ',
     &           'the pollutant names be listed in the header record.'
      write(ioerr,'(2A)',ERR=9999) 'The first word of the line ',
     &                                  'must be the word "SPECIES".'
      write(ioerr,'(A,//)',ERR=9999)'The pollutant names should follow.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading header record ',
     &                                      'of link emissions file.'
      write(ioerr,'(A)',ERR=9999) 'Number of pollutants exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999)'Maxumium allowed: ',MXCRIT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCRIT ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
