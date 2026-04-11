C**** RDESPL.F 
c
      subroutine rdespl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the existing splits file and stores the data into the common
c    variables to be used by the EMSCVT program.  The species list in
c    the splits file is compared with the requested species list for
c    compatability. An error ocuurs if the species lists are
c    inconsistent.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
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
      include 'emcvtspl.inc'
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
c   fndchr   I   returns the index of a string in an array of strings
c   strmin   I   returns the actual lenght of a string
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   indspl  I  array of indexes of current splits file species list in
c              requested species list
c
      character*(5*MXSTR) string
      integer*4           indspl(MXSPEC), i, irec, idx
      logical*4           lfound(MXSPEC)
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data lfound /MXSPEC*.FALSE./
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      nfac = 0
      irec = 1
c
c  --- read species information ---
c
      read(IORSPL,'(A)',ERR=7000,END=7001) string
      read(string,'(I5)',ERR=7002) nsplit
      do i=1,nsplit
          irec = irec + 1
          read(IORSPL,'(A)',ERR=7000,END=7001) string
          read(string,8000,ERR=7002) splnam(i), splmol(i), splcrt(i)
      enddo
c
c  --- set up index of criteria pollutants ---
c
      do i=1,nsplit
          call low2up( splnam(i) )
          call lftjst( splnam(i) )
          call low2up( splcrt(i) )
          call lftjst( splcrt(i) )
          idx = fndchr( splnam(i), 10, spclst, nspecs )
          if( idx .LE. 0 ) goto 7003
          indspl(i) = idx
          lfound(idx) = .TRUE.
       enddo
c
c  --- check for requested species that do are not in splits file ---
c
      do i=1,nspecs
         if( .NOT. lfound(i) ) goto 7003
      enddo
c
c  --- read the splits values, use index into requested species list ---
c
  111 continue
      irec = irec + 1
      nfac = nfac + 1
      if( nfac .GT. MXSPL ) goto 7004
      read(IORSPL,'(A)',ERR=7005,END=222) string
      read(IORSPL,8001,ERR=7006) prfspl(nfac),
     &                             (spltab(nfac,indspl(i)),i=1,nsplit)
      call lftjst( prfspl(nfac) )
      goto 111
c
c  --- add any species to splits list that will use defaults ---
c
  222 continue
c
c  --- write count to message file ---
c
      nfac = MAX(nfac-1,0)
      write(IOWMSG,'(/,A,I8)',ERR=7007)
     &                 'Number of factors read from splits file:', nfac
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading header of',
     &                      ' input split factors file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',isplfl(:strmin(isplfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading header of',
     &                      ' input split factors file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',isplfl(:strmin(isplfl))
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading header of',
     &                      ' input split factors file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',isplfl(:strmin(isplfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading header of',
     &                                 ' input split factors file.'
      write(ioerr,'(2A)',ERR=9999) 'The species list on file is ',
     &              'not consistent with the requested species list '
      write(ioerr,'(A)',ERR=9999) 'in the user input file.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please reconcile the ',
     &                                   'differences and try again.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading input',
     &                                       ' split factors file.'
      write(ioerr,'(A)',ERR=9999) 'Number of profiles exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPL
      write(ioerr,'(3A,//)',ERR=9999) 'Increase the parameter MXSPL ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading data of',
     &                      ' input split factors file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',isplfl(:strmin(isplfl))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading data of',
     &                      ' input split factors file at line: ',irec
      write(ioerr,'(2A)',ERR=9999)  'File: ',isplfl(:strmin(isplfl))
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A)',ERR=9999) string(:strmin(string))
      write(ioerr,'(10X,2A,I2,//)',ERR=9999) 'Make sure the profile ',
     &                                   'codes are of length: ',LENPRF
      goto 9999
c
 7007 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(300(:,A10,1X,F5.0,1X,A5))
 8001 format(A,300(:,F10.0))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
