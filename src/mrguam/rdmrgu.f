C**** RDMRGU.F 
c
      subroutine rdmrgu( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the /MRGUAM/ packet of USERIN and stores data in common
c    blocks to be used by the MRGUAM program.
c
c    Argument declaration.
c     Outputs:
c       ierr   I  error flag
c     Inputs:
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iomrguam.inc'
      include 'usrmrguam.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*(15*MXSPEC) line
      character*10          crtpol
      character*(KEYLEN)    keywrd, keychk, respon
      integer*4             jerr, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/MRGUAM/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
      lchkhd = .TRUE.
c
c  --- call library routine to read the user input file
c      for /SPECIES LIST/ packet ---
c
      call rdspec(jerr,IORINP, .FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- call library routine to read the user input file
c      for /DATE/ packet ---
c
      call rddate(jerr,IORINP)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- call library routine to read the user input file
c      for /REGION/ packet ---
c
      call rdregn(jerr,IORINP)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set the flags for the tables we want ---
c
      lcrtasc(IDXFMT) = .TRUE.
      lspcasc(IDXFMT) = .TRUE.
      lcrthr(IDXFMT) = .TRUE.
      lspchr(IDXFMT) = .TRUE.
      lcrthcd(IDXFMT) = .TRUE.
      lspchcd(IDXFMT) = .TRUE.
c
      lcrtasc(IDXCSV) = .FALSE.
      lspcasc(IDXCSV) = .FALSE.
      lcrthr(IDXCSV) = .FALSE.
      lspchr(IDXCSV) = .FALSE.
      lcrthcd(IDXCSV) = .FALSE.
      lspchcd(IDXCSV) = .FALSE.
c
c   --- call routine to find /MRGUAM/ keyword ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd)
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) then 
          lchkhd = .FALSE.
          goto 222
      endif
c
c  ---- Read the check header flag ---
c
      irec = 1
      read(IORINP,'(A20,A)',ERR=7001,END=7002) keychk, respon
      call lftjst( respon )
      call low2up( respon )
      if( respon(1:3) .EQ. YES ) then
          lchkhd = .TRUE.
      else if( respon(1:3) .EQ. NO ) then
          lchkhd = .FALSE.
      else
          goto 7003
      endif
c
c  --- file sucessfully read ----
c
  222 continue
      if( nspecs .LE. 0 ) goto 7007
      ierr = ISUCES
      return
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &            keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999)
     &                   'Invalid response to check header flag: ',
     &                                      line(21:strmin(line))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Reading the species list.'
      write(ioerr,'(A,/,A,//)',ERR=9999) 'Line read: ',
     &                                         line(:strmin(line))
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 
     &                   'Number of criteria pollutants exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCRIT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCRIT ',
     &                                                  'and recompile.'
      goto 9999
c
 7007 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'No output species were listed.'
      write(ioerr,'(2A,/,A,//)',ERR=9999) 'This version of MRGUAM ',
     &      'requires that the output species be defined in the ',
     &                            '/MRGUAM/ packet of the USERIN file.'
      goto 9999
c
 7008 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(3A)',ERR=9999) 'Not enough information was ',
     &                    'provided for species: ',spcnam(nspecs)
      write(ioerr,'(2A,/,A,//)',ERR=9999) 'In addition to the ',
     &                 'species name you must also provide the ',
     &                   'criteria pollutant and the molecular weight.'
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
