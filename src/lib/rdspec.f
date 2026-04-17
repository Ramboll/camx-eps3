C**** RDSPEC.F 
c
      subroutine rdspec( ierr,
     &                         iounit, lchkcrt )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /SPECIES LIST/
c    packet in common variables
c
c    Arguemnt declaration.
c     Outputs:
c       ierr    I  error flag
c     Inputs:
c       iounit  I  logical unit number of file to read
c       lchkcrt L  flag to determine if criteria pollutants should be
c                  checked for consistancy (MRGUAM = NO)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    jan 2019 gwilson added variables for species ggas flag
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamcntl.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
      logical*4 lchkcrt
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns actual length of string (min of 1)
c   fndchr   I   finds a string in an array of strings
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keyin
      character*10        tmpspc, gasflag
      integer*4           irec, jerr, idxtmp, ispc, i
      logical             lfound(MXSPEC)
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/SPECIES LIST/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nspecs = 0
c
c   --- call routine to get to find packet identifier ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c  --- read until /END/ keyword encountered ---
c
 111  continue
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7002 ) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c  --- check for array overflow and load the data ---
c
      nspecs = nspecs + 1 
      if( nspecs .LE. 0 .OR. nspecs .GT. MXSPEC ) goto 7003
      read( string(21:),'(A10,A10,F10.0,A)',ERR=7004 ) spcnam(nspecs),
     &                            spcrit(nspecs), molwt(nspecs), gasflag
      tmpspc = spcrit(nspecs)
      call lftjst( tmpspc )
      call low2up( tmpspc )
      call lftjst( spcnam(nspecs) )
c
c  --- if not checking criteria, load the criteria names ---
c
      if( .NOT. lchkcrt ) then
         idxtmp = fndchr( tmpspc, 10, crtnam, ncrthd )
         if( idxtmp .LE. 0. ) then
            ncrthd = ncrthd + 1
            if( ncrthd .GT. MXCRIT ) goto 7006
            crtnam(ncrthd) = tmpspc
          endif
      endif
c
c  --- if there is a species list, get the index of criteria pollutant --
c
      ispcrt(nspecs) = 0
      do i=1,ncrthd
        if( tmpspc .EQ. crtnam(i) ) ispcrt(nspecs) = i
      enddo
      if( lchkcrt .AND. ispcrt(nspecs) .EQ. 0 ) goto 7005
c
c  --- set species type to default and see if gas flag is present ----
c
      lgas(nspecs) = .TRUE.
      lfound(nspecs) = .FALSE.
      call lftjst( gasflag )
      call low2up( gasflag )
      if( gasflag(1:3) .EQ. 'GAS' ) then
         lgas(nspecs) = .TRUE.
         lfound(nspecs) = .TRUE.
      else if( gasflag(1:2) .EQ. 'PM' ) then
         lgas(nspecs) = .FALSE.   
         lfound(nspecs) = .TRUE.
      endif
c
c  --- get next record ---
c
      goto 111
c
c   --- write warning for species set to gas by default ---
c
 222  continue
      do ispc=1,nspecs
         if( .NOT. lfound(ispc) ) then
            write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
            write(ioerr,'(4A)',ERR=9999) 'WARNING: By default, the ',
     &             'species: ',spcnam(ispc)(:strmin(spcnam(ispc))),
     &                          ' will be treated as a GAS speicies.'
            write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         endif
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999)
     &         'This program requires the ',keywrd(:strmin(keywrd)),
     &                                            ' packet of USERIN.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
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
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in USERIN: ',nspecs
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A)',ERR=9999) string(:strmin(string))
      write(ioerr,'(3A)',ERR=9999) 'Criteria pollutant for species: ', 
     &                      spcnam(nspecs)(:strmin(spcnam(nspecs))),
     &                            ' does not appear to be in the file.'
      write(ioerr,'(2A)',ERR=9999) 'Associated criteria pollutant: ',
     &                           spcrit(nspecs)(:strmin(spcrit(nspecs)))
      write(ioerr,'(A)',ERR=9999) 'Criteria pollutants in the file:'
      do i=1,ncrthd
          write(ioerr,'(20X,A)') crtnam(i)(:strmin(crtnam(i)))
      enddo
      write(ioerr,'(//)',ERR=9999)
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
