C**** RDCNTL.F 
c
      subroutine rdcntl( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /EPISODE/
c    packet in common variables
c
c    Arguemnt declaration.
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
      include 'uamcntl.inc'
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
c    External functions:
c-----------------------------------------------------------------------
c
c   strmin   I   returns actual length of string
c
      integer*4 strmin
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c   nhours  I   number of hours in episode
c   idxspc  I   index for species array
c   jerr    I   error flag for subroutine calls
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd
      integer*4           idxspc, nhours, irec, jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/EPISODE/'/
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
c   --- call routine to get to find packet identifier ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c   --- read file note (reading through unused data) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 ) 
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 )
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(A60)',ERR=7006 ) fnote
c
c   --- read species data (checking for array overflow) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(I10)',ERR=7006 ) nspecs
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 )
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 )
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 )
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(20X,A)',ERR=7006 )
      if( nspecs .LE. 0 .OR. nspecs .GT. MXSPEC ) goto 7002
      do idxspc=1,nspecs
         irec = irec + 1
         read( iounit,'(A)',ERR=7001,END=7005 ) string
         read( string,'(A10)',ERR=7006 ) spcnam(idxspc)
      enddo
c
c   --- read time data (checking for valid data) ---
c
      irec = irec + 1
      read( iounit,'(A)',ERR=7001,END=7005 ) string
      read( string,'(2(I10,F10.0))',ERR=7007 ) ibgdat, begtim,
     &                                                    iendat, endtim
c
c   --- make sure the data/times use consistent format ---
c
      if( endtim .EQ. 0. ) then
         iendat = iendat - 1
         endtim = 2400.
      endif
      if( ibgdat .GT. iendat ) goto 7003
      nhours = (iendat-ibgdat)*24 + INT( (endtim-begtim)/100 )
      if( nhours .GT. MXHOUR ) goto 7004
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
     &                                             ' packet of USERIN.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Number of species exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Species in USERIN: ',nspecs
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPEC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPEC ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Invalid julian dates.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Beginning date: ',ibgdat
      write(ioerr,'(10X,A,I10,//)',ERR=9999) 'Ending date   : ',iendat
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 'Number of hours exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Hours in episode: ',nhours
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXHOUR
      write(ioerr,'(A,//)',ERR=9999) 
     &                  'EPS does allows only a single day episode.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      goto 9999
c
 7007 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(2A)',ERR=9999)  '    Make sure the number ',
     &                                       'of species is correct.'
      write(ioerr,'(A,I2,A,//)',ERR=9999)  '    There should be: ',
     &                                  nspecs,' species in the list.'
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
