C**** RDTHRES.F version 2.0 09/01/2007
c
      subroutine rdthresh( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /THRESHOLD/
c    packet in common variables for use by the PIGSET program.
c
c    Arguemnt declaration.
c     Outputs:
c       ierr    I  error flag
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
      include 'iopigems.inc'
      include 'usrpigems.inc'
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
c   strmin   I   returns actual length of string (min of 1)
c   fndchr   I   finds a string in an array of strings
c   fndint   I   finds an integer in an array of integers
c
      integer*4 strmin
      integer*4 fndchr
      integer*4 fndint
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keyin
      integer*4           irec, jerr, idxtmp, idxspc, i
      logical*4           lfound
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      lfound = .FALSE.
      nthresh = 0
c
c   --- call routine to get to find /THRESHOLD VALUE/ packet identifier ---
c
      keywrd = '/THRESHOLD VALUE/'
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 6000
c
c  --- read until /END/ keyword encountered ---
c
      irec = irec + 1
      read( IORINP,'(A)',ERR=7000,END=7001 ) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c  --- the first record is the co-location distance ---
c
      read( string(21:),'(F10.0,A10)',ERR=7003 ) colodist, colocpol
      call lftjst( colocpol )
      call low2up( colocpol )
c
c   --- if the colocation distance is negative, co-location
c       is disabled ---
c
      if( colodist .LE. 0 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(4A)',ERR=9999) 'WARNING:  Read a negative ',
     &       'co-location distance in the ',keywrd(:strmin(keywrd)),
     &                                                   ' packet.'
         write(ioerr,'(2A)',ERR=9999) 'The co-location algorithm ',
     &                                          'will be disabled.'
         write(ioerr,'(2A)',ERR=9999) 'Sources can still be PiGged ',
     &                                      'on an individual basis.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      else
c
c   --- find the index of the pollutant in list ---
c
         idxcoloc = fndchr( colocpol, 10, crtnam, ncrthd )
         if( idxcoloc .LE. 0 ) goto 6005
       endif
c
c  --- read until /END/ keyword encountered ---
c
 111  continue
      irec = irec + 1
      read( IORINP,'(A)',ERR=7000,END=7001 ) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 222
c
c  --- check for array overflow and load the data ---
c
      nthresh = nthresh + 1 
      if( nthresh  .GT. MXTHRSH ) goto 7002
      read( string(21:),'(I10,A10,F10.0)',ERR=7003 ) 
     &          ithrshcd(nthresh), thrshspc(nthresh), thrshval(nthresh)
      call lftjst( thrshspc(nthresh) )
      call low2up( thrshspc(nthresh) )
c
c  --- make sure the species is in the inventory --
c
      idxspc = fndchr( thrshspc(nthresh), 10, crtnam, ncrthd )
      if( idxspc .LE. 0 ) goto 6003
      idxthspc(nthresh) = idxspc
      if( idxthspc(nthresh) .EQ. idxcoloc ) lfound = .TRUE.
c
c  --- get next record ---
c
      goto 111
c
c   --- /END/ keyword was found ---
c
 222  continue
c
c   --- if no threshold value is specified write a warning ----
c
      if( nthresh .LE. 0 ) goto 6001
c
c   --- if the colocation pollutant is not in the packet, 
c       write an error message ----
c
      if( colodist .GT. 0. .AND. .NOT. lfound ) goto 7004
c
c   --- call routine to get to find /THRESHOLD MAP/ packet identifier ---
c
 333  continue
      nthrshmap = 0
      keywrd = '/THRESHOLD MAP/'
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 6004
c
c  --- read until /END/ keyword encountered ---
c
 444  continue
      irec = irec + 1
      read( IORINP,'(A)',ERR=7000,END=7001 ) string
      keyin = string(:KEYLEN)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin .EQ. KEYEND ) goto 555
c
c  --- check for array overflow and load the data ---
c
      nthrshmap = nthrshmap + 1
      if( nthresh  .GT. MXTHRSH ) goto 7005
      read( string(21:),'(A10,I10)',ERR=7003 ) thrshcty(nthrshmap), 
     &                                            ithrshmap(nthrshmap)
      call lftjst( thrshcty(nthrshmap) )
c
c  --- get next record ---
c
      goto 444
c
c   --- set error flag to sucess ---
c
  555 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(3A)',ERR=9999) 'WARNING:  No ',
     &      keywrd(:strmin(keywrd)),' packet found in the USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'A master stack list will be ',
     &                       'created, but no sources will be PiGged.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      ierr = ISUCES
      goto 9999
c
 6001 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(3A)',ERR=9999) 'WARNING:  No threshold records',
     &     ' found in the ',keywrd(:strmin(keywrd)),
     &                                'packet of  the USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'A master stack list will be ',
     &                       'created, but no sources will be PiGged.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      ierr = ISUCES
      goto 9999
c
 6003 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(4A)',ERR=9999) 'WARNING:  A pollutant ',
     &               'specified in the ',keywrd(:strmin(keywrd)),
     &                                                 ' packet is not'
      write(ioerr,'(A)',ERR=9999) 'in the inventory.'
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,/)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(10X,A,T40,2A)',ERR=9999) 'Threshold pollutant',': ',
     &                                            thrshspc(nthresh)
      write(ioerr,'(10X,2A)',ERR=9999)  'Pollutants in Inventory',': '
      do i=1,ncrthd
         write(ioerr,'(10X,A,T40,2A)',ERR=9999) ' ',': ',crtnam(i)
      enddo
      write(ioerr,'(/,2A)',ERR=9999) 'This threshold record will',
     &                                                 ' be skipped.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      nthresh = nthresh - 1
      goto 111
c
 6004 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(3A)',ERR=9999) 'WARNING:  No ',
     &      keywrd(:strmin(keywrd)),' packet found in the USERIN file.'
      if( lmaskfl ) then
         write(ioerr,'(2A)',ERR=9999) 'All sources will be mapped ',
     &                       'to threshold codes using the mask file.'
      else 
         write(ioerr,'(2A,I4)',ERR=9999) 'An output emissions file ',
     &                'will be created, but no sources will be PiGged.'
      endif
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999) 
     &                        'Number of threshold values exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXTHRSH
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXTHRSH ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                    'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                       'packet of the USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                                   ' packet of the USERIN file.'
      write(ioerr,'(3A)',ERR=9999) 'The threshold pollutant for the ',
     &                          'colocation distance is not included'
      write(ioerr,'(3A)',ERR=9999) 'later in the packet.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Colocation pollutant: ',
     &                                                       colocpol
      write(ioerr,'(/,2A)',ERR=9999) 'Please specify one of the ',
     &                      'included threshold pollutants to be used'
      write(ioerr,'(A,//)',ERR=9999) 'for purposes of colocation.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of the USERIN file.'
      write(ioerr,'(A)',ERR=9999)
     &                    'Number of threshold map records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXTHRSH
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXTHRSH ',
     &                                                  'and recompile.'
      goto 9999
c
 6005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of the USERIN file.'
      write(ioerr,'(3A)',ERR=9999) 'The pollutant specified in the ',
     &                        keywrd(:strmin(keywrd)),' packet is not'
      write(ioerr,'(A)',ERR=9999) 'in the inventory.'
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,/)',ERR=9999)  string(:strmin(string))
      write(ioerr,'(10X,A,T40,2A)',ERR=9999) 'Colocation pollutant',
     &                                                 ': ',colocpol
      write(ioerr,'(10X,2A)',ERR=9999)  'Pollutants in Inventory',': '
      do i=1,ncrthd
         write(ioerr,'(10X,A,T40,2A)',ERR=9999) ' ',': ',crtnam(i)
      enddo
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
