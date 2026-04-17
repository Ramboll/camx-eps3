C**** RDCOMB.F version 2.0 09/01/2007
c
      subroutine rdcomb( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the 
c    /COMBINE STACKS/ packet in common variables for use by the PIGSET 
c    program.
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
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   KEYNEW  C   keyword that indicates the record is for a new stack
c   PIGYES  C   flag that indicates the new stack should be PiGged
c   PIGNO   C   flag that indicates the new stack should not be PiGged
c
      character*10 KEYNEW
      character*10 PIGYES 
      character*10 PIGNO
c
      parameter( KEYNEW = 'NEW       ' )
      parameter( PIGYES = 'PIG       ' )
      parameter( PIGNO  = 'NOPIG     ' )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   keyword for packet as variable
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keyin
      character*10        pigflag
      integer*4           irec, jerr, idxtmp, i
      logical             lfoundit
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/COMBINE STACKS/'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nstknew = 0
c
c   --- call routine to get to find packet identifier,
c       it is optional so if not found just return ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) then
         ierr = ISUCES
         goto 9999
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
c  --- if the first 10 character is the "NEW" keyword,
c      create record for this new stack ---
c
      keyin = string(:10)
      call low2up( keyin )
      call lftjst( keyin )
      if( keyin(1:3) .EQ. KEYNEW ) then
         nstknew = nstknew + 1 
         if( nstknew  .GT. MXSNEW ) goto 7002
         read( string(12:),'(3(A10,1X),6(F10.0,1X))',ERR=7003 ) 
     &         ctynew(nstknew), facnew(nstknew), stidnew(nstknew),
     &            hgtnew(nstknew), dianew(nstknew), tptnew(nstknew), 
     &               velnew(nstknew), xlocnew(nstknew), ylocnew(nstknew)
         call lftjst( ctynew(nstknew) )
         call lftjst( facnew(nstknew) )
         call lftjst( stidnew(nstknew) )
         lcolonew(nstknew) = .FALSE.
c
c  --- make sure the flag for PiG treatment is acdeptable and
c      set the logical variable accordingly ---
c
         pigflag = keyin(4:)
         call lftjst( pigflag )
         call low2up( pigflag )
         if( pigflag .EQ. PIGYES ) then
            lpignew(nstknew) = .TRUE.
         else if( pigflag .EQ. PIGNO ) then
            lpignew(nstknew) = .FALSE.
         else
            goto 7004
         endif
c
c  --- fix the diameter so that it doesn't automatically PiG ---
c
         if( .NOT. lpignew(nstknew) .AND. 
     &            dianew(nstknew) .LT. 0. ) dianew(nstknew) = 0.
c
c  --- initialize  counter for this new stack ---
c
         nstkcomb(nstknew) = 0
c
c  --- get the next record ---
c
         goto 111
c
c  --- this record contains info about existing stack to combine ---
c
      else
         if( nstknew .LE. 0 ) goto 7005
         nstkcomb(nstknew) = nstkcomb(nstknew) + 1
         if( nstkcomb(nstknew) .GT. MXCOMB ) goto 7006
c
         read( string(12:),'(3(A10,1X))',ERR=7003 ) 
     &         ctycomb(nstknew,nstkcomb(nstknew)), 
     &                 faccomb(nstknew,nstkcomb(nstknew)), 
     &                      stidcomb(nstknew,nstkcomb(nstknew))
      endif
c
c  --- get next record ---
c
      goto 111
c
c   --- set error flag to sucess ---
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
     &                        'Number of "NEW" stacks exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSNEW
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSNEW ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 
     &                     'ERROR: Reading ',keywrd(:strmin(keywrd)),
     &                         ' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,//)',ERR=9999)  string(:strmin(string))
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(2A)',ERR=9999) 'A "NEW" stack record contains an ',
     &                                              'invalid PiG flag.'
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,/)',ERR=9999)  string(:strmin(string))
      write(ioerr,9000,ERR=9999) 'County ID',':',ctynew(nstknew)
      write(ioerr,9000,ERR=9999) 'Facility ID',':',facnew(nstknew)
      write(ioerr,9000,ERR=9999) 'Stack ID',':',stidnew(nstknew)
      write(ioerr,9000,ERR=9999) 'PiG Flag read',':',string(11:20)
      write(ioerr,'(/)',ERR=9999)
      write(ioerr,9000,ERR=9999) 'Acceptable responses are'
      write(ioerr,9000,ERR=9999) ' ',':',PIGYES
      write(ioerr,9000,ERR=9999) ' ',':',PIGNO
      write(ioerr,'(//)',ERR=9999)
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(2A)',ERR=9999) 'There is a record for ',
     &                                        'a new stack to create.'
      write(ioerr,'(2A)',ERR=9999) 'but there are records for ',
     &                                     'existing stacks to combine.'
      write(ioerr,'(2A)',ERR=9999) 'Please supply a record that ',
     &                                    'with the keyword NEW with'
      write(ioerr,'(2A,//)',ERR=9999) 'stack identifiers and stack ',
     &                                  'parameters for the new stack.'
      goto 9999 
c
 7006 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &                   keywrd(:strmin(keywrd)),' packet of USERIN.'
      write(ioerr,'(A)',ERR=9999)
     &                   'Number of new stacks to combine exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCOMB
      write(ioerr,'(2A,/)',ERR=9999) 'Increase the parameter MXCOMB ',
     &                                                  'and recompile.'
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(A,/)',ERR=9999)  string(:strmin(string))
      write(ioerr,9000,ERR=9999) 'New County ID',':',ctynew(nstknew)
      write(ioerr,9000,ERR=9999) 'New Facility ID',':',facnew(nstknew)
      write(ioerr,9000,ERR=9999) 'New Stack ID',':',stidnew(nstknew)
      write(ioerr,'(//)',ERR=9999)
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T30,2A)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
