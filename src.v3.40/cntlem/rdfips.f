C**** RDFIPS.F 
c
      subroutine rdfips( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the FIPS code packet and stores the data in common
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
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'hash.inc'
      include 'iocntlem.inc'
      include 'ctldsc.inc'
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
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   temporary storage for seaching fo keyword
c   jerr    I   error flag for subroutine calls
c   facin   R   temporary variable for factors
c   lngfip  C   temporary variable for long FIPS code
c
      character*(2*MXSTR) ctlstr
      character*(KEYLEN)  keywrd, keychk
      character*10        lngfip
      character*5         infip, insbrg
      integer*4           jerr, idxlng, irec, ihbeg, ihend, i
      real*4              facin(MXCRIT)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- call routine to find /FIPS CODE/ packet ---
c       not finding it is not an error, since it is optional ---
c
      irec = 0
      ncdcty = 0
      keywrd = '/FIPS CODE/'
c
      call fndkey( jerr,
     &                   IORCTL, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 222
c
c   --- read the FIPS code records ---
c
 111  continue
      irec = irec + 1
      read(IORCTL,'(A)',END=222,ERR=7000) ctlstr
      keychk = ctlstr(:KEYLEN)
      call lftjst( keychk )
      call low2up( keychk )
      if( keychk .EQ. KEYEND ) goto 222
c
      read(ctlstr,8000,ERR=7001) infip, insbrg, ihbeg, ihend, 
     &                                          (facin(i),i=1,ncrthd)
      if( ihbeg .LT. 0 ) ihbeg = 0
      if( ihend .LE. 0 ) ihend = 24
c
c  --- create long FIPS code ---
c
      call lftjst( infip )
      call lftjst( insbrg )
      lngfip(1:5) = infip
      lngfip(6:10) = insbrg
c
c  --- check for existing codes ---
c
      idxlng = 0
      do i=1,ncdcty
         if( ctyfip(i) .EQ. lngfip .AND. ihrcty(i,1) .EQ. ihbeg .AND. 
     &                               ihrcty(i,2) .EQ. ihend ) idxlng = i
      enddo
c
c  ---  not found so adding one to the list ---
c
      if( idxlng .LE. 0 ) then
          ncdcty = ncdcty + 1
          if (ncdcty .GT. MXCTY) go to 7002
          ctyfip(ncdcty) = lngfip
          do i=1,ncrthd
             ctyfac(i,ncdcty) = facin(i)
          enddo
          ihrcty(ncdcty,1) = ihbeg
          ihrcty(ncdcty,2) = ihend
c
c   ---  found a duplicate record, give user message then skip  ---
c
      else
          write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
          write(ioerr,'(2A)',ERR=9999) 'WARNING:  Reading control ',
     &                                              'factors file.'
          write(ioerr,'(2A)',ERR=9999) 
     &         'Duplicate /FIPS CODE/ control record read.'
          write(ioerr,'(10X,A,/,A)',ERR=9999) 'Line read: ',
     &                                          ctlstr(:strmin(ctlstr))
          write(ioerr,'(2A)',ERR=9999) 'This record will be skipped.'
          write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
          nwarn = nwarn + 1
      endif
c
c   --- read next record ---
c
      goto 111
c
c   --- write count to message file ---
c
 222  continue
      write(IOWMSG,9001,ERR=7003)
     &       'Control factors records read from /FIPS CODE/ packet: ',
     &               MAX(0,irec-1), ' with ',ncdcty,' unique controls.'
c
c   ---- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,4A,I8)',ERR=9999) 'ERROR:  Reading ',
     &               keywrd(:strmin(keywrd)),' packet of the ',
     &                         'control factors file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',ctlfl(:strmin(ctlfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,4A)',ERR=9999) 'ERROR:  Reading ',
     &               keywrd(:strmin(keywrd)),' packet of the ',
     &                                      'control factors file.'
      write(ioerr,'(2A)',ERR=9999)  'File: ',ctlfl(:strmin(ctlfl))
      write(ioerr,'(A,/,A,//)',ERR=9999)  'Line read: ',
     &                                       ctlstr(:strmin(ctlstr))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,4A)',ERR=9999) 'ERROR:  Reading ',
     &               keywrd(:strmin(keywrd)),' packet of the ',
     &                                      'control factors file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                                   'control factors exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXCTY
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCTY ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(A5,1X,A5,2I5,10(:,F10.0))
 9000 format(/,1X,A,I5,A,/,2A,/)
 9001 format(1X,2(A,I3),A,/)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
