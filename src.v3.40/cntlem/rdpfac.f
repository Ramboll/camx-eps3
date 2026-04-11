C**** RDPFAC.F 
c 
      subroutine rdpfac (ierr, ncdcod, codfip, codsic, gfac,
     &                                                 nmxctl, keywrd)
c
c-----------------------------------------------------------------------
c
c    Reads the projection factors control factor packet and stores the
c    data in common
c
c    Argument Declaration.
c     Outputs:
c       ierr    I  error flag
c       ncdcod  I  number of input control factor records
c       codfip  A  array of fip/subregion codes
c       codsic  A  array of SIC
c       gfac    R  array of projection factors
c     Inputs:
c       nmxctl  I  maximum number of control factors for array dimension
c       keywrd  A  search string for packet keyword
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
      include 'iocntlem.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      character*(KEYLEN)  keywrd
      integer*4           nmxctl
      character*10        codfip(nmxctl)
      character*10        codsic(nmxctl)
      integer*4           ierr
      integer*4           ncdcod
      real*4              gfac(nmxctl)
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
      character*(2*MXSTR) ctlstr
      character*(KEYLEN)  keychk
      character*10        inlng, insic
      character*5         infip, insbrg
      integer*4           irec, jerr, i, idxlng
      real*4              gfacin
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c   --- call routine to find keywrd packet
c       not finding it is not an error, since it is optional ---
c
      irec = 0
      ncdcod = 0
c
      call fndkey( jerr,
     &                   IORCTL, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 222
c
c   --- read the project records ---
c
 111  continue
      irec = irec + 1
      read(IORCTL,'(A)',END=222,ERR=7000) ctlstr
      keychk = ctlstr(:KEYLEN)
      call lftjst( keychk )
      call low2up( keychk )
      if( keychk .EQ. KEYEND ) goto 222
c
      read (ctlstr,8000,ERR=7001) infip, insbrg, insic, gfacin
      call lftjst( insic )
c
c  --- create the long FIPS code ---
c
      call lftjst( infip )
      call lftjst( insbrg )
      inlng(1:5) = infip
      inlng(6:10) = insbrg
c
      idxlng = 0
      do i=1,ncdcod
        if( inlng .EQ. codfip(i) .AND. insic .EQ. codsic(i) ) idxlng = i
      enddo
c
c  ---  not found so adding one to the list ---
c
      if( idxlng .LE. 0 ) then
         ncdcod = ncdcod + 1
         if( ncdcod .GT. NMXCTL) goto 7002
         codfip(ncdcod) = inlng
         codsic(ncdcod) = insic
         gfac(ncdcod) = gfacin
c
c   ---  found a duplicate record, give user message then skip  ---
c
      else
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  Reading control ',
     &                                              'factors file.'
         write(ioerr,'(3A)',ERR=9999) 'Duplicate ',
     &             keywrd(:strmin(keywrd)),' control record read.'
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
c   --- write control count to message file ---
c
 222  continue
      write(IOWMSG,9001,ERR=7003) 'Control factors records read from ',
     &      keywrd(:strmin(keywrd)),' packet: ', irec,' with ', ncdcod,
     &                                               ' unique controls'
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
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',NMXCTL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter NMXCTL ',
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
 8000 format(2(A5,1X),A10,1X,F10.0)
 9001 format(1X,3(:,A),I5,A,I5,A,/)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
