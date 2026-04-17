C**** RDSRC.F 
c
      subroutine rdsrc( ierr )
c
c-----------------------------------------------------------------------
c
c    reads the source category code packet and stores the data in common
c
c    Argument declaration.
c     Outputs:
c       ierr  I error flag
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
      include 'embrdat.inc'
      include 'hash.inc'
      include 'iocntlem.inc'
      include 'ctldsc.inc'
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
c   strmin       I   returns the actual length of a string
c   mkhash_pair  I   returns the hash value for the FIPS/ASC pair
c
      integer*4 strmin
      integer*4 mkhash_pair
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c   keywrd  C   temporary storage for seaching fo keyword
c   jerr    I   error flag for subroutine calls
c   facin   R   temporary variable for factors
c   lngfip  C   temporary variable for long FIPS code
c   inplt   C   temporary variable for plant/facility ID
c   instk   C   temporary variable for stack ID
c   inpnt   C   temporary variable for point ID
c   inscc   C   temporary variable for reading ASC/SCC code
c   insic   C   temporary variable for reading ASC/SIC code
c
      character*(KEYLEN)  keywrd, keychk
      character*(2*MXSTR) ctlstr
      character*10        lngfip, inscc, inplt, instk, inpnt
      character*5         infip, insbrg
      character*4         insic
      integer*4           jerr, irec, i, ihbeg, ihend
      integer*4           idxhash
      real*4              facin(MXCRIT)
      logical*4           lfound
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
c   ---- initialize the hash table entries ---
c
      do i=1,MXHASH_PAIR
         idxhsh_src_first_pair(i) = IMISS
      enddo
      do i=1,MXSRC
         idxhsh_src_next_pair(i) = IMISS
      enddo
c
c   --- call routine to find /SOURCE CATEGORY/ packet ---
c       not finding it is not an error, since it is optional ---
c
      irec = 0
      ncdsrc = 0
      keywrd = '/SOURCE CATEGORY/'
c
      call fndkey( jerr,
     &                   IORCTL, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 222
c
c   --- read the FIPS CODE records ---
c
 111  continue
      irec = irec + 1
      read(IORCTL,'(A)',END=222,ERR=7000) ctlstr
      keychk = ctlstr(:KEYLEN)
      call lftjst( keychk )
      call low2up( keychk )
      if( keychk .EQ. KEYEND ) goto 222
c
      read (ctlstr,8000,ERR=7001) infip, insbrg, inplt, instk, 
     &                              inpnt, insic, inscc, ihbeg, ihend,
     &                                           (facin(i),i=1,ncrthd)
      if( ihbeg .LT. 0 ) ihbeg = 0
      if( ihend .LE. 0 ) ihend = 24
c
c  --- create long FIPS code and justify codes for matching ---
c
      call lftjst( infip )
      call lftjst( insbrg )
      call lftjst( inplt )
      call lftjst( instk )
      call lftjst( inpnt )
      call rgtjst( inscc )
      lngfip(1:5) = infip
      lngfip(6:10) = insbrg
c
c  --- get the hash table value for this FIPS/ASC pair ---
c
      idxhash = mkhash_pair( infip, inscc )
c
c  --- if this is the first entry for this hash, just initialize
c      the array elements and set the hash index ---
c
      if( idxhsh_src_first_pair(idxhash) .EQ. IMISS ) then
          ncdsrc = ncdsrc + 1
          if(ncdsrc .GT. MXSRC) go to 7002
          srcfip(ncdsrc) = lngfip
          srcplt(ncdsrc) = inplt
          stksrc(ncdsrc) = instk
          srcpnt(ncdsrc) = inpnt
          srcsic(ncdsrc) = insic
          srcscc(ncdsrc) = inscc
          do i=1,ncrthd
             srcfac(i,ncdsrc) = facin(i)
          enddo
          ihrsrc(ncdsrc,1) = ihbeg
          ihrsrc(ncdsrc,2) = ihend
          idxhsh_src_first_pair(idxhash) = ncdsrc
          goto 111
      endif
c
c  --- there is another entry for this hash, get it ---
c
      idxhash = idxhsh_src_first_pair(idxhash)
c
c  --- hash entry exists, check for exact match ---
c
  333 continue
      lfound = .TRUE.
      if( srcfip(idxhash) .NE. lngfip ) lfound = .FALSE.
      if( srcplt(idxhash) .NE. inplt ) lfound = .FALSE.
      if( stksrc(idxhash) .NE. instk ) lfound = .FALSE.
      if( srcpnt(idxhash) .NE. inpnt ) lfound = .FALSE.
      if( srcsic(idxhash) .NE. insic ) lfound = .FALSE.
      if( srcscc(idxhash) .NE. inscc ) lfound = .FALSE.
      if( ihrsrc(idxhash,1) .NE. ihbeg ) lfound = .FALSE.
      if( ihrsrc(idxhash,2) .NE. ihend ) lfound = .FALSE.
      if( lfound ) then
          write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
          write(ioerr,'(2A)',ERR=9999) 'WARNING:  Reading control ',
     &                                              'factors file.'
          write(ioerr,'(2A)',ERR=9999)
     &         'Duplicate /SOURCE CATEGORY/ control record read.'
          write(ioerr,'(10X,A,/,A)',ERR=9999) 'Line read: ',
     &                                          ctlstr(:strmin(ctlstr))
          write(ioerr,'(2A)',ERR=9999) 'This record will be skipped.'
          write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
          nwarn = nwarn + 1
      endif
c
c  --- if next hash entry is blank, fill in with this data ---
c
      if( idxhsh_src_next_pair(idxhash) .EQ. IMISS ) then 
          ncdsrc = ncdsrc + 1
          if(ncdsrc .GT. MXSRC) go to 7002
          srcfip(ncdsrc) = lngfip
          srcplt(ncdsrc) = inplt
          stksrc(ncdsrc) = instk
          srcpnt(ncdsrc) = inpnt
          srcsic(ncdsrc) = insic
          srcscc(ncdsrc) = inscc
          do i=1,ncrthd
             srcfac(i,ncdsrc) = facin(i)
          enddo
          ihrsrc(ncdsrc,1) = ihbeg
          ihrsrc(ncdsrc,2) = ihend
          idxhsh_src_next_pair(idxhash) = ncdsrc
          goto 111
      endif
c
c  ---- another hash entry exists, set the hash index and look again ---
c
      idxhash = idxhsh_src_next_pair(idxhash)
      goto 333
c
c   --- write count to message file ---
c
 222  continue
      write(IOWMSG,9001,ERR=7003) 'Control factors records read from ',
     &    '/SOURCE CATEGORY/ packet: ',MAX(irec-1,0),' with ',ncdsrc,
     &                                               ' unique controls.'
c
c   ---- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
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
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSRC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSRC ',
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
c   Format statements:
c-----------------------------------------------------------------------
c
 8000 format(2(A5,1X),3(A10,1X),A4,1X,A10,1X,2I5,100(:,F10.0))
 8001 format(2(A5,1X),3(A10,1X),A4,1X,A10,1X,2I5,100(:,F10.6))
 9001 format(1X,2A,2(I6,A),/)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
