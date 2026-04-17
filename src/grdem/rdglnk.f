C**** RDGLNK.F 
c
      subroutine rdglnk (ierr)
c
c-----------------------------------------------------------------------
c
c     Sets up link gridding, the cells that are crossed
c     by each link are calculated and stored, as well as the
c     fraction of county totals for the link
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
c
c-----------------------------------------------------------------------
c    LOG:  
c      may 2008  mjimenex
c                changed codes to characte fields
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'cntylst.inc'
      include 'uamregn.inc'
      include 'iogrdem.inc'
      include 'grdemlnk.inc'
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
       integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   fndint   I  returns the index of an integer in an array of integers
c   fndchr   I  returns the index of an string in an array of strings
c   strmin   I  returns actual length of string (min of 1)
c
      integer*4 fndint
      integer*4 fndchr
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fiptmp   C   FIPS code read from file
c   frac     R   fraction of total link distance in cell
c   ctype    C   type of link read from file
c   iznlnk   I   UTM zone of link
c   kcell    I   cells crossed by current link
c   nclnk    I   number of cells crossed by current link
c   tdist    R   total distance for the link
c   xcdbeg   R   easting coordinate beginning node of link
c   xcdend   R   easting coordinate of ending node of link
c   ycdbeg   R   northing coordinate beginning node of link
c   ycdend   R   easting coordinate of ending node of link
c
      character*(MXSTR) string
      character*5       fiptmp
      character*5       ctype
      integer*4         iznlnk, irec, icl(6), jcl(6), i, j
      integer*4         icnt, idxfip, idxtyp, nclnk, kcell(MXLKCL)
      integer*4         idum, jerr
      real*4            xcdbeg, ycdbeg, xcdend, ycdend, frac(MXLKCL)
      real*4            tdist, alat, alon
      logical*4         l6000, l6001
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      dftyp(IDXHW) = TYPHW
      dftyp(IDXRR) = TYPRR
      dftyp(IDXAP) = TYPAP
      dftyp(IDXPT) = TYPPT
      dftyp(IDXDS) = TYPDS
      nlink = 1
      l6000 = .FALSE.
      l6001 = .FALSE.
      irec = 0
c
c   --- read next record ---
c
  111 continue
      irec = irec + 1
      read(IORLNK,'(A)',ERR=7000,END=222) string
      read(string,8000,ERR=7001) fiptmp, ctype, xcdbeg, ycdbeg,
     &                                            xcdend, ycdend, iznlnk
      call lftjst( ctype )
      call low2up( ctype )
      xcdbeg = xcdbeg / 1000.
      ycdbeg = ycdbeg / 1000.
      xcdend = xcdend / 1000.
      ycdend = ycdend / 1000.
c
c   --- skip record if county is not in county list ---
c
      idxfip = fndchr ( fiptmp, 5, fipcd, ncdfip )
      if( idxfip .LE. 0 ) goto 6000
c
c   --- check for invalid link type ---
c
      idxtyp = fndchr ( ctype, 3, dftyp, NLTYPE )
      if( idxtyp .LE. 0 ) goto 6001
c
c   --- if not the same UTM zone, convert to modeling zone ---
c
      if( izone .NE. iznlnk ) then
         call maputg( xcdbeg, ycdbeg, iznlnk, alon, alat )
         call mapgtu( alon, alat, izone, idum, xcdbeg, ycdbeg )
         call maputg( xcdend, ycdend, iznlnk, alon, alat )
         call mapgtu( alon, alat, izone, idum, xcdend, ycdend )
      endif
c
c  --- call routine to get cells crossed by link ---
c
      call gline(jerr,nclnk,kcell,frac,xcdbeg,ycdbeg,xcdend,ycdend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   ---- store total link type by county ----
c
      tdist = sqrt( (xcdend-xcdbeg)*(xcdend-xcdbeg) +
     &                               (ycdend-ycdbeg)*(ycdend-ycdbeg) )
      tlink(idxtyp,idxfip) = tlink(idxtyp,idxfip) + tdist
c
c   ---- if link not in region, skip the storage ---
c
      if( nclnk .LE. 0 ) then
          write(IOWMSG,9000,ERR=7001) 'Link #',irec,' Type: ',ctype,
     &            ' crossed ',nclnk,' cells in county: ',fipcd(idxfip)
          goto 111
      endif
c
c   ---- if this is the first link for this county, initialize ----
c
      if( nlncty(idxfip) .LE. 0 ) then
         ibeglk(idxfip) = nlink
         iendlk(idxfip) = nlink
      endif
      nlncty(idxfip) = nlncty(idxfip) + 1
c
c   ---- store cell and link distances in arrays ---
c
      if( nlink + nclnk .GT. MXLKCL ) goto 7002
      do i=1,nclnk
         icelnk(nlink+i-1) = kcell(i)
         clktyp(nlink+i-1) = ctype
         frclnk(nlink+i-1) = frac(i)*tdist
      enddo
c
c   --- echo data to message file ----
c
      write(IOWMSG,9000,ERR=7003) 'Link #',irec,' Type: ',ctype,
     &            ' crossed ',nclnk,' cells in county: ',fipcd(idxfip)
      do i=1,nclnk,6
          icnt = 0
          do j=i,i+5
             if( kcell(j) .GT. 0 ) then
                 icl(j-i+1) = kcell(j)/1000
                 jcl(j-i+1) = MOD(kcell(j),1000)
                 icnt = icnt + 1
             endif
          enddo
          write(IOWMSG,9001,ERR=7003) (icl(j),jcl(j),j=1,icnt)
      enddo
      nlink = nlink + nclnk
      iendlk(idxfip) = nlink - 1
c
c   --- read next record ---
c
      goto 111
c
c   --- entire file processed, echo data ---
c
  222 continue
      nlink = nlink - 1
      write(IOWMSG,'(A)',ERR=7003)
      write(IOWMSG,'(T10,A,I8)',ERR=7003) 
     &                     'Number of link records processed: ',irec-1
      write(IOWMSG,'(T10,A,I8)',ERR=7003)
     &              'Number of grid cells crossed by any link: ',nlink
c
c  --- set error flag to suceess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning Messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. l6000 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(A)',ERR=9999) 'WARNING:  Reading link ',
     &                                              'definition file.'
         write(ioerr,'(2A)',ERR=9999) 'Link definition file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fiptmp
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      goto 111
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(A)',ERR=9999) 'WARNING:  Reading link ',
     &                                              'definition file.'
         write(ioerr,'(2A)',ERR=9999) 'Link definition file contains ',
     &                               'records with invalid link type.'
         write(ioerr,'(10X,A,A5)',ERR=9999) 'Invalid link type: ',ctype
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'link type will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &         'ERROR:  Reading link definition file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',lnkfl(:strmin(lnkfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A,I8)',ERR=9999)
     &         'ERROR:  Reading link definition file at line: ',irec
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',lnkfl(:strmin(lnkfl))
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7002 continue
      write(ioerr,'(/,A)',ERR=9999)
     &         'ERROR:  Reading link definition file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of cells',
     &                                  'crossed by a link exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXLKCL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXLKCL ',
     &                                                'and recompile.'
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
 8000 format(A5,A5,4F10.0,I10)
 9000 format(T10,A,I4,A,A3,A,I4,2A)
 9001 format(T10,6(:,'(',I3,',',I3,')',1X) )
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
