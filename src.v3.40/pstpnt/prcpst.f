C***** PRCPST.F 
c
      subroutine prcpst(ierr)
c
c-----------------------------------------------------------------------
c
c    Main processing routine for the PSTPNT module.  An EMBR record is
c    read and prcocessed.  If it is not an elevated point source record
c    the record is skipped.  The file is processed once for each hour
c    in the modelling episode.   The first time through, all the stack
c    information is stored in the stack arrays which is written to the
c    header part of the output file. The emissions for each stack is
c    then accumulated for each hour and written to the output file.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
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
      include 'uamcntl.inc'
      include 'cntylst.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iopstpnt.inc'
      include 'pstpntstk.inc'
      include 'pstpntot.inc'

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
c   fndhash_fips I   returns index of FIPS code in an hash table (library)
c   fndstk       I   returns the index of the stack data in stack arrays
c
      integer*4 fndhash_fips
      integer*4 fndstk
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   icurdt  I   current date for UAM type file
c   idxfip  I   index of county in county list
c   idxasc  I   index of scc in source list
c   lwarn   L   true if the warning for fips not found has been given
c
      integer*4 irec, i, j, k, jerr, idxstk, ibeghr, iendhr
      integer*4 icurdt(MXHOUR), idxfip
      integer*4 idxhr, nhours, ibegdt, ienddt, idxscc
      real*4    emsgrd(MXSPEC,MXSTK,MXHOUR), emscrt(MXCRIT,MXSTK), denom
      logical*4 lanyhr, lhour(MXHOUR), l6001, l6002, l6003
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      l6001 = .FALSE.
      l6002 = .FALSE.
      l6003 = .FALSE.
      irec = 0
c        
c  --- initialize emsgrd values to zero ---
c
      do j=1,MXCRIT
         do i=1,MXSTK
            emscrt(j,i) = 0.0
         enddo
      enddo
      do j=1,MXSPEC
         do i=1,MXSTK
            do k=1,MXHOUR       
              emsgrd(j,i,k) = 0.0
            enddo
         enddo
      enddo
c
c  --- initialize arrays with dates and hours for this epsiode ---
c
      nhours = 0
  111 continue
      nhours = nhours + 1
      ibeghr = INT( begtim / 100 ) + nhours - 1
      iendhr = INT( begtim / 100 ) + nhours
      ibegdt = ibgdat * 100 + ibeghr
      ienddt = ibgdat * 100 + ibeghr
      if( ibeghr .GT. 24 ) then
          ibeghr = ABS(24 - ibeghr)
          ibegdt = (ibgdat+1) * 100 + ibeghr
      endif
      if( iendhr .GT. 24 ) then
          iendhr = ABS(24 - iendhr)
          ienddt = (ibgdat+1) * 100 + iendhr
      endif
      if( ( INT(ienddt/100.) .LT. iendat) .OR.
     &          (INT( ienddt/100.)  .EQ. iendat .AND.
     &                     iendhr .LE. INT( endtim/ 100 ) ) ) then
        if( nhours .GT. MXHOUR ) goto 7000
        icurdt(nhours) = ibegdt
        goto 111
      endif
      nhours = nhours - 1
c
c  ---  read an EMBR record ---
c
  222 continue
      irec = irec + 1
      call rdembr(jerr,
     &                  IORMBR, irec )
      if( jerr .EQ. IEOD ) goto 333
      if( jerr .EQ. IEOF ) goto 6000
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get index of FIPS code in county list ---
c
      idxfip = fndhash_fips( fipin )
      if( idxfip .LE. 0 ) goto 6001
c
c   --- add input emissions to totals ---
c
      call totpst(jerr,IDXIN,emin,spcpol,ICDIN,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check for correct hours ---
c
      ibeghr = MOD(invbeg,100)
      iendhr = MOD(invend,100)
      if( iendhr .EQ. 0 ) iendhr = 24
      if( iendhr - ibeghr .GT. 24 ) goto 6002
      lanyhr = .FALSE.
      do i=1,nhours
        lhour(i) = .FALSE.
        if( icurdt(i) .GE. invbeg .AND. icurdt(i) .LT. invend) then
           lanyhr = .TRUE.
           lhour(i) = .TRUE.
        endif
      enddo
c
c    --- data on the record for hours beyond the current hours ---
c
      if( .NOT. lanyhr ) goto 6003
c
c   --- if not elevated point source, skip it ----
c
      if( rectyp .NE. 'E' ) then
          call totpst(jerr,IDXLOW,emin,spcpol,0,0,scccd,invbeg,invend)
          if( jerr .NE. ISUCES ) goto 9999
          call totpst(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- call routine to get index of stack in arrays, if not
c       found, add to arrays ---
c
      idxstk = fndstk ( fipin, plant, stkid, stkdia, stkgev,
     &                             stkhgt, stktmp, utme, utmn )
      if( idxstk .EQ. 0 ) then
           call setloc( jerr,
     &                       utme, utmn, 0. )
           if( jerr .NE. ISUCES ) then
                call totpst(jerr,IDXSKP,emin,spcpol,0,0,
     &                                           scccd,invbeg,invend)
                if( jerr .NE. ISUCES ) goto 9999
                goto 222
           endif
           nstack = nstack + 1
           if( nstack .GT. MXSTK ) goto 7001
           ptflag(nstack) = rectyp
           if( ptflag(nstack) .EQ. 'E' ) nstele = nstele + 1
           infips(nstack) = fipin
           inplnt(nstack) = plant
           stckin(nstack) = stkid
           xloc(nstack) = utme
           yloc(nstack) = utmn
           stkht(nstack) = stkhgt
           stdiam(nstack) = stkdia
           stextp(nstack) = stktmp
           stexvl(nstack) = stkgev * 3600.
           idxstk = nstack
           iptscc(idxstk) = nptscc + 1
      endif
c
c  --- look for this SCC code in list of sources feeding stack ---
c
      idxscc = 0
      if( nptscc .GE. iptscc(idxstk) ) then
         do i=iptscc(idxstk),nptscc
            if( scccd .EQ. ptscc(i) ) then
                 idxscc = i
           endif
         enddo
      endif
      if( idxscc .LE. 0 ) then
         nptscc = nptscc + 1
         if( nptscc .GT. MXPTSCC ) goto 7002
         idxscc = nptscc
         ptscc(idxscc) = scccd
         ptsic(idxscc) = siccd
      endif
c
c   --- add to totals by stack/SCC ----
c
      do i=1,ncrthd
        if( emin(i) .GT. 0. )
     &                   emptscc(idxscc,i) = emptscc(idxscc,i) + emin(i)
      enddo
c
c  --- skip if this is not an elevated stack ---
c
      if( rectyp .NE. 'E' ) then
          goto 222
      endif
c
c  --- calculate the hourly emissions ---
c
      denom = FLOAT(iendhr - ibeghr)
      if( denom .GT. 0. ) then
          do i=1,ncrthd
             if( emin(i) .GT. 0. ) then
                 emout(i) = emin(i) / denom
                 emscrt(i,idxstk) = emscrt(i,idxstk) + emin(i)
             endif
          enddo
          do i=1,nspchd
             if( spcpol(i) .GT. 0. ) spcout(i) = spcpol(i) / denom
          enddo
      endif
c
c   --- add input emissions to array and to totals ---
c
      do i=1,nspecs
         if( spcpol(i) .GT. 0. ) then
           do idxhr=1,MXHOUR
              if( lhour(idxhr) ) emsgrd(i,idxstk,idxhr) = 
     &                             emsgrd(i,idxstk,idxhr) + spcout(i)
           enddo
         endif
      enddo
c
c   --- call routine to add to totals array ---
c
      call totpst(jerr,IDXOUT,emin,spcpol,ICDOUT,idxfip,
     &                                            scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- get next record ---
c
      goto 222
c
c  --- finished pass through, write output file ---
c
  333 continue
c
c  ---- write packet containing stack data ---
c
      call wrtpts( jerr )
      if( jerr .NE. ISUCES ) goto 9999
c
c  ---- write this hourly emissions for whole episode ---
c
      call wrtems( jerr,
     &                   icurdt, emscrt, emsgrd, nhours )
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- set error flag and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
      write(ioerr,'(2A)',ERR=9999) 'WARNING:  Input EMBR file does ',
     &                                 'not contain a TRAILER section.'
      write(ioerr,'(2A)',ERR=9999) 'File may be truncated or ',
     &                                                    'incomplete.'
      write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
      nwarn = nwarn + 1
      goto 333
c
 6001 continue
      if( .NOT. l6001 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fipin
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6001 = .TRUE.
      call totpst(jerr,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                                           scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totpst(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
 6002 continue
      if( .NOT. l6002 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &              'records that have not been temporally allocated.'
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Beginning date: ',invbeg
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Ending date   : ',invend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                   ' written to the error file.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6002 = .TRUE.
      call totpst(jerr,IDXEMR,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call wrepst(jerr, nrec(IDXEMR) )
      goto 222
c
 6003 continue
      if( .NOT. l6003 ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                  'records that are not in the episode period.'
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Beginning date: ',invbeg
         write(ioerr,'(10X,A,I8)',ERR=9999) 'Ending date   : ',invend
         write(ioerr,'(5X,2A)',ERR=9999) 'All such records will be',
     &                                                    ' skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6003 = .TRUE.
      call totpst(jerr,IDXSKP,emin,spcpol,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 222
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: EPS cannot process an ',
     &                      'inventory of longer than a 24 hour period.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please update the USERIN ',
     &                                'to reflect a single day epsiode.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'Maximum number of stacks ',
     &                           'in the inventory exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSTK
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSTK ',
     &                                                  'and recompile.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999) 'Maximum number of ',
     &                   'stack/SCC pairs in the inventory exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPTSCC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPTSCC ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
