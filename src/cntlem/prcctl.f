C**** PRCCTL.F 
c
      subroutine prcctl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header and stores the variables in common varibales
c    then it creates the program name/date/time string and writes
c    the rest of the header
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'cntylst.inc'
      include 'sccxref.inc'
      include 'msgtot.inc'
      include 'hash.inc'
      include 'iocntlem.inc'
      include 'ctlreg.inc'
      include 'ctldsc.inc'
      include 'ctltot.inc'
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
c   fndchr   I   returns the index of a string in an array of strings
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr, idxfip, idxasc, irec, itregs
      integer*4 itdsc, ihr, i, j
      integer*4 ibegdt, ibghr, inbsav, inesav
      logical*4 lchange, l6000
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      l6000 = .FALSE.
      irec = 0
c
c   --- set total control values ---
c
      itregs = ncdppt + ncdpam
      itdsc = ncdsrc + ncdcty
c
c   ---  loop back to here for reading EMBR /DATA/ records ---
c
  111 continue
      call rdembr(jerr,
     &                 IORMBR, irec )
      if( jerr .EQ. IEOD ) goto 222
      if( jerr .NE. ISUCES ) goto 9999
      ibegdt = invbeg / 100
      inbsav = invbeg
      inesav = invend
      irec = irec + 1
c
c  --- intitialize the output arrays ---
c
      do i=1,ncrthd
         emout(i) = 0.
         do j=1,MXHOUR
           emshr(i,j) = 0.
         enddo
      enddo
      do i=1,nspchd
         spcout(i) =  0.
         do j=1,MXHOUR
           spchr(i,j) = 0.
         enddo
      enddo
c
c   --- get index of county in county list ---
c
      idxfip = fndchr( fipin, 5, fipcd, ncdfip )
      if( idxfip .LE. 0 ) goto 6000
c
c   --- get the hours of the inventory ---
c
      ibegin = MOD(invbeg,100) + 1
      iendin = MOD(invend,100)
      if( iendin .EQ. 0 ) iendin = 24
      nhours = iendin - ibegin + 1
      if( nhours .LE. 0 ) goto 111
      do ihr = ibegin,iendin
          do i=1,ncrthd
             emshr(i,ihr) = emin(i)/nhours
          enddo
          do i=1,nspchd
             if( spcpol(i) .GT. 0 ) then
                spchr(i,ihr) = spcpol(i)/nhours
             else
                spchr(i,ihr) = RMISS
             endif
          enddo
      enddo
c
c   --- add to input totals arrays ---
c
      call totctl(jerr,idxasc,IDXIN,emin,spcpol,ICDIN,
     &                              idxfip,scccd,siccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- check if there are any regulatory controls ---
c
      if (itregs .GT. 0) then
c
c   --- apply projection factors ---
c
         jerr = ISUCES
         if( ncdppt .GT. 0 .OR. ncdpam .GT. 0 ) then
             call projec( idxasc )
         endif
c
c   --- end of regulatory applications
c
      endif
c
c   --- check for descrenary control factors
c
      if( itdsc .GT. 0) then
c
c   --- call routine to process "discretionary controls" ---
c
          call ctldsc( idxasc )
c
c   --- end of discretionary controls ---
c
      endif
c
c  --- initialize arrays to first hour ---
c
      do i=1,ncrthd
         if( emshr(i,ibegin) .GT. 0. ) emout(i) = emshr(i,ibegin)
      enddo
      do i=1,nspchd
         if( spchr(i,ibegin) .GT. 0. ) spcout(i) =  spchr(i,ibegin)
      enddo
c
c  --- loop over the hours and accumulate totals ---
c
      ibghr = ibegin-1
      ihr = ibegin+1
 333  continue
      lchange = .FALSE.
      do i=1,ncrthd
       if( emshr(i,ihr) .NE. emshr(i,ihr-1) ) lchange = .TRUE.
      enddo
      if( lchange ) then
c
c   --- set dates and call routine to write EMBR record ---
c
         invbeg = ibegdt * 100 + ibghr
         ibghr = ihr-1
         invend = ibegdt * 100 + ihr-1
         call wrembr(jerr,
     &                 IOWMBR, nrec(IDXOUT) )
         if( jerr .NE. ISUCES ) goto 9999
c
c   --- add to output totals arrays ---
c
         call totctl(jerr,idxasc,IDXOUT,emout,spcout,ICDOUT,
     &                             idxfip,scccd,siccd,invbeg,invend)
         if( jerr .NE. ISUCES ) goto 9999
         do i=1,ncrthd
            emout(i) = 0.
         enddo
         do i=1,nspchd
            spcout(i) =  0.
         enddo
      endif
      do i=1,ncrthd
        if( emshr(i,ihr) .GT. 0. ) emout(i) =  emout(i) + emshr(i,ihr)
      enddo
      do i=1,nspchd
        if( spchr(i,ihr) .GT. 0. ) spcout(i) =  spcout(i) + spchr(i,ihr)
      enddo
      ihr = ihr + 1
      if( ihr .LE. iendin ) goto 333
c
c  --- write the last span, if necessary ---
c
      if( ibghr .GE. iendin ) goto 111
      if( ibghr .EQ. ibegin-1 ) then
          invbeg = inbsav 
          invend = inesav 
      else 
         invbeg = ibegdt * 100 + ibghr
         ibghr = ihr
         invend = ibegdt * 100 + ihr-1
      endif
      call wrembr(jerr,
     &                 IOWMBR, nrec(IDXOUT) )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- add to output totals arrays ---
c
      call totctl(jerr,idxasc,IDXOUT,emout,spcout,ICDOUT,
     &                            idxfip,scccd,siccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- get next record ---
c
      goto 111
c
c   --- set error flag to sucess ---
c
  222 continue
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
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                           'counties that are not in the USERIN.'
         write(ioerr,'(10X,2A)',ERR=9999) 'Unlisted FIPS: ',fipin
         write(ioerr,'(2A)',ERR=9999) 'All records with invalid ',
     &                                    'counties will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      l6000 = .TRUE.
      call totctl(jerr,idxasc,IDXIN,emin,spcpol,ICDIN,MXFIP+1,
     &                                      scccd,siccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      call totctl(jerr,idxasc,IDXSKP,emin,spcpol,0,
     &                               idxfip,scccd,siccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
      goto 111
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
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
