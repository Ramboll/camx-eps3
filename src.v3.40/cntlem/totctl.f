C**** TOTCTL.F 
c
      subroutine totctl( ierr, idxasc,
     &                        idxtot, emiss, spcin, icdtot, idxfip, 
     &                                 inscc, insic, idatbeg, idatend )

c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to a the totals array at index
c     given by the input argument.  The number of records variable is
c     also incramented for that index.
c
c   Arguments:
c
c     Inputs:
c       ierr     I   error code
c       idxasc   I   index of code in global totals arrays
c       idxtot   I   index of the record type in the totals array
c       emiss    R   array of emissions to add to totals for criteria
c       spcin    R   array of emissions to add to totals for CBIV
c       icdtot   I   code for global totals arrays
c       idxfip   I   index into county arrays
c       inscc    C   ASC/SCC code
c       idatbeg  I   beginning date
c       idatend  I   ending date
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'ctltot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxasc
      integer*4    idxtot
      real*4       emiss(MXCRIT)
      real*4       spcin(MXSPEC)
      integer*4    icdtot
      integer*4    idxfip
      character*10 inscc
      character*4  insic
      integer*4    idatbeg
      integer*4    idatend
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*14 sicscc
      integer*4    jerr, i
      real*4       spctton(MXSPEC)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize the error code ---
c
      ierr = IFAIL
c
c   ---- find the index of this SCC/SIC code in the totals array ---
c
      sicscc(1:10) = inscc
      sicscc(11:14) = insic
      idxasc = fndchr( sicscc, 14, sccsic, nscsic )
      if( idxasc .LE. 0 ) then
          nscsic = nscsic + 1
          if( nscsic .GT. MXSCSC ) goto 7000
          idxasc = nscsic
          sccsic(nscsic) = sicscc
          ctlflg(nscsic) = '    '
          lcodctl(nscsic) = .TRUE.
      endif
c
c   ---- add emissions to criteria totals ---
c
      do i=1,ncrthd
         if( emiss(i) .GT. 0. ) totems(idxtot,i) = totems(idxtot,i) +
     &                                                   DBLE(emiss(i))
      enddo
c
c   ---- add any species emissions to totals ---
c
      do i=1,nspchd
         if( spcin(i) .GT. 0. ) then
            totspc(idxtot,i) = totspc(idxtot,i) +
     &                              DBLE(spcin(i) * molwt(i) * CVTTON)
            spctton(i) = spcin(i) * molwt(i) * CVTTON
         else
            spctton(i) = 0.
         endif
      enddo
c
c   ---- accumulate number of records processed counter ---
c
      if( idxtot .NE. IDXOUT) nrec(idxtot) = nrec(idxtot) + 1
c
c   ---- add totals to input arrays ----
c
      if( idxtot .EQ. IDXIN ) then
        do 10 i=1,ncrthd
            if( emiss(i) .LE. 0. ) goto 10
            totifp(idxasc,0,i) = totifp(idxasc,0,i) + emiss(i)
            totifp(0,0,i) = totifp(0,0,i) + emiss(i)
c
c   ---- if we have a FIPS code, add emissions to totals for county ----
c
            if( idxfip .GT. 0 ) then
               totifp(0,idxfip,i) = totifp(0,idxfip,i) + emiss(i)
               totifp(idxasc,idxfip,i) =
     &                                totifp(idxasc,idxfip,i) + emiss(i)
            endif
   10   continue
c
c   ---- add totals to output arrays ----
c
      else if( idxtot .EQ. IDXOUT ) then
         do 20 i=1,ncrthd
            if( emiss(i) .LE. 0. ) goto 20
            totofp(idxasc,0,i) = totofp(idxasc,0,i) + emiss(i)
            totofp(0,0,i) = totofp(0,0,i) + emiss(i)
c
c   ---- if we have a FIPS code, add emissions to totals for county ----
c
            if( idxfip .GT. 0 ) then
               totofp(0,idxfip,i) = totofp(0,idxfip,i) + emiss(i)
               totofp(idxasc,idxfip,i) =
     &                                totofp(idxasc,idxfip,i) + emiss(i)
            endif
   20    continue
      endif
c
c   ---- if we do need to add to global totals, just return ---
c
      if( icdtot .LE. 0 ) then
        ierr = ISUCES
        goto 9999
      endif
c
c   ---- call routines to put totals into global tables arrays ---
c
      do i=1,ncrthd
        call addtotcty( jerr,icdtot,idxfip,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,icdtot,inscc,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,icdtot,idxfip,inscc,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,idatbeg,idatend,icdtot,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotpid( jerr,icdtot,idxfip,plant,stkid,
     &                                        point,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,inscc,idatbeg,idatend,icdtot,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
      do i=1,nspchd
        call addtotcty( jerr,icdtot,idxfip,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,icdtot,inscc,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,icdtot,idxfip,inscc,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,idatbeg,idatend,icdtot,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotpid( jerr,icdtot,idxfip,plant,stkid,
     &                                        point,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,inscc,idatbeg,idatend,
     &                                       icdtot,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
c
c  --- set the error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Adding emissions ',
     &                                  'to totals for output tables.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &                                   'SIC/SCC pairs exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSCSC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSCSC ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
