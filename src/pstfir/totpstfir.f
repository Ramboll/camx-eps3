C**** TOTPSTFIR.F 
c
      subroutine totpstfir( ierr, 
     &                        idxtot, emiss, spcin, icdtot, idxfip, 
     &                                      sccin, idatbeg, idatend )

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
c       idxtot   I   index of the record type in the totals array
c       emiss    R   array of emissions to add to totals for criteria
c       spcin    R   array of emissions to add to totals for speciated
c       icdtot   I   code for global totals arrays
c       idxfip   I   index into county arrays
c       sccin    C   ASC/SCC code
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'pstfirtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      real*4       emiss(MXCRIT)
      real*4       spcin(MXSPEC)
      integer*4    icdtot
      integer*4    idxfip
      character*10 sccin
      integer*4    idatbeg
      integer*4    idatend
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr, i
      real*4    spctton(MXSPEC)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize the error code ---
c
      ierr = IFAIL
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
      do i=1,nspecs
         if( spcin(i) .GT. 0. ) then
            totspc(idxtot,i) = totspc(idxtot,i) +
     &                               DBLE(spcin(i) * molwt(i) * CVTTON)
            spctton(i) = spcin(i) * molwt(i) * CVTTON
         else
            spctton(i) = 0.
         endif
      enddo
c
c   ---- accumulate number of records processed counter ---
c
      nrec(idxtot) = nrec(idxtot) + 1
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
        call addtotasc( jerr,icdtot,sccin,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,icdtot,idxfip,sccin,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,idatbeg,idatend,icdtot,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotpid( jerr,icdtot,idxfip,plant,stkid,
     &                                        point,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,sccin,idatbeg,idatend,icdtot,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
      do i=1,nspecs
        call addtotcty( jerr,icdtot,idxfip,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotasc( jerr,icdtot,sccin,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotfcd( jerr,icdtot,idxfip,sccin,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothrs( jerr,idatbeg,idatend,icdtot,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtotpid( jerr,icdtot,idxfip,plant,stkid,
     &                                        point,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
c
        call addtothcd( jerr,sccin,idatbeg,idatend,
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
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
