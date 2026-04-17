C**** TOTRET.F 
c
      subroutine totret( ierr,
     &                        idxtot, emiss, spcems, icdtot, idxfip,
     &                                      sccin, idatbeg, idatend )

c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array at index
c     given by the input argument.  The number of records variable is
c     also incramented for that index.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index in totals array
c       emiss    R   emissions to add to totals for criteria pollutant
c       spcems   R   emissions to add to totals for CBIV species
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
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'embretot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      real*4       emiss(MXCRIT)
      real*4       spcems(MXSPEC)
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
c   ---- set error flag ---
c
      ierr = IFAIL
c
c   ---- add emissions to processing array ---
c
      do i=1,ncrthd
         if( emiss(i) .GT. 0. ) totems(idxtot,i) = totems(idxtot,i)
     &                                                + DBLE(emiss(i))
      enddo
c
c   ---- if there are CBIV species, add totals to array ---
c
      if( nspchd .GT. 0 ) then
         do i=1,nspchd
            if( spcems(i) .GT. 0. ) then
                totspc(idxtot,i) = totspc(idxtot,i) + 
     &                             DBLE( spcems(i) * molwt(i) * CVTTON)
                spctton(i) = spcems(i) * molwt(i) * CVTTON
            else 
                spctton(i) = 0.
            endif
         enddo
      endif
c
c   --- incrament record counter ---
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
        call addtothcd( jerr,sccin,idatbeg,idatend,icdtot,i,0,emiss(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
      do i=1,nspchd
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
        call addtothcd( jerr,sccin,idatbeg,idatend,
     &                                       icdtot,0,i,spctton(i) )
        if( jerr .NE. ISUCES ) goto 9999
      enddo
c
c   --- set error flag to sucess ---
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
