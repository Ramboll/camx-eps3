C***** PRCLNK.F 
c
      subroutine prclnk(ierr, idxlnk, idxfip, lhour, 
     &                                          denom, emtot, spctot )
c
c-----------------------------------------------------------------------
c
c   Processes an input EMBR motor vehicle source record.  The emissions
c   are distributed to the cells in the county based on the link
c   definition data.  If the output file is UAM the emissions are stored
c   in the gridded array, otherwise a new EMBR record is written for
c   each grid cell.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
c   Inputs
c     idxlnk   I   index of link type in default link types list
c     idxfip   I   index of county in county list
c     lhour    L   array of logical to tell if current records is for
c                  each of the hours of day
c     denom    R   denominator used to convert emisisons to hourly
c     emtot    R   array of emissions totals for criteria
c     spctot   R   array of emissions totals for speciated
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
      include 'msgtot.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemlnk.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
       integer*4 ierr
       integer*4 idxlnk
       integer*4 idxfip
       logical*4 lhour(MXHOUR)
       real*4    denom
       real*4    emtot(MXCRIT)
       real*4    spctot(MXSPEC)
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   icell  I   cell index in X direction
c   jcell  I   cell index in Y direction
c
      integer*4 icell, jcell, icl, jerr, idxhr, i
      real*4    emtmp(MXCRIT), spctmp(MXSPEC), total
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- loop through the cells in this county ---
c
      if( ibeglk(idxfip) .LE. 0 ) goto 111
      do 10 icl=ibeglk(idxfip),iendlk(idxfip)
         if( dftyp(idxlnk) .NE. clktyp(icl) ) goto 10
c
c   --- get cell index ---
c
         ijcell = icelnk(icl)
         icell = INT( icelnk(icl) / 1000 )
         jcell = MOD( icelnk(icl), 1000 )
c
c   --- adjust the criteria emissions ---
c
         total = 0.0
         do i=1,ncrthd
            emtmp(i) = 0.
            if( emin(i) .GT. 0. ) then
               emout(i) = emin(i) * frclnk(icl)/tlink(idxlnk,idxfip)
               emtmp(i) = emtot(i) * frclnk(icl)/tlink(idxlnk,idxfip)
               if( emout(i) .GT. 0. ) total = total + emout(i)
            endif
         enddo
c
c   --- adjust the speciated emissions ---
c
         do i=1,nspchd
            spctmp(i) = 0.
            if( spcpol(i) .GT. 0. ) then
                spcout(i) = spcpol(i) * frclnk(icl)/tlink(idxlnk,idxfip)
                spctmp(i) = spctot(i) * frclnk(icl)/tlink(idxlnk,idxfip)
                if( (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
                  do idxhr=1,MXHOUR
                    if( lhour(idxhr) ) then
                        emsgrd(idxhr,i,jcell,icell) =
     &                         emsgrd(idxhr,i,jcell,icell) + 
     &                                                 spcout(i)/denom
                    endif
                  enddo
               endif
            endif
         enddo
c
c   --- if not a UAM file, write a new EMBR record ---
c
         if( loutfile .AND. (.NOT. luam .OR. lmeds .OR. lcomp) ) then
              if( lmeds ) then
                  call wrmeds( jerr,
     &                                  IOWEMS, nrec(IDXOUT) )
              elseif( lcomp ) then
                  call wrcomp( jerr,
     &                                  IOWEMS, nrec(IDXOUT) )
              else
                  call wrembr( jerr,
     &                                  IOWEMS, nrec(IDXOUT) )
              endif
              if( jerr .NE. ISUCES ) goto 9999
         endif
c
c  --- add to totals ---
c
         call totgrd(jerr,IDXOUT,emtmp,spctmp,ICDOUT,idxfip,
     &                          scccd,invbeg,invend,.FALSE.,.FALSE.)
         if( jerr .NE. ISUCES ) goto 9999
c
   10 continue
c
c  --- set error flag to sucess ---
c
  111 continue
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
