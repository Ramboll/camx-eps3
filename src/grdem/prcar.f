C*** PRCAR.F 
c
      subroutine prcar(ierr,
     &             idxsrg, idxfip, idxlng, lhour, denom,
     &                            emtot, spctot, lusesec, lusetert )
c
c-----------------------------------------------------------------------
c
c   Processes an input EMBR area source record.  The emissions is
c   distributed to the cells in the county based on the gridded
c   surrogate data.  If the output file is UAM the emissions are stored
c   in the gridded array, otherwise a new EMBR record is written for
c   each grid cell.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
c   Inputs
c     idxsrg   I   index of surrogate in gridded surrogates array
c     idxfip   I   index of county in county list
c     idxlng   I   index of long-FIPS code in surrogate arrays
c     lhour    L   array of logical to tell if current records is for
c                  each of the three hours
c     denom    R   denominator to divide emissions to get hourly
c     emtot    R   array of emissions totals for criteria
c     spctot   R   array of emissions totals for speciated
c     lusesec  L   true if this source is mapped to secondary surrogate
c     lusetert L   true if this source is mapped to tertiary surrogate
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
      include 'uamregn.inc'
      include 'hash.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemsrg.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
       integer*4 ierr
       integer*4 idxsrg
       integer*4 idxfip
       integer*4 idxlng
       logical*4 lhour(MXHOUR)
       real*4    denom
       real*4    emtot(MXCRIT)
       real*4    spctot(MXSPEC)
       logical*4 lusesec
       logical*4 lusetert
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   srglbl  C   label for surrogate output table
c   denslbl C   label for surrogate output table (without spaces)
c   icell   I   cell index in X direction
c   jcell   I   cell index in Y direction
c
      character*1  srgflg
      character*22 srglbl
      character*19 denslbl
      integer*4    icell, jcell, icl, jerr, idxhr, i
      real*4       emtmp(MXCRIT), spctmp(MXSPEC), total
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- add to surrogate totals for optional output report ---
c
      if ( lsrpt )  then
         srgflg = "P"
         if ( lusesec ) srgflg = "S"
         if ( lusetert ) srgflg = "T"
         srglbl = fipin //' '// scccd //' '// codnm(idxsrg) 
     &                                                 //' '// srgflg
         denslbl = fipin//scccd//codnm(idxsrg)//srgflg
         call totsrg( jerr,
     &                1, srglbl, denslbl, emtot )
         if( jerr .NE. ISUCES ) goto 9999
      endif
c
c   --- loop through the cells in this county ---
c
      do 10 icl=ibegsg(idxlng),iendsg(idxlng)
         ijcell = iclsrg(icl)
         icell = INT( iclsrg(icl) / 1000 )
         jcell = MOD( iclsrg(icl), 1000 )
c
c   --- check that cell is inside the domain ---
c
         if( icell .LE. 0 .OR. icell .GT. nxcell .OR.
     &             jcell .LE. 0 .OR. jcell .GT. nycell ) goto 10
c
c   --- adjust the criteria emissions ---
c
         total = 0.0
         do i=1,ncrthd
            emtmp(i) = 0.
            if( emin(i) .GT. 0.) then
               emout(i) = emin(i) * srgfrc(idxsrg,icl)
               emtmp(i) = emtot(i) * srgfrc(idxsrg,icl)
               if( emout(i) .GT. 0. ) total = total + emout(i)
            endif
         enddo
c
c   --- adjust the speciated emissions ---
c
         do i=1,nspchd
            spctmp(i) = 0.
            if( spcpol(i) .GT. 0. ) then
               spcout(i) = spcpol(i) * srgfrc(idxsrg,icl)
               spctmp(i) = spctot(i) * srgfrc(idxsrg,icl)
               if( (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
                   do idxhr=1,MXHOUR
                      if( lhour(idxhr) ) then
                         emsgrd(idxhr,i,jcell,icell) =
     &                       emsgrd(idxhr,i,jcell,icell) + 
     &                                          spcout(i)/denom
                      endif
                   enddo
               endif
            endif
         enddo
c
c   --- if not a UAM file, write a new EMBR record ---
c
         if( loutfile .AND. (.NOT. luam .OR. lmeds .OR. lcomp) ) then
             if( total .GT. 0.) then
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
         endif
c
c  --- add to totals ---
c
         call totgrd(jerr,IDXOUT,emtmp,spctmp,ICDOUT,idxfip,
     &                          scccd,invbeg,invend,lusesec,lusetert)
         if( jerr .NE. ISUCES ) goto 9999
         if ( lsrpt )  then
            call totsrg( jerr,
     &                    2, srglbl, denslbl, emtmp )
            if( jerr .NE. ISUCES ) goto 9999
         endif
c
   10 continue
c
c  --- set error flag to sucess ---
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
