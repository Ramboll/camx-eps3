C**** PRCPT.F 
c
      subroutine prcpt(ierr, idxfip, lhour, denom, lwarn, emtot, spctot)
c
c-----------------------------------------------------------------------
c
c   Processes an input EMBR low level point source record.  The location
c   of the source is calculated and the emissions are either stored in
c   the gridded array or written to the EMBR file.
c
c   Argument Declaration:
c   Outputs:
c     ierr     I   error flag
c   Inputs
c     idxfip   I   index of county in county list
c     lhour    L   array of logical to tell if current records is for
c                  each of the hours of the day
c     denom    R   denominator used to convert emissions to hourly
c     lwarn    L   flag to determine if warning has been written
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
      include 'uamregn.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
       integer*4 ierr
       integer*4 idxfip
       logical*4 lhour(MXHOUR)
       real*4    denom
       logical*4 lwarn
       real*4    emtot(MXCRIT)
       real*4    spctot(MXSPEC)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c   icell  I   cell index in X direction
c   jcell  I   cell index in Y direction
c
      integer*4 icell, jcell, jerr, idxhr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- calculate the cell location ---
c
      call setij(icell, jcell,
     &                         utme, utmn, 0. )
      if( icell .LE. 0 .OR. icell .GT. nxcell .OR. jcell .LE. 0 .OR.
     &                                   jcell .GT. nycell ) goto 6000
      ijcell = icell * 1000 + jcell
c
c   --- fill output common arrays ---
c
      do i=1,ncrthd
         emout(i) = emin(i)
      enddo
      do i=1,nspchd
         spcout(i) = spcpol(i)
         if( spcout(i) .GT. 0. .AND. 
     &             (lnetcdf_out .OR. luam) .AND. .NOT. lmeds ) then
             do idxhr=1,MXHOUR
                 if( lhour(idxhr) ) then
                      emsgrd(idxhr,i,jcell,icell) =
     &                     emsgrd(idxhr,i,jcell,icell) + spcout(i)/denom
                 endif
             enddo
          endif
      enddo
c
c   --- if not a UAM file, write a new EMBR record ---
c
      if( loutfile .AND. (.NOT. luam .OR. lmeds .OR. lcomp) ) then
          if( lmeds ) then
               call wrmeds( jerr,
     &                            IOWEMS, nrec(IDXOUT) )
          elseif( lcomp ) then
               call wrcomp( jerr,
     &                            IOWEMS, nrec(IDXOUT) )
          else
               call wrembr( jerr,
     &                            IOWEMS, nrec(IDXOUT) )
          endif
          if( jerr .NE. ISUCES ) goto 9999
      endif
c
c  --- add to totals ---
c
      call totgrd(jerr,IDXOUT,emtot,spctot,ICDOUT,idxfip,
     &                          scccd,invbeg,invend,.FALSE.,.FALSE.)
      if( jerr .NE. ISUCES ) goto 9999
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
 6000 continue
      if( .NOT. lwarn ) then
         write(ioerr,'(//,40A,/)',ERR=9999) ('/\\',i=1,40)
         write(ioerr,'(2A)',ERR=9999) 'WARNING:  EMBR file contains ',
     &                     'point sources that are not in the domain.'
         write(ioerr,'(10X,A,F12.4)',ERR=9999) 'X-coordinate: ',utme
         write(ioerr,'(10X,A,F12.4)',ERR=9999) 'Y-coordinate: ',utmn
         write(ioerr,'(A)',ERR=9999) 
     &                          'All such records will be skipped.'
         write(ioerr,'(/,40A,//)',ERR=9999) ('/\\',i=1,40)
         nwarn = nwarn + 1
      endif
      lwarn = .TRUE.
      call totgrd(jerr,IDXSKP,emtot,spctot,0,0,scccd,invbeg,invend)
      if( jerr .NE. ISUCES ) goto 9999
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
