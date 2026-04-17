C**** COLOCATE.F version 2.0 09/01/2007
c
      subroutine colocate(ierr)
c
c-----------------------------------------------------------------------
c
c    This routine walks though the list of stacks and finds the 
c    sources that a co-located and adds up all of the emissions for
c    co-located sources to see if the combined total exceeds the 
c    threshold.  The threshold code that is used for comparison 
c    purposes is specified by the user in the USERIN file. A source
c    will only qualify for colocation checking if it is 25% of the
c    colocation threshold value.  Any sources that are below 2.5%
c    will be elimited from colocation consideration.
c
c    Argument Declaration.
c     Outputs:
c        ierr  I  error code
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
      include 'pigemstot.inc'
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
c   fndchr   I   finds a string in an array of strings
c   fndint   I   finds an integer in an array of integers
c   fndmap   I   finds the best match of county in /THRESHOLD MAP/ packet
c
      integer*4 fndchr
      integer*4 fndint
      integer*4 fndmap
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   COLOIN   R  fraction of the threshold that is needed for consideration  
c   COLOOUT  R  fraction of the threshold that is needed for non-consideration  
c
      real*4 COLOCIN        
      real*4 COLOCOUT
c
      parameter( COLOCIN  = 0.25  )
      parameter( COLOCOUT = 0.025 )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxpt, idxrest, idxmap, idxthrsh, icrt, i
      real*4    distance, threshmax, threshmin, emssum
      logical   linclude(MXSTKLST), lfound
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- set the error flag ---
c
      ierr = IFAIL
c
c  --- loop over all stacks ---
c
      do 10 idxpt=1,nstklist
c
c  --- skip this source if it has plume rise distribution override ---
c
        if( lprdo(idxpt) ) goto 10
c
c  --- find the "best" trheshold map for this source ---
c
        idxmap = fndmap( ctylist(idxpt), idxcoloc )
        if( idxmap .LE. 0 ) goto 10    
c
c  --- find the threshold code for this mapping ---
c
        idxthrsh = 0
        do i=1,nthresh
          if( idxthspc(i) .EQ. idxcoloc .AND. 
     &                 ithrshmap(idxmap) .EQ. ithrshcd(i) ) idxthrsh = i
        enddo
        if( idxthrsh .LE. 0 ) goto 10
c
c  --- if this has already been flagged as collocated, skip it ---
c
        lfound = .FALSE.
        if( lcolocat(idxpt) ) goto 10
c
c  --- set the min and max values for consideration ---
c
        threshmax = COLOCIN * thrshval(idxthrsh)
        threshmin = COLOCOUT * thrshval(idxthrsh)
c
c  --- if the emissions for this source is not large enough, skip it ---
c
        if( emsmax(idxpt,idxcoloc) .LT. threshmax ) goto 10
        emssum = emsmax(idxpt,idxcoloc)
c
c  --- loop over all of the stacks after this one ---
c
        do 20 idxrest=idxpt+1,nstklist
           linclude(idxrest) = .FALSE.
c
c  --- skip this source if it has plume rise distribution override ---
c
           if( lprdo(idxrest) ) goto 20
c
c  --- if this stack is not large enough, skip it ---
c
           if( emsmax(idxrest,idxcoloc) .LE. threshmin ) goto 20
c
c  --- if this has already been flagged as collocated, skip it ---
c
           if( lcolocat(idxpt) ) goto 20
c
c  --- check if the distance is within the range ---
c
           distance = sqrt( (xloclist(idxpt)-xloclist(idxrest))**2
     &                    + (yloclist(idxpt)-yloclist(idxrest))**2 )
           if( distance .LE. colodist ) then
              linclude(idxrest) = .TRUE.
              linclude(idxpt) = .TRUE.
              lfound = .TRUE.
              emssum = emssum + emsmax(idxrest,idxcoloc)
            endif
c
c  --- get the next stack ---
c
   20   continue
c
c  --- if the sum of emissions is above the threshold, create
c      a new stack and combine all of the collocated stacks ---
c
        if( emssum .GT. thrshval(idxthrsh) .AND. lfound ) then
            nstknew = nstknew + 1
            if( nstknew  .GT. MXSNEW ) goto 7000
            ctynew(nstknew) = ctylist(idxpt)
            facnew(nstknew) = faclist(idxpt)
            write(stidnew(nstknew),'(A,I7.7)') 'NEW',nstknew
            hgtnew(nstknew) = RMISS
            dianew(nstknew) = 0.
            tptnew(nstknew) = RMISS
            velnew(nstknew) = RMISS
            xlocnew(nstknew) = RMISS
            ylocnew(nstknew) = RMISS
            lpignew(nstknew) = .TRUE.
            lcolonew(nstknew) = .TRUE.
c
c  --- add all of the colocated stacks to the combined arrays ---
c
            do i=idxpt,nstklist
               if( linclude(i) ) then
                   nstkcomb(nstknew) = nstkcomb(nstknew) + 1
                   if( nstkcomb(nstknew) .GT. MXCOMB ) goto 7001
                   ctycomb(nstknew,nstkcomb(nstknew)) = ctylist(i)
                   faccomb(nstknew,nstkcomb(nstknew)) = faclist(i)
                   stidcomb(nstknew,nstkcomb(nstknew)) = stklist(i)
                   hgtcomb(nstknew,nstkcomb(nstknew)) = hgtlist(i)
                   diacomb(nstknew,nstkcomb(nstknew)) = dialist(i)
                   tptcomb(nstknew,nstkcomb(nstknew)) = tptlist(i)
                   velcomb(nstknew,nstkcomb(nstknew)) = vellist(i)
                   xloccomb(nstknew,nstkcomb(nstknew)) = xloclist(i)
                   yloccomb(nstknew,nstkcomb(nstknew)) = yloclist(i)
                   ifilenew(nstknew,nstkcomb(nstknew)) = ifilestk(i)
                   do icrt=1,ncrthd
                     crtcomb(nstknew,nstkcomb(nstknew),icrt) = 
     &                                              emsmax(i,icrt)
                   enddo
                   lcolocat(i) = .TRUE.
                   ntotstk(ifilestk(i),ICDCOL) = 
     &                           ntotstk(ifilestk(i),ICDCOL) + 1
               endif
            enddo
         endif
c
c  --- get the next stack ---
c
   10 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: In colocation routine.'
      write(ioerr,'(3A)',ERR=9999) 'Number of new stacks that need ',
     &                                      'to be created exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSNEW
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSNEW ',
     &            'and recompile, or decrease the colocation distance.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: In colocation routine.'
      write(ioerr,'(A)',ERR=9999)
     &                   'Number of new stacks to combine exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXCOMB
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXCOMB ',
     &            'and recompile, or decrease the colocation distance.'
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
