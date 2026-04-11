C**** STKAVG.F version 2.0 09/01/2007
c
      subroutine stkavg( )
c
c-----------------------------------------------------------------------
c
c    This routine caclulates the "average" stack parameters for the
c    stacks that are to be combined to make a new stack.
c    The new stack parameters are caclulated based on these rules:
c         flowrate - sum of flowrate
c         diameters - sum diameters
c         stack height - max stack height
c         temperature - flowrate weighted average of exit temperatures
c         velocity - calculate velocity from flowrate and area
c
c    Argument Declaration.
c     Outputs:
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      01/23/06	-gwilson-	Fixed bug that was not updating stack
c                               parameters.
c      17nov2015 mjimenez       Corrected the flowrate; equals sum, not
c                               average.
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
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
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   PI   R  pretty good aprox for Pi
c
      real*4 PI
c
      parameter( PI = 3.1415927 )     
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
       integer*4 inew, nsumflow, nsumloc, ispc, ihr, i
       real*4    sumflow, sumdiam, hgtmax, sumtpt, sumxloc, sumyloc
       real*4    flowavg
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- if there are no new stacks, just exit ---
c
      if( nstknew .LE. 0 ) goto 9999
c
c  --- loop over all of the new stacks ---
c
      do inew=1,nstknew
c
c  --- set the sums to zero ---
c     
         sumflow  = 0.
         nsumflow = 0
         flowavg = 0.
         sumdiam = 0.
         hgtmax = -99999. 
         sumtpt = 0.
         sumxloc = 0.
         sumyloc = 0.
         nsumloc = 0
c
c  --- loop over the stacks that are tcombine to make 
c      this new stack ---
c
         do i=1,nstkcomb(inew)
c
c  --- calculate the flowrate ---
c
            if( ABS(diacomb(inew,i)) .GT. 0. ) then
               if( velcomb(inew,i) .GT. 0.) then 
                   sumflow = sumflow + PI * 
     &                 (ABS(diacomb(inew,i))/2.0)**2. * velcomb(inew,i)
                   nsumflow = nsumflow + 1
                   sumtpt = sumtpt + PI * 
     &                (ABS(diacomb(inew,i))/2.0)**2. * velcomb(inew,i) *
     &                                                   tptcomb(inew,i)
               endif
               sumdiam =  sumdiam + diacomb(inew,i)**2
            endif
            hgtmax =  MAX( hgtmax, hgtcomb(inew,i) )
            sumxloc = sumxloc + xloccomb(inew,i)
            sumyloc = sumyloc + yloccomb(inew,i)
            nsumloc = nsumloc + 1
         enddo
c upd 17nov2015 if( nsumflow .GT. 0 ) flowavg = sumflow / FLOAT(nsumflow)
         if( nsumflow .GT. 0 ) flowavg = sumflow 
c
c  --- replace the existing parameters with these new values --
c
         if( dianew(inew) .LT. 0. ) dianew(inew) = sumdiam**0.5
         if( dianew(inew) .EQ. 0. ) dianew(inew) = -sumdiam**0.5
         if( hgtnew(inew) .LE. 0. ) hgtnew(inew) = hgtmax
         if( tptnew(inew) .LE. 0. .AND. sumflow .GT. 0. ) 
     &                          tptnew(inew) = sumtpt / sumflow
         if( velnew(inew) .LE. 0. ) velnew(inew) = flowavg / 
     &                            (PI * (ABS(dianew(inew))/2.0) ** 2.0)
         if( nsumloc .GT. 0 ) then
            xlocnew(inew) = sumxloc / FLOAT( nsumloc )
            ylocnew(inew) = sumyloc / FLOAT( nsumloc )
         endif
c
c  --- replace the master stack list parameters with the 
c      newly calculated values  ----
c
         if( idxmaster(inew) .GT. 0 ) then
             hgtlist(idxmaster(inew)) = hgtnew(inew)
             dialist(idxmaster(inew)) = dianew(inew)
             vellist(idxmaster(inew)) = velnew(inew)
             tptlist(idxmaster(inew)) = tptnew(inew)
             xloclist(idxmaster(inew)) = xlocnew(inew)
             yloclist(idxmaster(inew)) = ylocnew(inew)
c
c  --- also update the emissions values ---
c
             do ispc=1,nspecs
                do ihr=1,MXHOUR
                   emsstk(idxmaster(inew),ispc,ihr) = 0.
                   do i=1,nstkcomb(inew)
                      emsstk(idxmaster(inew),ispc,ihr) = 
     &                     emsstk(idxmaster(inew),ispc,ihr) +
     &                              spccomb(inew,i,ispc,ihr) 
                   enddo
                enddo
             enddo
         endif
c
c  --- next new stack
c
      enddo
c
      ierr = ISUCES
      goto 9999
c
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
