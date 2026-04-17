C**** PLUME_DISTRIB
c
      subroutine plume_distrib(
     &                          idxvslmap, stackbot, stacktop )
      implicit none
c
c-----------------------------------------------------------------------
c
c     This routine parses the plume and calculates the amount of 
c     emissions that goes into each layer. The emissions are added
c     to the gridded array for output to the 3D emissions file.
c
c   Arguments:
c
c     Inputs:
c       idxvslmap I   index of vessel in global arrays
c       stackbot  R   bottom of plume (m from ground)
c       stacktop  R   top of plume (m from ground)
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
      include 'layers.inc'
      include 'pstshpstk.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4 idxvslmap 
      real*4    stackbot
      real*4    stacktop
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
      integer*4 ilay, icurlay, ibotlay, itoplay
      real*4    cur_plume_bot, cur_plume_top, diff
      real*4    bot, top, zstk
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- calculate the bottom and top of plume and which layer ---
c
      zstk = 0.5*(stacktop+stackbot)
      do icurlay = 1,nlayer
         if(aglht(icurlay) .GT. zstk) goto 111
      enddo
      icurlay = nlayer
 111  continue
      cur_plume_bot = MAX(0., MIN( stackbot, stacktop ) )
      cur_plume_top = MIN( aglht(nlayer),MAX( stacktop, stackbot ) )
c
c  --- calculate the fraction in each layer ---
c
      diff = cur_plume_top - cur_plume_bot
      do icurlay = 1,nlayer
        plumefrac(icurlay,idxvslmap) = 0. 
      enddo
c
      do itoplay = icurlay,nlayer
       if( aglht(itoplay) .GE. cur_plume_top) goto 222
      enddo
      itoplay = nlayer
c
 222  continue
      do ibotlay = 1,icurlay
         if(aglht(ibotlay) .GT. cur_plume_bot) goto 333
      enddo
c 
 333  continue
      do ilay = ibotlay,itoplay
        bot = MAX(aglht(ilay-1),cur_plume_bot)
        top = MIN(aglht(ilay),cur_plume_top)
        if( top .GT. bot ) plumefrac(ilay,idxvslmap) = (top - bot)/diff
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
