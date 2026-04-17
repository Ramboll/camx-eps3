C**** ADD_3DEMISS
c
      subroutine add_3demiss(
     &                       icell, jcell, idxhr, stackbot, stacktop, 
     &                                                 idxspc, spcems )
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
c       icell    I   current column
c       jcell    I   current row
c       idxhr    I   current hour
c       stackbot R   bottom of plume (m from ground)
c       stacktop R   top of plume (m from ground)
c       idxspc   I   index of species in list
c       spcems   R   emissions to be added
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
      include 'pstfirtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4 icell
      integer*4 jcell
      integer*4 idxhr
      real*4    stackbot
      real*4    stacktop
      integer*4 idxspc
      real*4    spcems
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
      integer*4 ilay, icurlay, ibotlay, itoplay, idx_ijcell
      real*4    plumebot, plumetop, plumefrac(MXLAYER), diff, zstk
      real*4    bot, top
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
      plumebot = MAX(0., MIN( stackbot, stacktop ) )
      plumetop = MIN( aglht(nlayer),MAX( stacktop, stackbot ) )
c
c  --- calculate the fraction in each layer ---
c
      diff = plumetop - plumebot
      plumefrac = 0.
      do itoplay = icurlay,nlayer
       if( aglht(itoplay) .GE. plumetop) goto 222
      enddo
      itoplay = nlayer
c
 222  continue
      do ibotlay = 1,icurlay
         if(aglht(ibotlay) .GT. plumebot) goto 333
      enddo
c 
 333  continue
      do ilay = ibotlay,itoplay
        bot = MAX(aglht(ilay-1),plumebot)
        top = MIN(aglht(ilay),plumetop)
        plumefrac(ilay) = (top - bot)/diff
      enddo
c
      do ilay=1,nlayer-1
         if( plumefrac(ilay) .GT. 0. .AND. spcems .GT. 0. ) then
             idx_ijcell = ijcell3d(icell,jcell,ilay)
             if( idx_ijcell .LE. 0 ) then
                num_ijcell3d = num_ijcell3d + 1
                if( num_ijcell3d .GT. MX3DCELLS ) goto 7000
                ijcell3d(icell,jcell,ilay) = num_ijcell3d
                idx_ijcell = num_ijcell3d
             endif
             emiss3d(idxhr,idxspc,idx_ijcell) = 
     &                    emiss3d(idxhr,idxspc,idx_ijcell) + 
     &                                   spcems * plumefrac(ilay)
         endif
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'Maximum number of cells in ',
     &                        'the gridded emissions file exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MX3DCELLS
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter ',
     &                                      'MX3DCELLS and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
