c**** CLCVERT.F
c
      subroutine clcvert( ierr )
c
c-----------------------------------------------------------------------
c
c    Sets the vertical layer factors by fire class and hour.
c
c    The point specific fire emissions are distributed based on the
c    fraction of the total emissions in each layer for a given hour.
c    A factor is calculated for each fire class, hour, and vertical layer.
c    Layers are 0:MXLAY, where entry 0 is the surface fraction.
c
c    The algorithms that determine the fraction of emissions in each
c    layer, by hour and fire size is taken from
c       WRAP 2002 PhII EI Report 2005 July 22
c       Section 2.4.3 Fire Size Classes and Plume Profile Calculations
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:  may 2015 mjimenez
c       The fraction by vertical layer is no longer required.  CAMx
c       now supports a plume rise distribution override feature which
c       allows the plume bottom and plume top to specified.
c       New arrays have been defined to support this feature.
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
       include 'param.inc'
       include 'firebin.inc'
       include 'layers.inc'
       include 'usrpstfir.inc'
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
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4           nb, hr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c   behour   R   buoyant efficiency as function of hour of day
c                data values taken from WRAP 2002 PhII EI Report
c                                       2005 July 22 (Table 11)
      real*4 behour(24)
c
      data behour /0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,
     &             0.06,0.10,0.20,0.40,0.70,0.80,0.90,0.95,
     &             0.99,0.80,0.70,0.40,0.06,0.03,0.03,0.03/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
c
c     --- initialize factors to zero ---
c
      do nb = 1, nfbin
         do hr = 1, MXHOUR
            plmovrtop(nb,hr) = 0.0
            plmovrbot(nb,hr) = 0.0
            srffrac(nb,hr)   = 0.0
            elvfrac(nb,hr)   = 0.0
            aglfrac(nb,hr)   = 0.0
            srfbot(nb,hr)    = 0.0
            srftop(nb,hr)    = 0.0
         enddo
      enddo
c
c  --- fill vertical factors for each fire class and hour ---
c
      do nb = 1, nfbin
         do hr = 1, MXHOUR
c
c  --- for each fire class and bin determine plume top/bottom ---
c  --- cap top of plume to max layer height ---
c
            plmovrtop(nb,hr) = (behour(hr)**2) * (befbin(nb)**2) 
     &                                                * topfbin(nb)
            if ( plmovrtop(nb,hr) .GT. aglht(nlayer) )
     &                                 plmovrtop(nb,hr) = aglht(nlayer)

            plmovrbot(nb,hr) = (behour(hr)**2) * (befbin(nb)**2) 
     &                                             * botfbin(nb)
            if ( plmovrbot(nb,hr) .GT. plmovrtop(nb,hr) ) goto 7000
c
c  --- surface fraction ---
c  --- if outputing all elevated emissions ---
c  ---    save top/bottom for surface fraction ---
c
            srffrac(nb,hr) = 1. - (behour(hr) * befbin(nb))
            elvfrac(nb,hr) = 1. - srffrac(nb,hr)

            if ( lelvall ) then
               srfbot(nb,hr) = 0.1
               srftop(nb,hr) = WLAY1
               aglfrac(nb,hr) = srffrac(nb,hr)
c
c  --- if layer 1 is lower than WRAP layer1 (within 10%) adjust surface ---
c  --- fraction to low level file and set top/bottom of surface above layer1 ---
c
            elseif ( aglht(1) .LT. WLAY1*.9 ) then
               aglfrac(nb,hr) = srffrac(nb,hr) * (1. - aglht(1)/WLAY1)
               srffrac(nb,hr) = srffrac(nb,hr) * aglht(1)/WLAY1
               srfbot(nb,hr) = aglht(1)
               srftop(nb,hr) = WLAY1
            endif

         enddo  ! next hour
      enddo  ! next bin
c
c   ---- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Calculating ',
     &                'vertical layer fractions.'
      write(ioerr,'(2(10X,A,1X,I2,/),2(10X,A,1X,F8.2,/))',ERR=9999) 
     &                            'Class: ',nb,
     &                            'Hour: ', hr,
     &                            'Plume Bottom: ', plmovrbot(nb,hr),
     &                            'Plume Top: ', plmovrtop(nb,hr)
      write(ioerr,'(10X,2A,/)',ERR=9999) 'Review layer structure ',
     &                           'and fire class bin defintions.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
