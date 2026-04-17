C**** RDVSLMAP.F 
c
      subroutine rdvslmap( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the Vessel/Stack lookup table and stores the data in
c    common block variables to by used by the PSTSHP module.
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c    feb  2018 gwilson  added plumefrac calculation for alll layers
c                       to support 3D emissions
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iopstshp.inc'
      include 'layers.inc'
      include 'pstshpstk.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines called:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) string
      character*10        keywrd
      integer*4           i, j, ilay
      real*4              diff, bottmp, toptmp
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- initialize the array counter ---
c
      nvslmap = 0
c
c   --- increment counter and check for array overflow ---
c
  111 continue
      nvslmap = nvslmap + 1
      if( nvslmap .GT. MXSTKMAP ) goto 7000
c
c   --- read next record ---
c
      read(IORVSL,'(A)',ERR=7002,END=222) string
c
c   --- get the data for this record ----
c
      read(string,'(A10,1X,A10,1X,F10.0,1X,F10.0)',ERR=7004,END=222)
     &                           ascmap(nvslmap), vslmap(nvslmap), 
     &                        plumebot(nvslmap), plumetop(nvslmap)
      call rgtjst( ascmap(nvslmap) )
      call lftjst( vslmap(nvslmap) )
c
      bottmp = plumebot(nvslmap)
      toptmp = plumetop(nvslmap)
      if( lelvfl .OR. l3demiss ) then
          call plume_distrib(nvslmap,bottmp,toptmp)
      else
c
c  --- set surface fraction to 1 if no elevated output requested ---
c
            plumefrac(1,nvslmap) = 1.0
            do ilay=2,nlayer
               plumefrac(ilay,nvslmap) = 0.
            enddo
      endif
c
c  --- get the next record ---
c
      goto 111
c
c   --- processed entire file, echo to message file ---
c
  222 continue
      nvslmap = nvslmap - 1
      if( nvslmap .LE. 0 ) goto 7003
c
c   ---- echo the number of records read to the message file ---
c
      write(IOWMSG,'(/,2A,I8,/)',ERR=9999) 'Number of records read ',
     &                       'from Vessel/Stack XREF file: ',nvslmap
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading the ',
     &                            'Vessel/Stack cross reference file.'
      write(ioerr,'(A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(2A,//)',ERR=9999) 
     &             'Increase the parameter MXSTKMAP and recompile.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A,I8,//)',ERR=9999) 'ERROR: Reading the ',
     &     'Vessel/Stack cross reference file at line: ',nvslmap
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Reading the ',
     &                            'Vessel/Stack cross reference file.'
      write(ioerr,'(2A,//)',ERR=9999) 'No records found in the ',
     &                          'Vessel/Stack cross reference file.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A,I8)',ERR=9999) 'ERROR: Reading the ',
     &     'Vessel/Stack cross reference file at line: ',nvslmap
      write(ioerr,'(A)',ERR=9999) 'Line read: '
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
