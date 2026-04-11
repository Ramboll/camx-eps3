C**** NCF_PIG_WRTHDR,F

      subroutine ncf_pig_wrthdr( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the header and stores the variables in common varibales
c    then it creates the program name/date/time string and writes
c    the rest of the header
c    If the output file type is UAM the header is just skipped
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
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
      include 'uamregn.inc'
      include 'uamcntl.inc'
      include 'ncf_iodat.inc'
      include 'iopigems.inc'
      include 'pigemstot.inc'
      include 'stkpigems.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External functions
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*200 action
      character*20  spec_units(MXSPEC), spec_long_name(MXSPEC)
      character*60  spec_desc(MXSPEC), spec_coords(MXSPEC)
      integer       jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c --- calculate the grid desciption variables for NetCDF output ---
c
      call grdprep(nxcell,nycell,cellon,cellat,xorig,yorig,dxcell,
     &                                          dycell,deltax,deltay)
c
c  --- set and write the NetCDF variables and dimensions ---
c
      action = 'Writing output NetCDF point source file.'
      call ncf_set_vars_base_pts()
      call ncf_set_tstep(ibgdat,begtim,iendat,endtim)
      call ncf_set_specatt_pts(spec_units,spec_long_name,
     &                                             spec_desc,spec_coords)
      call ncf_set_global('PTSOURCE  ',1,ibgdat,begtim,
     &                                       iendat,endtim,nspecs+4)
      call ncf_wrt_dim_pts(jerr,action,ncf_iowemis,nstklist,nspecs)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_global_pts(jerr,action,ncf_iowemis,nspecs,spcnam)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_vars_base_pts(jerr,action,ncf_iowemis)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_vars_species_pts(jerr,action,ncf_iowemis,nstklist,nspecs,
     &           spcnam,spec_units,spec_long_name,spec_desc,spec_coords)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- header is complete, change NetCDF filemode ---
c
      jerr = nf_enddef(ncf_iowemis)
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Errror Messages:
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
