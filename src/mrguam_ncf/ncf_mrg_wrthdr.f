C**** NCF_MRG_WRTHDR,F
c
      subroutine ncf_mrg_wrthdr( ierr )
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
      include 'layers.inc'
      include 'ncf_iodat.inc'
      include 'mrguemis.inc'
      include 'iomrguam.inc'
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
      integer*4 strmin             
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
      call ncf_set_vars_base()
      call ncf_set_tstep(ibgdat,begtim,iendat,endtim)
      call ncf_set_specatt_ems(spec_units,spec_long_name,
     &                                             spec_desc,spec_coords)
      nlayer = 1
      call ncf_set_global('EMISSIONS ',1,ibgdat,begtim,
     &                                            iendat,endtim,nspecs)
      action = 'Writing header to output emissions file: '//ouamfl(:strmin(ouamfl))
      call ncf_wrt_dim(jerr,action,ncf_mrgout,nxcell,nycell,1,nspecs)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_global(jerr,action,ncf_mrgout,nspecs,spcnam)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_vars_base(jerr,action,ncf_mrgout)
      if( jerr .NE. ISUCES ) goto 9999
      call ncf_wrt_vars_species(jerr,action,ncf_mrgout,nxcell,nycell,nspecs,
     &           spcnam,spec_units,spec_long_name,spec_desc,spec_coords)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- header is complete, change NetCDF filemode ---
c
      jerr = nf_enddef(ncf_mrgout)
c
c  --- write the data for grid description variables ---
c
      action = 'Writing grid definition data to oytput emissions file: '
     &                                           //ouamfl(:strmin(ouamfl))
      call ncf_wrt_data_grid(jerr,action,ncf_mrgout,nxcell,nycell,
     &               xorig*1000.,yorig*1000.,dxcell*1000.,dycell*1000,
     &                                             1,cellat,cellon)
      if( jerr .NE. ISUCES ) goto 9999
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
