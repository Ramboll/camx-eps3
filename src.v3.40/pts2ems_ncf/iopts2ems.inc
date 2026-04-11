c*** IOGPTS2EMS.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for PTS2EMS_NCF program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater conatining version of the
c                grdem program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 PTS2EMS_NCF module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORINP
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IOWMSG = 20 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   uinpfl   C   user input filename
c   inptsfl  C   input PTSOURCE file
c   oemsfl   C   output gridded NetCDF filename
c   msgfl    C   message filename
c
      character*(MXSTR) uinpfl
      character*(MXSTR) msgfl
      character*(MXSTR) inptsfl
      character*(MXSTR) oemsfl
c
      common /iofil/ uinpfl, inptsfl, oemsfl, msgfl
c
c-----------------------------------------------------------------------
c    Flags for files not used
c-----------------------------------------------------------------------
c
c   lnetcdf_in   L   flag for type of input point source file
c   inpunit      I   file handle for output NetCDF file
c   ncf_iowemis  I   file handle for output NetCDF file
c
      logical*4 lnetcdf_in
      integer*4 inpunit
      integer*4 ncf_iowemis
c
      common /usedat/ lnetcdf_in, inpunit, ncf_iowemis
