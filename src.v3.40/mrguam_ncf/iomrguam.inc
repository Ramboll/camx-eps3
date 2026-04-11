c**** MRGUAM.INC 
c
c-----------------------------------------------------------------------
c
c   Include file for the MRGUAM module.
c
c-----------------------------------------------------------------------
c   Program version:
c-----------------------------------------------------------------------
c
      character*(MXSTR) VERSON
c
      parameter ( VERSON = 'EPS3 MRGUAM module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c   Array bounds parameters:
c-----------------------------------------------------------------------
c
c   MXMRG  I  maximum number of files that can be merged
c
      integer*4 MXMRG
      parameter( MXMRG = 100 )
c
c-----------------------------------------------------------------------
c   I/O unit numbers:
c-----------------------------------------------------------------------
c
c   IORINP  I  user input file
c   IOWMSG  I  output message file
c   MXUNIT  I  base unit number for input UAM files
c
      integer*4 IORINP
      integer*4 IOWMSG
      integer*4 MXUNIT

      parameter (IORINP = 10)
      parameter (IOWMSG = 20)
      parameter (MXUNIT = 30)
c
c-----------------------------------------------------------------------
c   Filenames:
c-----------------------------------------------------------------------
c
c   innm         C   input UAM filenames
c   msgfl        C   message filename
c   ouamfl       C   output UAM filename
c   uinpfl       C   user input filename
c   ncf_mrgemis  I   array of NCF file ID for input files
c   ncf_mrgout   I   NCF file ID for output file
c   is_netcdf    L   flag to determing if input file is NetCDF format
c
      character*(MXSTR) innm(MXMRG)
      character*(MXSTR) msgfl
      character*(MXSTR) ouamfl
      character*(MXSTR) uinpfl
      integer*4         ncf_mrgemis(MXMRG)
      integer*4         ncf_mrgout
      logical*4         is_netcdf(MXMRG)
c
      common /mspciochr/  innm, msgfl, ouamfl, uinpfl
      common /mspciodat/  ncf_mrgemis, ncf_mrgout, is_netcdf
c
c-----------------------------------------------------------------------
c   Species list data:
c-----------------------------------------------------------------------
c
c     table  C  species names in each input file
c     idxspc I  maps from file & species into species list from uamcntl
c     iscnt  I  number of species in each file
c     intnam I  the species names in integer form for the output file
c     iflcnt I  number of input files which are being merged
c
      character*10 table(MXMRG,MXSPEC)
      integer*4    idxspc(MXMRG,MXSPEC)
      integer*4    iscnt(MXMRG)
      character*4  intnam(MXSPEC,10)
      integer*4    iflcnt
c
      common /mspcchr/ table, intnam
      common /mspcdat/ idxspc, iscnt, iflcnt
c
c-----------------------------------------------------------------------
c   Segment of UAM header:
c-----------------------------------------------------------------------
c
c   seg4   I   array of data from UAM header
c
      integer*4 seg4(4)
c
      common /segdat/ seg4
