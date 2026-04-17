**** GRDSRGRPT.INC 

c-----------------------------------------------------------------------
c
c    Include file for emissions totals by 
c                          FIPS/ASC/SrgCd/Flg for GRDEM program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXFSSF  I   maximum number of FIPS/ASC/SurgCd/Flag totals array
c
       integer*4 MXFSSF
c
       parameter ( MXFSSF = 200000 )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   fssflst       C   array of codeslist 
c   emsifssf      R   array for input FIPS/ASC/Surg/Flag
c                    emissions of criteria pollutants (gm)
c   emsifssf      R   array for output FIPS/ASC/Surg/Flag
c   nfssf         I   number of codes
c   last_fssf     C   last FSSF code processed
c   idx_last_fssf I  index of the last FSSF code processed
c
      character*22 fssflst(MXFSSF)
      character*22 last_fssf
      integer*4    idx_last_fssf
      integer*4    nfssf
      real*4       emsifssf(MXFSSF,MXCRIT)
      real*4       emsofssf(MXFSSF,MXCRIT)
c
      common /fsschr/ fssflst, last_fssf
c
      common /fssdat/ idx_last_fssf, nfssf, emsifssf, emsofssf
c
c-----------------------------------------------------------------------
c   Parameters for the hash list:
c-----------------------------------------------------------------------
c
c  MXHASH_FSSF   I  maximum hash value allowed in the has function
c
      integer*4 MXHASH_FSSF
c
      parameter( MXHASH_FSSF = 153078 )
c
c-----------------------------------------------------------------------
c   Variables for the hash list:
c-----------------------------------------------------------------------
c
c   idxhsh_fssf_first  I  index of the first entry for the hash value
c   idxhsh_fssf_next   I  index into the next available hash entry for
c                         this hash value
c
      integer*4 idxhsh_fssf_first(MXHASH_FSSF)
      integer*4 idxhsh_fssf_next(MXFSSF)
c
      common /hashfssfdat/ idxhsh_fssf_first, idxhsh_fssf_next
