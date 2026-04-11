c*** CTLTOT.INC 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    Include file for emissions totals for CNTLEM program                       
c                                                                               
c-----------------------------------------------------------------------        
c   LOG:                                                                        
c-----------------------------------------------------------------------        
c                                                                               
c-----------------------------------------------------------------------        
c   Parameter for array bounds:                                                 
c-----------------------------------------------------------------------        
c                                                                               
      integer*4 MXSCSC                                                          
c                                                                               
      parameter (MXSCSC = 15000)
c                                                                               
c-----------------------------------------------------------------------        
c   Parameters for index in totals array:                                       
c-----------------------------------------------------------------------        
c                                                                               
c   IDXIN    I   index of total input emissions for mobile sources              
c   IDXOUT   I   index of total output emissions                                
c   IDXSKP   I   index of skipped emissions                                     
c                                                                               
      integer*4 IDXIN                                                           
      integer*4 IDXOUT                                                          
      integer*4 IDXSKP                                                          
c                                                                               
      parameter( IDXIN  = 1 )                                                   
      parameter( IDXOUT = 2 )                                                   
      parameter( IDXSKP = 3 )                                                   
c                                                                               
c-----------------------------------------------------------------------        
c   Variables for totals of emissions:                                          
c-----------------------------------------------------------------------        
c                                                                               
c   totems   R   array for emissions of criteria pollutants                     
c   totifp   R   emissions of input criteria by county and SCC/SIC              
c   totofp   R   emissions of output criteria by county and SCC/SIC             
c   totisp   R   emissions of input speciated by county and SCC/SIC             
c   totosp   R   emissions of input speciated by county and SCC/SIC             
c   totspc   R   array for emissions of speciated                               
c                                                                               
      real*8 totems(IDXSKP,MXCRIT)                                               
      real*4 totifp(0:MXSCSC,0:MXFIP,MXCRIT)                                     
      real*4 totofp(0:MXSCSC,0:MXFIP,MXCRIT)                                     
      real*8 totspc(IDXSKP,MXSPEC)                                              
c                                                                               
      common /totdt1/ totems
      common /totdt2/ totifp
      common /totdt3/ totofp
      common /totdt4/ totspc                            
c                                                                               
c-----------------------------------------------------------------------        
c   Record processing totals:                                                   
c-----------------------------------------------------------------------        
c                                                                               
c   nrec     I   number of output records written                               
c                                                                               
      integer*4   nrec(IDXSKP)                                                  
c                                                                               
      common /trec/ nrec                                                        
c                                                                               
c-----------------------------------------------------------------------        
c   Arrays for SCC/SIC codes:                                                   
c-----------------------------------------------------------------------        
c                                                                               
c   sccsic  C   array for SCC/SIC code pairs                                    
c   idxsrt  I   array of pointers to sorted SCC/SIC array                       
c   nscsic  I   number of SCC/SIC code pairs in the array                       
c   lcodctl I   flag to determine if this code pair is controlled
c                                                                               
      character*14 sccsic(MXSCSC)                                               
      integer*4    idxsrt(MXSCSC)                                               
      integer*4    nscsic                                                       
      logical*4    lcodctl(MXSCSC)
c                                                                               
      common /sccnum/ nscsic, idxsrt                                            
      common /sccchr/ sccsic, lcodctl
c                                                                               
c-----------------------------------------------------------------------        
c   Parameters for process flags:                                               
c-----------------------------------------------------------------------        
c                                                                               
c   IFLPRJ  I  position in process flag for /PROJECT .../ controls              
c   IFLSRC  I  position in process flag for /SOURCE CATEGORY/                   
c   IFLCTY  I  position in process flag for /FIPS CODE/ controls                
c                                                                               
      integer*4 IFLPRJ                                                          
      integer*4 IFLSRC                                                          
      integer*4 IFLCTY                                                          
c                                                                               
      parameter( IFLPRJ = 1 )                                                   
      parameter( IFLSRC = 2 )                                                   
      parameter( IFLCTY = 3 )                                                   
c                                                                               
c-----------------------------------------------------------------------        
c   Process flag variables:                                                     
c-----------------------------------------------------------------------        
c                                                                               
c   ctlflg   C   string which shows which discretionary controls have           
c                been applies to each source category                           
c                                                                               
      character*(IFLCTY) ctlflg(MXSCSC)                                         
c                                                                               
      common /flgchr/ ctlflg
c                                                                               
c-----------------------------------------------------------------------        
c   Variables for storing the hours of input inventory:
c-----------------------------------------------------------------------        
c
c   ibegin  I   beginning hour of the inventory
c   iendin  I   ending hour of the inventory
c   nhours  I   number of hours in inventory
c   emshr   R   array of criteria pollutants for each hour
c   spchr   R   array of speciated pollutants for each hour
c
      integer*4 ibegin    
      integer*4 iendin    
      integer*4 nhours 
      real*4    emshr(MXCRIT,MXHOUR+1)
      real*4    spchr(MXSPEC,MXHOUR+1)
c
      common /ctlinv/ ibegin, iendin, nhours, emshr, spchr
  
