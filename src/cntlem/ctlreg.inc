c**** CTLREG.INC 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    Include file for control factors all types:                                
c       motor vehicle.                                                          
c                                                                               
c-----------------------------------------------------------------------        
c    Log:                                                                       
c-----------------------------------------------------------------------        
c                                                                               
c-----------------------------------------------------------------------        
c          Project factors (projections) for points                             
c-----------------------------------------------------------------------        
c                                                                               
c  Parameters:                                                                  
c                                                                               
c   MXPPT   I   maximum growth factors by FIP/subrgn, 2-digit SIC               
c                                                                               
       integer*4 MXPPT                                                          
c                                                                               
       parameter ( MXPPT = 2000 )                                               
c                                                                               
c  Common block variables:                                                      
c                                                                               
c   ncdppt  I  number of input growth factors for points                        
c   pptfip  C  array for storing FIP/subregion codes for points                 
c   pptsic  C  array for storing SIC codes for growth factors for points        
c   pptfac  R  growth factors for points                                        
c                                                                               
       character*10 pptfip(MXPPT)                                               
       character*10 pptsic(MXPPT)                                               
       integer*4    ncdppt                                                      
       real*4       pptfac(MXPPT)                                               
c                                                                               
       common /chrppt/ pptfip, pptsic                                           
       common /datppt/ ncdppt, pptfac
c                                                                               
c-----------------------------------------------------------------------        
c          Project factors (projections) for area                               
c-----------------------------------------------------------------------        
c                                                                               
c  Parameters:                                                                  
c                                                                               
c   MXPAM   I   maximum growth factors by FIP/subrgn, (ASC code)                
c                                                                               
       integer*4 MXPAM                                                          
c                                                                               
       parameter ( MXPAM = 12000 )                                               
c                                                                               
c  Common block variables:                                                      
c                                                                               
c   ncdpam  I  number of input growth factors for area                          
c   pamfip  C  array for storing FIP/subregion codes for area                   
c   pamsic  C  array for storing SIC codes for growth factors for area          
c   pamfac  R  growth factors for area                                          
c                                                                               
       character*10 pamfip(MXPAM)                                               
       character*10 pamsic(MXPAM)                                               
       integer*4    ncdpam                                                      
       real*4       pamfac(MXPAM)                                               
c                                                                               
       common /chrpam/ pamfip, pamsic                                           
       common /datpam/ ncdpam, pamfac
c                                                                               
