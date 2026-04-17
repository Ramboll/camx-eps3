c**** CNTLEM.INC 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    Include file for user input parameters and variables for CNTLHR
c    program                                                                    
c                                                                               
c                                                                               
c-----------------------------------------------------------------------        
c    Log:                                                                       
c-----------------------------------------------------------------------        
c                                                                               
c-----------------------------------------------------------------------        
c    Userin variables:                                                          
c-----------------------------------------------------------------------        
c                                                                               
c    lprntc  L   print option flag                                              
c                 = TRUE, print county and total control tables                 
c                 = FALSE, print only total tables (default)                    
c    lexact  L   flag to determine if the control factors are exact 
c                matches
c                 = TRUE, only exact matches will be checked
c                 = FALSE, best match criteria will be used
c                                                                               
      logical*4  lprntc                                                         
      logical*4  lexact
c                                                                               
c     Common blocks                                                             
c                                                                               
      common /lpdat/ lprntc, lexact
