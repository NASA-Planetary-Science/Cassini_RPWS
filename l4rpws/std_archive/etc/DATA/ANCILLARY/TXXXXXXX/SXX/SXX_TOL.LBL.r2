/* label standards identifiers */                                             
PDS_VERSION_ID                = PDS3                                          
                                                                              
/* pds label for a rpws spectogram */                                         
RECORD_TYPE                   = FIXED_LENGTH                                  
RECORD_BYTES                  = 80                                            
FILE_RECORDS                  = %(recs)-4s                                          
                                                                              
/* pointers to start records of objects in file, std ref3.5:5.3.3.2 */        
^RPWS_TIME_ORDERED_TABLE      = ("%(seq)s_TOL.TAB",1)                             
                                                                              
/* identification data elements - data product labels, std ref3.5:5.3.4.1 */  
DATA_SET_ID                  = {"CO-V/E/J/S/SS-RPWS-4-SUMM-KEY60S-V1.0",      
                                "CO-V/E/J/S/SS-RPWS-2-REFDR-ALL-V1.0",        
                                "CO-V/E/J/S/SS-RPWS-3-RDR-LRFULL-V1.0",       
                                "CO-V/E/J/S/SS-RPWS-2-REFDR-WBRFULL-V1.0",    
                                "CO-V/E/J/S/SS-RPWS-2-REFDR-WFRFULL-V1.0"}    
PRODUCT_ID                    = "%(seq)s_TOL"                                     
PRODUCT_VERSION_ID            = "1.00"                                        
PRODUCT_TYPE                  = ANCILLARY                                     
INSTRUMENT_HOST_NAME          = "CASSINI ORBITER"                             
INSTRUMENT_HOST_ID            = CO                                            
INSTRUMENT_NAME               = "RADIO AND PLASMA WAVE SCIENCE"               
INSTRUMENT_ID                 = RPWS                                          
MISSION_PHASE_NAME            = "TOUR"                                        
TARGET_NAME                   = {"SATURN","SOLAR SYSTEM"}                     
START_TIME                    = %(start-scet)sZ                        
STOP_TIME                     = %(stop-scet)sZ                        
SPACECRAFT_CLOCK_START_COUNT  = "%(start-sclk)s"                            
SPACECRAFT_CLOCK_STOP_COUNT   = "%(stop-sclk)s"                            
PRODUCT_CREATION_TIME         = %(create-date)s                                    
STANDARD_DATA_PRODUCT_ID      = RPWS_TOL                                      
                                                                              
/* descriptive data elements */                                               
OBJECT                        = RPWS_TIME_ORDERED_TABLE                       
  INTERCHANGE_FORMAT          = ASCII                                         
  ROW_BYTES                   = 80                                            
  ROWS                        = %(rows)-4s                                          
  COLUMNS                     = 2                                             
                                                                              
  OBJECT                      = COLUMN                                        
    NAME                      = BEGIN_TIME                                    
    DATA_TYPE                 = TIME                                          
    START_BYTE                = 1                                             
    BYTES                     = 21                                            
    DESCRIPTION               = "Spacecraft Event Time of the RPWS instrument 
                                 operation."                                  
  END_OBJECT                  = COLUMN                                        
                                                                              
  OBJECT                      = COLUMN                                        
    NAME                      = RPWS_OPERATION                                
    DATA_TYPE                 = CHARACTER                                     
    START_BYTE                = 27                                            
    BYTES                     = 51                                            
    DESCRIPTION               = "RPWS instrument operation, such as a Trigger 
                                 command to place RPWS in a specified mode,   
                                 or an individual instrument command."        
  END_OBJECT                  = COLUMN                                        
                                                                              
END_OBJECT                    = RPWS_TIME_ORDERED_TABLE                       
                                                                              
END                                                                           
