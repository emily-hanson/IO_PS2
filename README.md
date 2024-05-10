# IO_PS2

Here we house the R scripts used in the paper for PS2. Other files used (need for replication) are accessible online.     
  
File Directory:  
    R scripts:    
        Create_Markets.R    
        GetData_SearchoverGrid.R      
        GetData_SearchbyMarket.R     
        making tables and figures as in BR 1991.R   
    Data:  
        Markets_Data.rts     
        Market_Data_moreInd.csv (.rts available by request)   
        Vet_Data_ONsubset.rts    
  
Available Online:   
    Province Shape File                  lpr_000a16a_e.shp  
    CMA Shape File                       lcma000b16a_e.shp  
    Population Centers Shape File        lpc_000a21a_e.shp  
    Market Characteristics Data          98-401-X2021009_English_CSV_data.csv  
  
Accessing Online Files:  
    For shape files:  
    - Download from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21  
    - Either Cartographic Boundary Files (CBF) or Digital Boundary Files (DBF) is fine for these purposes  
    For Market Characteristics  
    - Download from: https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E  
    - choose population centre row, csv  
    
Code Structure  

Create_Markets.R combines various statistics canada shape files and market characteristics at the population center level to create the list of markets used    
    - Produces: Markets_Data.rts / .csv   
    - Define Market    
        -  A Population center (i.e. a contiguous area with a population density of 400 person per square km and with a population 1,000 or more)     
            plus a 1 km buffer around the population center     
        -  Not in a CMA    
        -  whose boundary is 20 km from a CMA    
        -  whose boundary is 10 km from another population center     
    - More about population centres here: https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=geo049a     
  
GetData_SearchoverGrid.R is code that calls the Google API to pull a list of all establishments  
    - Produces: Vet_Data_ONsubset.rts or similar file for all Canada  
    - To run yourself you would have to set up a google API key which is connected to a billing source   
    - The code creates a rectangular grid of lat long points across canada's provinces  
    - Then I use the shapefile of provinces to remove points in the US or in territories     
    - Each point represents a google API call with a radius of 50 km around that point  

GetData_SearchbyMarket.R      
    - Produces: Market_Data_wInd.csv    
    - Using markets defined in Create_Markets.R, scrapes data from Google API for industries in those markets and adds those counts to the markets file   
    - Use data from API pull to check how many establishments are in that market. Both are shape files. Markets are polygons, establishments are points    
    - This approach requires the markets to be defined first and then calls the api for a point that is the centroid of the population center.    
    - This results in many fewer calls (ie cheaper) than overGrid. The downside is you don't get the total number of establishments in CA.    

making tables and figures as in BR 1991.R   
    - Lance's file for analysis    
    - Makes tables and figures from Timothy F. Bresnahan and Peter C. Reiss 1991   

dentistcode.R is slightly editted version of making tables and figures as in BR 1991.R used for our main analysis  
    - Nalinda's file   
  
  Vet_Data_ONsubset.rts    
    - Provides an example of the establishment resukts from the data scrape.   
    - Vet data results for all of CA, Market results for Funeral Homes, Dentists, and Mechanics is available upon request.   
