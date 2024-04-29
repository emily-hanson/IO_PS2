# IO_PS2

Here we house the R scripts, the API call output data, latex files for collaboration, and figures/png. Other files used (need for replication) are accessible online.     
  
File Directory:  
    R scripts:  
        Getdata_nokey.R    
        Cleandata.R      
    Data:  
        Vet_Data_canada.rds  
        Vet_Data_ONsubset.rts
        Markets_Data.rts
    Latex Files:  
        writeup.tex  
        moonoply.bib   
  
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
  
Getdata_nokey.R is code that calls the Google API to pull a list of all establishments  
    - Produces: Vet_Data_canada.rds   
    - To run yourself you would have to set up a google API key which is connected to a billing source   
    - The code creates a rectangular grid of lat long points across canada's provinces  
    - Then I use the shapefile of provinces to remove points in the US or in territories     
    - Each point represents a google API call with a radius of 50 km around that point  
    - There are a total of 3251 of these points  

Cleandata.R takes output from Getdata.R, combines it with various statistics canada shape files, and market characteristics at the population center level  
    - Produces: Markets_Data.rts
    - Define Market  
        -  A Population center (i.e. a contiguous area with a population density of 400 person per square km and with a population 1,000 or more)   
            plus a 1 km buffer around the population center
        -  Not in a CMA  
        -  whose boundary is 20 km from a CMA  
        -  whose boundary is 10 km from another population center  
    - Use data from API pull to check how many establishments are in that market. Both are shape files. Markets are polygons, establishments are points  
    - More about population centres here: https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=geo049a   
  
  
Notes:  
    - An alterative way to structure the data retrevial would be to build the markets first and then call the api for a point that is the centroid of the population center. This would result in many fewer calls (which would be cheaper). The downside is you don't get to know the total number of establishments or run robustness for adding buffers on the geegraphic bounds of the market.  
