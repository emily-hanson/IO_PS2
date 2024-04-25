# IO_PS2

Getdata_nokey.R is code that produces MoonopolyData_On.rds   
    - To run yourself you would have to set up a google API key which is connected to a bill source.   
    - This uses the shapefile_prov.shp to get the lat long bounds for Ontario.   
    - hapefile_prov.shp is called some gibberish in the code (its orginal file name when doenloaded form statistics Canada)  

Cleandata.R takes MoonopolyData_On.rds and cleans it to workable data   
    - Need to define markets   
    - Considering census division as market, could also easily use CSD which is more granular.  
    - need to import market characteristics  
    - To run this, you need to downlaod shape files from statistics Canada for census division or whatever we call a market   
    - These shape files come from https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm  

shapefile_prov.shp is here for easy access.   
    - Census division shape files are to large to store here.   
    - Can download from: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm  
    - Used 2016 shapefiles instead of 2021 for no good reason  

Latex Files:  
        - writeup.tex  
        - moonoply.bib  
