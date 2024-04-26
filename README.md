# IO_PS2

Getdata_nokey.R is code that produces MoonopolyData_On.rds   
    - To run yourself you would have to set up a google API key which is connected to a bill source.   
    - This uses the shapefile_prov.shp to get the lat long bounds for Ontario.   
    - shapefile_prov.shp is called some gibberish in the code (its orginal file name when doenloaded form statistics Canada)  

Cleandata.R takes MoonopolyData_On.rds and cleans it to workable data   
    - Define Market  
        -  A Population center (i.e. a contigous area with a population density of 400 person per square km and with a populaton 1,000 or more)   
        -  Not in a CMA  
        -  whose boundry is 150 km from a CMA  
        -  whose boundry is 30 km from another population center  
    - More about poptulation centres here: https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=geo049a   
    - To run this, you need to downlaod shape files from statistics Canada from https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm  

shapefile_prov.shp is here for easy access.   
    - Census division shape files are to large to store here.   
    - Can download from: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm  
    - Used 2016 shapefiles instead of 2021 for no good reason  

Latex Files:  
        - writeup.tex  
        - moonoply.bib  
