# Indoor WiFi fingerprinting/ trilateration from a provided dataset. 
  Files:
  - agenda.md:                    
  - etc:                          contains supplementary information
    - accessPointLocations.txt    ap locations mac and spatial info
    - annotated_map.png           updated floorplan
    - floorplan.jpg               layout of data collection area
  - grid.Rmd                      spatial evaluation of data
  - IPS.qmd                       new version of .Rmd to tidy raw data
  - raw_data:                     contains original data files
    - offline.final.trace.txt     original offline data 
    - online.final.trace.txt      original online data
  - scripts:                      contains script files for processing the data
    - tidying_raw_data            script to tidy raw off/online data
  - tidy_data:                    tidy off/online data
  - trilat.Rmd                    create location system