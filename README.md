<h1 align="center"> Wi-Fi Indoor Positioning System (IPS) <br/> based on <br/>KNN Algorithm + Trilateration Technique </h1>

```
 📦Indoor_positioning_system           
 ┣ 📂etc                                // Supplementary materials
 ┃ ┣ 📄annotated_map.png
 ┃ ┣ 📄floorplan.jpg
 ┃ ┗ 📄Overview.pdf
 ┣ 📂src                                // Source code
 ┃ ┣ 📂Data_Visualization                   // Data Visualization
 ┃ ┃ ┣ 📂Grid_plot                              // Plot of APs + grids
 ┃ ┃ ┃ ┣ 📄grid.png
 ┃ ┃ ┃ ┣ 📄grid.Rmd
 ┃ ┃ ┃ ┣ 📄IPS_Offline.RData
 ┃ ┃ ┃ ┣ 📄IPS_Online.RData
 ┃ ┃ ┃ ┣ 📄Offline_grid.png
 ┃ ┃ ┃ ┣ 📄Online_grid.png
 ┃ ┃ ┃ ┣ 📄Rooms.png
 ┃ ┃ ┃ ┣ 📄Rooms_2.png
 ┃ ┃ ┃ ┗ 📄WAP.png
 ┃ ┃ ┗ 📂Shiny_app                              // Visualization via Shiny app
 ┃ ┃ ┃ ┗ 📄app.R
 ┃ ┣ 📂Method-KNN                           // KNN method
 ┃ ┃ ┣ 📂clean_data
 ┃ ┃ ┃ ┣ 📄Fun-findNN.RData
 ┃ ┃ ┃ ┣ 📄Fun-floorErrorMap.RData
 ┃ ┃ ┃ ┣ 📄Fun-Ori2Angle.Rdata
 ┃ ┃ ┃ ┣ 📄Fun-predXY.RData
 ┃ ┃ ┃ ┣ 📄Fun-reshapeSS.RData
 ┃ ┃ ┃ ┣ 📄Fun-selectTrain.RData
 ┃ ┃ ┃ ┣ 📄IPS_AP_Locations.RData
 ┃ ┃ ┃ ┣ 📄IPS_offline.RData
 ┃ ┃ ┃ ┣ 📄IPS_online.RData
 ┃ ┃ ┃ ┣ 📄IPS_testingData.RData
 ┃ ┃ ┃ ┗ 📄IPS_trainingData.RData
 ┃ ┃ ┣ 📂raw_data
 ┃ ┃ ┃ ┣ 📄accessPointLocations.txt
 ┃ ┃ ┃ ┣ 📄offline.final.trace.txt
 ┃ ┃ ┃ ┗ 📄online.final.trace.txt
 ┃ ┃ ┣ 📄Step.1_Data_Cleaning.qmd
 ┃ ┃ ┣ 📄Step.1_Data_Cleaning.R
 ┃ ┃ ┣ 📄Step.2_Data_Analysis.qmd
 ┃ ┃ ┣ 📄Step.2_Data_Analysis.R
 ┃ ┃ ┣ 📄Step.3_Data_Visualization.qmd
 ┃ ┃ ┣ 📄Step.3_Data_Visualization.R
 ┃ ┃ ┣ 📄Step.Final_Complete_Code.qmd
 ┃ ┃ ┣ 📄Step.Final_Complete_Code.R
 ┃ ┃ ┗ 📄Step.Final_Complete_Code.Rmd
 ┃ ┣ 📂Method-Trilateration                 // Trilateration method
 ┃ ┃ ┗ 📄Trilateration.Rmd
 ┃ ┣ 📂raw_data                             // Raw data
 ┃ ┃ ┣ 📄accessPointLocations.txt
 ┃ ┃ ┣ 📄offline.final.trace.txt
 ┃ ┃ ┗ 📄online.final.trace.txt
 ┃ ┗ 📄clean_data.R                         // Data tidying
 ┣ 📄LICENSE
 ┣ 📄README.md
 ┣ 📄ips-ref.md                          // Sources for paper
 ┗ 📄agenda.md                          / Members' role
```

 **PS**: Please go to [`Agenda.md`](https://github.com/cyrus-pdx/Indoor_positioning_system/blob/main/agenda.md) to update yourselves or check others' update.
