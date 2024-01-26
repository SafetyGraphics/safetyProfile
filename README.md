# safetyProfile

The {safetyProfile} package contains a shiny application that allows users to view subject-level profile reports for patients participating in clinical trials. The visualizations included in the package present a patient-centric summary of parameter changes over the trial period, including laboratory results, concomitant medications, adverse events along with participant demographics data. The {safetyProfile} can be used as a stand-alone shiny application and at the same time its flexible shiny modules allow the tool to be used as a part of the {safetyGraphics} R package (robust safety monitoring framework). The package has been developed as part of the <a target="_blank" href="https://safetygraphics.github.io/">Interactive Safety Graphics (ISG) workstream</a> of the ASA Biopharm-DIA Safety Working Group. 

## Using the app

Install the package from github with `devtools::install_github('safetyGraphics/safetyProfile', ref="main")` then run stand-alone app:

```
library(safetyProfile)
profileApp()
```
