Welcome to MaB!

MaB (Meta-analysis of Behavior) is a project investigating the effects of early life adversity on behavior in rodents. 
For more information about the project (e.g. preprint, dataset), checkout https://osf.io/ra947/.
For questions/suggestions/remarks, contact Valeria @ v.bonapersona-2@ucmutrecht.nl 



Here, we report the script for the statistical analysis. A few comments about the files: 
- MAB_Data.csv contains the data not yet curated
- MAB_BehaviourTests_Variables.csv contains the classification of behavioral tests and variables used
- Each row corresponds to a comparison between a control and a early-life-adversity group

How to use: 
- define your working directory (not included in the code!)
- run 01_datasetPreparation.R . This file creates a .RData file necessary for the analysis (data curated as available at https://zenodo.org/record/2540657#.XEGPP2ko9aR )
- run 02_MAB_analysis.R for the analysis (hypotheses-testing as well as exploratory).
- run 03_MAB_riskBiasAssessment.R

Images will be saved in the MAB_figures folder.

