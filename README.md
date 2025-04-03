# FAO_DIFA2025
Welcome to the GitHub repository for the DIFA Report for the United Nations Food and Agricultural Organisation (FAO)! The aim of this repository is to produce quantitative estimates of the agricultural yield and economic losses due to disasters in 198 countries and territories around the world.

## Developers
This code base was developed by:
- Hamish Patten
- Ignacio Acosta
With the original DIFA methodology code base developed also with:
- Priti Rajagopalan
- Nina Deliu
With non-coding support from the FAO Statistics Division, including:
- Piero Conforti
- Zehra Zaidi
- Jose Rosero Moncayo

## Folder structure
There are two main folders for this project, the first, *'RCode'*, holds the scripts required to automatically extract all the data of the report, define, train and validate the different models, as well as post-processing and plotting functions. The second folder, *'Data'* hosts the data that is pulled in throughout usage of the *RCode* scripts.

### RCode Folder
There are five folders that comprise the RCode folder:
- **Data_Wrangling**: extract EM-DAT, FAOSTAT, Desinventar, UCDP, World Bank and other databases, as well as transforming these sources into formats we can use,
- **Models**: set up the different models required, including estimating the crop and cattle losses and imputing the item-price data per country,
- **Setup**: holds everything needed to run all of the DIFA scripts, including automatic installation of packages and loading all the scripts,
- **Plotting**: exactly what it says on the tin - plot the DIFA results,
- **Other**: miscellaneous files and scripts developed along the way, entirely unimportant and inconsequential to the DIFA.

### Data Folder
The data folder has three main sub-folders:
- **RawData**: this folder is where all the untreated data, such as EM-DAT, are dumped,
- **Results**: any modelling results, including disaster severity, are dumped here,
- **Taxonomies**: taxonomies are important to compare one database to another, such as comparing hazards across Desinventar to EM-DAT.

## Models & Parameterisation
Inside the '*Models*' folder, you will find many different attempts at modelling disaster-related commodity losses, as well as the disaster severity models and model-parameterisation methodologies. A brief explanation of these is as follows:
- **.stan files**: Stan is a statistical modeling package that uses Bayesian inference, including MCMC, variational inference and standard optimisation techniques, all these files are attempts to model disaster-related commodity losses,
- **DisasterSeverity_Model.R**: this script estimates crop and cattle losses for any EM-DAT record, based upon Desinventar data,
- **EstInitParams.R**: some model parameters can be approximated from the data, calculated here,
- **Methods.R**: methods to parameterise the models are written here, including the wrapper functions to Stan,
- **data_simulation.R**: in order to check whether the models are correctly parameterised, we simulate commodity datasets that are affected by disasters.

## Installation Guide
In order to install and setup this project, make sure the working directory in *R* is in the main folder ('*FAO_DIFA2025*') and then run the following:
```
source("./RCode/Setup/GetPackages.R")
source("./RCode/Setup/Functions.R")
```
This will do everything for you! The other thing to note is to make sure that you download an API key from EM-DAT, to do so, [login to their data portal](https://public.emdat.be/) and click the tab *User Information* and on the right you will see '*API Key*'.

## Main.R File
This is the most important file of the entire repository. It installs and loads the required packages, extracts and transforms the data, defines model choice and hyperparameters, trains and predicts the disaster severity models, as well as trains and predicts the disaster-related commodity loss models. The two functions one can observe correspond to disaster-related losses for EM-DAT and Desinventar, in that order.