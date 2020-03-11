# DATA MANAGEMENT SYSTEM

## 1. Overall file structure
The overall file structure for this project is as follows:

####metadata
This folder contains all of the site-level metadata, such as plot- and site-level information. It also contains metadata files for each of the separate assays used.\

####raw data
The raw data folder includes raw data for original field data, lab experiment data, and non-original primary data, like existing NEON data. Each of these three types of data should be in different folders.

####code
The code folder includes separate code folders for data processing, statistical model code, and process model code. The data processing code is used to process the raw data files.

####calculated data
The calculated data folder contains the data for statistical and modeling analysis that is exported from the data processing scripts in the code folder.

There is also a file in the parent folder that describes which files should be uploaded to GitHub. We are only uploading code and metadata too GitHub.

## 2. How to decide what should be a separate raw data file?
Within the raw data folder, different assays should never be in the same file (see Spreadsheet organization). In addition, there should be separate .csv files for every "run"" of an assay. A "run" is defined as a contiguous unit of time in which you are working on an assay. For instance, if you run substrate-induced respiration in the morning, work on something else for a while, and then run another batch in the afternoon, those should be entered as separate files.

## 3. File naming
For raw data, files should follow the following naming convention:

assay-or-data-type_date-collected.csv

For example, soil-sir_march-3-2020.csv includes substrate induced respiration data measured on March 3, 2020. Note that the date should not refer to the date the samples are collected. That information should be in the sample meta-data folder associated with each sample's unique id.

Also, for example, litter-mass_march-3-2020.csv could have the litter mass data from NEON accessed on March 3, 2020. Note, that there may be cases where the raw data from different NEON sites may have different structures and be incompatible. In that case, the raw data files should be separate by site and the file names should reflect that: litter-mass_HARV_march-3-2020.csv


## 4. Spreadsheet organization
We follow the principles of "tidy data" as established by Hadley Wickham (Wickham 2014 J Stat Soft). I strongly recommend reading this paper as a starting point. The foundational principle of tidy data is that variables should always be in columns and observations in rows. Relatedly, the following problems should be avoided:

#### -Column headers are values, not variable names
#### -Multiple variables are stored in one column
#### -Variables are stored in both rows and columns
#### -Multiple types of observational units are stored in the same table
#### -A single observational unit is stored in multiple tables

In addition to these basic tidy principles from Hadley Wickham, I will add two others:

### 4a. Raw data should be entered and stored in raw data spreadsheets, but calculated data should be calculated using code and stored in separate spreadsheets generated from the code. This will likely seem to be the most unfamiliar to biogeochemists in part because raw data for one assay can depend on calculated data from previous assays. There will be some adjustment to having to generate raw data, run a script to calculate new data and generate a new file, and then use those new data to start the new assay. However, we feel that the benefits of this approach are worth it. There are two main advantages: 1) it helps cut down on cluttered data files with many columns of subsequent calculations; 2) it helps cut down on human error in generating calculations; 3) it allows you to generate many calculations (through code) that you wouldn\'92t necessarily want to create as separate columns in a spreadsheet, for instance looking at the difference between moisture content at 50 vs 65 vs 75 percent water holding capacity.

### 4b. Do not create Excel files with multiple tabs. Each spreadsheet should be saved as a .csv, so it can be read across platforms. Multiple tabs from excel files should be stored as separate files.

### 4c. Every sample should have a unique ID code. This should be a code that refers to both the project and where the sample is from within the project. This makes it much simpler to merge data sets then having multiple variables that uniquely define an observation.


## 5. Variable and file naming
Following from above, variables should always have names that are not numeric values. To make variable names standardized and clear, we use the following variable naming system:

### 5a. Variables that are raw, entered data should be named with dots instead of spaces. For instance, tin.mass instead of "tin mass"
### 5b. Variables that are calculated from raw data should be named without punctuation and capitalization used to clarify new words. For instance, gravimetric moisture instead of "gravimetric moisture"
### 5c. Data sets and files stored in a file structure should be named with underscores instead of spaces. For instance, soil_properties instead of 'soil properties.'

In addition, variable and file names should always be descriptive, and informative, while as short as possible.


## 6. Variable units and metadata
The units of the variables in a raw data set should NOT be added to the end of a variable name, nor should they be added in a separate row below the variable name. For each spreadsheet with raw data, there should be a supplementary meta-dataset that includes units. This meta-dataset should have the same name as the raw data set with metadata added to the end of the title. For instance, soil_properties_metadata. This file should be stored in a separate metadata folder. Any other metadata should also be added into this file, such as notes from the assay.

The units of calculated data should be clearly documented in code, but not stored in the calculated data file. If you wish you can generate a metadata file for calculated data but this isn't strictly necessary if the units and other metadata information are clearly documented within the code.


## 7. GitHub
We will not be making any of the data--raw or processed--publicly available until published. The data should be stored in the Dropbox folder, which should be synced to your local desktop. Manipulations to data should not be done through Dropbox's online tools.

To avoid publishing data to GitHub, I have created a .gitignore file. This file describes which folders and files are ignored by GitHub.

If you are unfamiliar working with git and GitHub, here is a link to a useful online tutorial to familiarize yourself: https://guides.github.com/activities/hello-world/

The basic principle is that we will be using this resource to share code across the institutional partners (Yale, TNC, NCAR). We recommend creating separate branches for intermediate activities, such as NCAR's exploration of new code structure.


## 8. Using processing code
For each lab assay, there is a separate processing script. This script, by default, reads in all of the existing raw files and compiles them to two outputs: one is data aggregated by replicate and one that is not aggregated. To generate the calculated code, you should open the R file associated with the assay that you are looking to do calculations for and execute all of that code.


## References

Wickham, H. 2014. Tidy Data. Journal of Statistical Software 59 (10).