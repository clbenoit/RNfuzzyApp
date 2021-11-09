# RNfuzzyApp

---

RNfuzzyApp is an application to analyse aligned RNA-seq data and do fuzzy clustering.</br>
</br>

With RNfuzzyApp, you have chosen a user-friendly, web-based R shiny application to analyze your RNA-seq data. RNfuzzyApp allows you to perform:

-     data normalization
-     differential expression analysis
-     Mfuzz fuzzy clustering of time-series data
-     interactive visualization of your data
-     enrichment analysis
-     orthology assignments
-     ID conversion


Detailed instructions on how to use RNfuzzyApp can be found in the user manual provided in this repository (RNfuzzApp_UserManual.pdf) Find below details on how to install and run RNfuzzyApp, as well as a very brief instruction on how to pre-process your data. </br>

---

**INSTALLATION**

RNfuzzyApp was built under R version 4.0.0 and runs under  R 3.6 or higher. Installing RNfuzzyApp is easy and requires only a few commands to be executed within the R environment (R or RStudio). 

First, you need to install the shiny library in R. For this, type in your R environment

**install.packages("shiny")**

You might have to select a repository from which to download the library. 
Then activate the library in R by typing

**library(shiny)**

Second, download the RNfuzzyApp software. You can do this either by using the git command, if you have it installed at your computer. Choose first the location (folder), where you want to install RNfuzzyApp. Then type:

**git clone https://gitlab.com/habermann_lab/rna-seq-analysis-app.git**

Alternatively, simply download the software using the download button from the gitlab repository https://gitlab.com/habermann_lab/rna-seq-analysis-app to download and save RNfuzzyApp at your chosen location. 

Using the R command line, type the command:

**runApp(“/my_path_to_the_app/rna-seq-analysis-app-master-App/App”)** 

where my_path_to_the_app is the path on your computer to the folder to which you have downloaded RNfuzzyApp.

In RStudio, 

**open global.R and click ‘Run App’**

All required packages will automatically install and RNfuzzyApp will be opened in the web-browser.

To increase the maximum size of input data, you can run :
**options(shiny.maxRequestSize = n*1024^2)**
with n, the wanted size in MB

---
**TROUBLESHOOTING YOUR INSTALLATION**

Depending on your system, you might encounter errors that we cannot forsee. We have collected a couple of systems-specific errors we have been confronted with here. 

**R issues**
markdown does not seem to be installed with versions of R 4 or higher. If you miss the markdown package, the RNfuzzyApp web-server will not start and you will get an error message in the browser that indicates that the markdown pacakge is not found or unknown. In this case, you will have to install the markdown package within the R environment. To do so, type:

**install.packages("markdown")**

Then, relaunch RNfuzzyApp by typing 

**runApp(“/my_path_to_the_app/rna-seq-analysis-app-master-App/App”)**

**Mac OS related problems**
If you are using a Mac OS, you might have to install XQuartz for Mfuzz to run: https://www.xquartz.org/ , at least in the Big Sur environment. XQuartz will install an X11 environment, which is required to run parts of the RNfuzzyApp. 

**Linux MINT related errors**
In Linux Mint with a basic installation of R, we found issues related to curl/openssl, libssl, and libxml2. In our case, the required packages were missing from the Linux installation. To resolve these issues, you will have to install the required packages on the command line of your Linux system.
On Ubuntu or Mint, or Debian, you can use the apt-get commands to do so; or just talk to your system administrator to help you with installing these packages on your machine.

Usually, you use a series of commands like this:

**sudo apt-get update**

To install a missing curl software:

**sudo apt-get install libcurl4-openssl-dev**

To install the libssl-dev tools:

**sudo apt-get install libssl-dev**

To install the libxml2-dev tools:

**sudo apt-get install libxml2-dev**

Your errors might not be exactly the same depending on your system. The error messages are usually displayed in the R window (where you started RNfuzzyApp). If you see that the browser window which runs RNfuzzyApp gets grey, it means the App is stuck and you are missing one library. Check then your R window and look at your error. Typically, a web-search with the error will help you find the source of your install problem.

---

**DATA PREPROCESSING**

RNfuzzyApp is an R shiny app that requires raw or normalized read counts as input. For getting read counts, you need to pre-process your sequencing data using a standard procedure that includes e.g. quality control (using a tool like fastQC, read mapping to the genome (using a tool like STAR read aligner and finally, read counting (using a tool like FeatureCounts). 

As an input table, RNfuzzyApp requires a csv file, with geneIDs as rows and the  conditions, timepoints or covariates as columns. And it must be named as condition_replicate (whereby condition can be experimental condition or time-point).

Sample data can be found at: https://gitlab.com/habermann_lab/rna-seq-analysis-app/-/tree/master/App/test_files 

rnfuzzyapp_dea_test_data_tabula_muris.csv is a mouse dataset for testing the differential expression and simple hierarchical clustering functions of RNfuzzyApp and contains raw read counts of mouse skeletal muscle at 3 months, 12 months and 24 months of age from the Tabula muris senis project (https://tabula-muris-senis.ds.czbiohub.org/).  

rnfuzzyapp_mfuzz_test_data_spletter_2019.csv is for testing the fuzzy clustering using mFuzz function of RNfuzzyApp and contains normalized mean read counts of the developing Drosophila leg (see GEO database entry GSE143430 https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE143430). 

---
