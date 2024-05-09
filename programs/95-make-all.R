#######################################################################
# Master script
#######################################################################

## Clear out existing environment
gc()
rm(list = ls()) 
## Set master directory where all sub-directories are located

CPS_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable.csv"
CPS_all_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_All.csv"
CPS_hispanic_mean <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_hispanic_merge.csv"
CPS_hispanic_mean_county <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_mean_reg_merge_county.csv"
CPS_asian <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_Asians.csv"
CPS_hispanic_mean_hh <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_hispanic_merge_hh.csv"
CPS_interracial <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_Blacks.csv"
ACS_path <- "~/Dropbox/Research/My Research Data and Ideas/ACS-migration/ACS_DataTable.csv"

### GiT directories
mdir <- "~/Dropbox/Research/My Research Data and Ideas/austin-project"
datasets <- paste0(mdir,"/data/datasets")
raw <- paste0(mdir,"/data/raw")
tables_wd <- paste0(mdir,"/output/tables")
figures_wd <- paste0(mdir,"/output/figures")
programs <- paste0(mdir,"/programs")
thesis_tabs <- paste0(mdir,"/my_paper/tables")
thesis_plots <- paste0(mdir,"/my_paper/figure")
pres_tabs <- paste0(mdir,"/presentations/nov-19-2023-sea-presentation/tables")
pres_plots <- paste0(mdir,"/presentations/nov-19-2023-sea-presentation/figures")


### run do files and scripts

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package

### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")