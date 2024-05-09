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
big_data_dir  <- "~/Dropbox/Research/My Research Data and Ideas/austin-project/data/datasets"
### GiT directories
mdir <- here::here()
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
source(file.path(programs,"02-merge-immig-laws-cps-on-county-level.r"))
source(file.path(programs,"03-etwfe.r"))
source(file.path(programs,"04-etwfe-firstgen.r"))
source(file.path(programs,"05-etwfe-secondgen.r"))
source(file.path(programs,"06-etwfe-secondgen-hh.r"))
source(file.path(programs,"07-etwfe-secondgen-hw.r"))
source(file.path(programs,"08-etwfe-secondgen-wh.r"))
source(file.path(programs,"09-etwfe-thirdgen.r"))
source(file.path(programs,"10-etwfe-thirdgen-one.r"))
source(file.path(programs,"11-etwfe-thirdgen-two.r"))
source(file.path(programs,"12-etwfe-thirdgen-three.r"))
source(file.path(programs,"13-etwfe-thirdgen-four.r"))
source(file.path(programs,"14-etwfe-fourthgen.r"))

### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")