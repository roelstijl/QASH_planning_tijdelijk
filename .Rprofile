.First = function ()
  {
    # Load the required settings (function below)
    settings <<- load_settings()
    
  }

load_settings = function(){
  # Load package names from settings
  cat("Loading settings for project, on failure please reload packages.\nModify .Rprofile to change these settings")  
  
  # Load user specific things and sources function  
  suppressMessages(require(utils))
  options(stringsAsFactors = FALSE)  
  packages = read.table("Settings\\List_of_packages.csv")[,1]
  cfg=list()
  
  # Install the packages
  cat("\n\nInstalling packages (~\\Settings\\List_of_packages.csv):\n")
  for(pckname in packages[!(packages %in% rownames(installed.packages()))]) install.packages(pckname)
  
  # Require the packages
  cat("\n\nLoading packages (~\\Settings\\List_of_packages.csv):\n")
  for (m in 1:length(packages)) {cat(",",packages[m]); suppressMessages(library(packages[m],character.only=TRUE))}
  
  # Source all the files in the folder
  cat("\n\nSourcing files in folder / loading functions:\n")
  l_ply(list.files(pattern=".[R]$"),function(x) {cat(",",x); try(source(x))})
  
  # Determine if settings file exists
  cat("\n\nSetting settings:\n")
  cfg$Nodename = Sys.info()["nodename"]
  if(file.exists("Settings/Settings.xlsx")){
    cat("Loading settings from file (~\\Settings\\Settings.xlsx).... ")
    settingsall = data.table(read.xlsx(file="Settings/Settings.xlsx",1))
  }else{
    settingsall = data.table()
  }
  
  # Determine if computer in the settings file
  if (any(cfg$Nodename %in% settingsall$Nodename))
    settings = settingsall[Nodename==cfg$Nodename][1]
  else{
    cat("Select a folder using the popup..... ")
    maindir = choose.dir(default = getwd(),caption = "Select a folder for the data, \nsubdirectories will be generated")
    settings=data.table(
      Nodename         = cfg$Nodename,
      MainDir          = maindir,
      Source_Data_1A           = file.path(maindir,"1A_Source_Data"),
      Raw_Data_1B              = file.path(maindir,"1B_Raw_Data"),
      Exploration_Data_2A      = file.path(maindir,"2A_Exploration_Data"),
      Input_Data_2B            = file.path(maindir,"2B_Input_Data"),
      Analysis_Data_2C         = file.path(maindir,"2C_Analysis_Data"),
      Descriptive_Analysis_3A  = file.path(maindir,"3A_Descriptive_Analysis"),
      Predictive_Analysis_3B   = file.path(maindir,"3B_Predictive_Analysis"),
      Prescriptive_Analysis_3C = file.path(maindir,"3C_Prescriptive_Analysis"),
      Output_Data_4A           = file.path(maindir,"4A_Output_Data"),
      Visuals_Data_4B          = file.path(maindir,"4B_Visuals_Data"))
    l_ply(names(settings),function(x) (settings[,eval(x) :=strrep(get(x),"\\","/")]))
    
    cat("Saving settings to file.... ")
    write.xlsx(rbind(settings,settingsall,fill=T),file="Settings/Settings.xlsx",row.names=F)
  }
  
  cat("Creating directories.... ")
  dir.create(settings$Source_Data_1A, showWarnings = FALSE)
  dir.create(settings$Raw_Data_1B, showWarnings = FALSE)
  dir.create(settings$Exploration_Data_2A, showWarnings = FALSE)
  dir.create(settings$Input_Data_2B, showWarnings = FALSE)
  dir.create(settings$Analysis_Data_2C, showWarnings = FALSE)
  dir.create(settings$Descriptive_Analysis_3A, showWarnings = FALSE)
  dir.create(settings$Predictive_Analysis_3B, showWarnings = FALSE)
  dir.create(settings$Prescriptive_Analysis_3C, showWarnings = FALSE)
  dir.create(settings$Output_Data_4A, showWarnings = FALSE)
  dir.create(settings$Visuals_Data_4B, showWarnings = FALSE)
  
  cat("Done")
  
  cat("\n\nBuilt by R Stijl, M Musterd (Bearingpoint BV) & Jacco Heres (Alliander NV)\n")
  
  return(settings)
}