#Process Simulation Data

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,here,qdapRegex,data.table)

here::i_am("code/r_code/00b_process_simulation_data.R")

#data
#YOU MUST INDICATE WHETHER YOU WANT TO PROCESS THE BASELINES OR THE EXPERIMENT RESULTS
baselines = FALSE #set to TRUE if you want to process baseline data


if (baselines == TRUE){
  path = "data/raw_abm_results/baselines"
} else {
  path = "data/raw_abm_results/" 
}


temp <- list.files(
  path = here(paste0(path)),
  pattern = "*\\.csv$", 
  full.names = TRUE) 

temp = as.data.frame(temp)
colnames(temp) = "file"

if (baselines == TRUE){
temp = temp %>% 
        mutate(string_one = as.character(ex_between(file,"raw_abm_results/baselines/","jobid")),
               string_two = as.character(ex_between(file,"nreplicates=100_",".csv"))) %>%
  select(-file) %>%
  #distinct() %>%
  group_by(string_one,string_two) %>%
  arrange(string_one,string_two) %>%
  mutate(model_id = cur_group_id())
}

if (baselines == FALSE){
  temp = temp %>% 
    mutate(string_one = as.character(ex_between(file,"raw_abm_results/","jobid")),
           string_two = as.character(ex_between(file,"nreplicates=1000_",".csv"))) %>%
    select(-file) %>%
    #distinct() %>%
    group_by(string_one,string_two) %>%
    arrange(string_one,string_two) %>%
    mutate(model_id = cur_group_id())
}


temp = temp %>%
  distinct


if(baselines == TRUE){
  saveRDS(temp, file=here("data/baseline_id_list_temp.RDS"))
  } else {
  saveRDS(temp, file=here("data/model_id_list.RDS"))
  }
#iterate along the list of files, 
#finding the ones that are from the same model specs
#and then loading the data for those specific files (i.e., by each model)



for (i in 1:nrow(temp)){
 
    model_filenames <- intersect(list.files(
      path = here(paste0(path)),
      pattern = paste(temp$string_one[i]), 
      full.names = TRUE),
      list.files(
        path = here(paste0(path)), 
        pattern = paste(temp$string_two[i]), 
        full.names = TRUE) 
      )  
    
    
    
    #find instances where there are too many files, and keep most recent 10
    #issue with file saving/program failing on cluster and the files not
    #getting deleted, I think?
    if (length(model_filenames) > 10) {
      
        info = file.info(
          intersect(
            list.files(
              path = here(paste0(path)),
              pattern = paste(temp$string_one[i]), 
              full.names = TRUE),
            list.files(
              path = here(paste0(path)), 
              pattern = paste(temp$string_two[i]), 
              full.names = TRUE) ))
        info$model = rownames(info)
        
        test = cbind(model_filenames,info)
        rownames(test) = NULL
        test = test[rev(order(lubridate::ymd_hms(test$mtime))),]
        test = test[1:10,]
        model_filenames = test$model_filenames %>% unlist
        rm(test,info)
        gc()
      }
    
    #id is the array id used in the sbatch call, 
    #so to get a single sim, you group by id and by rep_idx below 
    #(also called ensemble, but I find that confusing, so we get rid of that)
    
    dat = map_dfr(model_filenames, fread, .id = 'id_temp')

    dat = dat %>%
      rename(id = id_temp)

    #No recovered data necessary for this analysis, so we filter out
    dat = dat %>%
      filter(status !="R")

    #save as RDS because it's much smaller
  if (baselines == TRUE) {
  saveRDS(dat,here(paste0("data/clean_abm_results_noRstatus/baselines/",temp$string_one[i],"_",temp$string_two[i],".RDS")))
  rm(dat)
  gc()
  } else {
    saveRDS(dat,here(paste0("data/clean_abm_results_noRstatus/",temp$string_one[i],"_",temp$string_two[i],".RDS")))
    rm(dat)
    gc()
  }
    
}


