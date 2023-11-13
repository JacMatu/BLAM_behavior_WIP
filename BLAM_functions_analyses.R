# Basic functions for data wrangling and extracting accuracy metrics for all tasks
# in the BLAM project. They use a "sliding window" to look for responses based 
# on the width of the window from each stimuli flagged as a TARGET. 
# Jacek Matuszewski

# Arguments to provide: 
# SUBJECTS
# GROUP
# TASK
# TARGET COLUMN 


# This should ideally combine both AVERAGE ACROSS CONDITIONS & CONDITION-SPECIFIC ANALYSES 




blam_accuracy_tw <- function(data, tw){
    
    
    temp_hits <- c()
    
    n_targets <- sum(as.numeric(data$target_func), na.rm = TRUE)
    
    for(iRow in 1:nrow(temp_data_clean)){
        
        #If this trial is a target
        if(temp_data_clean$target_func[iRow]== 1){
            
            #Note the target onset
            tar_onset <- temp_data_clean$onset[iRow]
            
            #Slice the DF based on the TW? 
            tw_data <- temp_data_clean %>% 
                filter(onset <= tar_onset + tw & onset > tar_onset-1)
            
            
            #Check if there is a response within that time window
            if(Reduce("|", tw_data$trial_type=='response')){
                
                hit <- 1
                temp_hits <- c(temp_hits, hit)
                #temp_hits[iRow] <- 1
            } #else {
            #  hit <- 0
            #  temp_hits <- c(temp_hits, hit)
            #}
            
        } 
    }
    accu_percent <- (sum(temp_hits)/n_targets)*100
    return(accu_percent)
    
}


# #blam_localizers_analyze_beh_accuracy <- function(inputs, group, subjects, task_name, target_col){
#     
#     # Prepare a data frame
#     assign(paste('df',task_name,group, sep='_'),
#         value = data.frame(stringsAsFactors = FALSE))
#     
#     
#     
#     # FIRST PASS: AVERAGE ACROSS CONDITIONS
#     for(i in seq_along(subjects)) {
#         
#         bids_sub <- paste0('sub-',group,subjects[i])
#         
#         #Grab events from this sub in this task and exclude the trash files
#         # (part-mag_events.tsv & part_phase_events.tsv)
        # sub_events <- list.files(path = paste(inputs,bids_sub, sep = '/'),
        #                          pattern = paste0(bids_sub,'_.*_' ,'task-',
        #                                           task_name, '.*',
        #                                           "_events\\.tsv$"),
        #                          recursive = TRUE) %>%
        #     str_subset(pattern = ".*_part-mag_events\\.tsv$", negate = TRUE) %>%
        #     str_subset(pattern = ".*_part-phase_events\\.tsv$",negate = TRUE)
#         
#         # Not really necessary for localizers that only had one run
#         
#         
#         temp_data<-read.table(paste(inputs, bids_sub, sub_events,
#                                     sep = '/'), header=TRUE)
#         
#         #Sometimes first event is flagged as target, which is impossible. 
#         # If that's the case, drop it from the number of targets
#         if(as.numeric(temp_data$soundTarget[1]) == 1){
#             n_targets <- sum(as.numeric(temp_data$soundTarget), na.rm = TRUE)-1
#         } else {
#             n_targets <- sum(as.numeric(temp_data$soundTarget), na.rm = TRUE)
#         }
#         
#         #n_responses <- temp_data %>% filter(trial_type == "response") %>% nrow()
#         
#         #Main subject df to wrangle, only one run now!
#         temp_data_clean <- temp_data %>% 
#             #get rid of trigger events
#             filter((!grepl('trigger', trial_type))) %>% 
#             #drop first event which is always flagged as a target
#             slice(-1) %>% 
#             #mutate(soundTarget = recode(soundTarget, 'n/a' = '2')) %>% 
#             mutate(soundTarget = recode(soundTarget, 'n/a' = '0')) %>% 
#             mutate(soundTarget = as.numeric(soundTarget)) 
#         
#         #Get pairs of subsequent events to look for a soundTARGET followed by RESPONSE
#         #pairs <- data.frame(first = head(temp_data_clean$soundTarget, -1), 
#         #                   second = tail(temp_data_clean$soundTarget, -1))
#         #temp_tab <- table(pairs)
#         
#         
#         ## NEW SOLUTION: HOW TO FIND THE RESPONSES WITHIN THE TW? 
#         temp_hits <- c()
#         
#         for(iRow in 1:nrow(temp_data_clean)){
#             
#             #If this trial is a target
#             if(temp_data_clean$soundTarget[iRow]== 1){
#                 
#                 #Note the target onset
#                 tar_onset <- temp_data_clean$onset[iRow]
#                 
#                 #Slice the DF based on the TW? 
#                 tw_data <- temp_data_clean %>% 
#                     filter(onset <= tar_onset + accu_tw & onset > tar_onset-1)
#                 
#                 
#                 #Check if there is a response within that time window
#                 if(Reduce("|", tw_data$trial_type=='response')){
#                     
#                     hit <- 1
#                     temp_hits <- c(temp_hits, hit)
#                     #temp_hits[iRow] <- 1
#                 } #else {
#                 #  hit <- 0
#                 #  temp_hits <- c(temp_hits, hit)
#                 #}
#                 
#             } 
#         }
#         
#         
#         #accu_percent <- (temp_tab[2,3]/n_targets)*100 
#         accu_percent <- (sum(temp_hits)/n_targets)*100
#         
#         #ADD METADATA to the temp df
#         
#         accu_temp <- data.frame(subject = bids_sub, group = groups[1],
#                                 condition = 'average',
#                                 task = tasks[2], accu_percent = 
#                                     accu_percent)
#         
#         #Add this subject to group data
#         df_AudioLoc_blind_accu <- rbind(df_AudioLoc_blind_accu, accu_temp)
#         
#         
#     }
#     
#     
#     
#     
# }