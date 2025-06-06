##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PREPROCESS SMALL MAMMAL CAMERA TRAP IMAGE CLASSIFICATION DATA
## last update 27.01.2023
## script made by Hanna Boehner
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


## DESCRIPTION
## This function is used to preprocess the datasets 'V_rodents_cameratraps_image_classification_lemming_blocks' and 'V_rodents_cameratraps_image_classification_intensive_quadrats'
## The camera traps are set to take two images per motion sensor trigger event and both images are included in the raw dataset
## In addition, a subset of images was classified manually and both, manual and automatic classification are included in the raw dataset
## The function returns a dataset with only one image per trigger event and only one classification (manual or automatic)
## The images are selected after the following criteria:
## If one of the images has a manual classification, this image will be kept
## If both images have a manual classification, the image that contains an animal will be kept, if both images contain an animal, the first image will be kept
## If both image have the same class, the image with the highest confidence will be kept
## If only one image contains an animal, the image with the animal will be kept
## If the images are classified as different animals, the image with the higher confidence will be kept

## The datasets with the image classifications and the image metadata have to be downloaded from the COAT dataportal 
## The function returns a dataset with the following columns (a description of the columns is found in the readme file coming the raw datassets): 
## sn_region, sn_locality, sn_section, sc_type_of_sites_ecological. sn_site
## t_date, t_time, v_image_name, 
## v_trigger_mode, v_sequence, v_event, v_temperature
## v_class_id, v_presence (either automatic or manual), v_confidence_automatic, v_observer_manual, v_type_manual_classification, v_comment_classification, v_comment_meta 


## NB
## Images containing 'new' species, such as mink or frog, are not included in the dataset, these will be returned as 'missing images'
## The data files are rather large and R might crash if it runs out of memory. In this case, you can try to process file by file and restart R after each file
## Or you can try to remove columns you don't need from the files


## PARAMETERS OF THE FUNCTION
## dat_name:    full name of the classification file (including file path) or name of the R object in case the data has been loaded already
## meta_name:   full name of the metadata file (including file path) or name of the R object in case the data has been loaded already
## out_dir:     full path to the folder where the processed files should be saved (optional)
## save:        should the processed datafiles be saved? TRUE or FALSE
## keep_manual: all images from Komagdalen 2016-2018 have been classified manually. Either all manual classification can be kept (keep_manual = "all) or 
##              only manual classification of images with low confidence, mustelid images and quality check images can be kept for a consistent dataset over all years (keep_manual = "low_confidence)
## is.dir:      should the data be loaded (dat_name or meta_name is the path to the files -> is.dir = TRUE) 
##              or data is already loaded (dat_name and meta_name are the names of the R objects, is.dir = FALSE) 


## EXAMPLE

## source("function_preprocess_image_classifications.R")  # load the function

## data_dir <- "small_mammals/V_rodents_cameratraps_image_classification_lemming_blocks"  # path to the classification files
## meta_dir <- "small_mammals/V_rodents_cameratraps_image_metadata_lemming_blocks"  # path to the metadata files

## data_names = dir(data_dir, full.names = TRUE)  # get full filenames of all classification files
## meta_names = dir(meta_dir, full.names = TRUE)  # get full filenames of all metadata files

## ## loop through all files

## dat <- c() # empty object to store the files (as a list)

## for (i in 1:length(data_names)) {
##    dat[[i]] <- preprocess_classifications(dat_name = dat_names[i], 
##                                           meta_name = meta_names[i], 
##                                           save = FALSE,
##                                           keep_manual = "low_confidence", 
##                                           out_dir = "small_mammals/processed_data",
##                                           is.dir = TRUE)
## }




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## FUNCTION
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

preprocess_classifications <- function(dat_name = dat_name, meta_name = meta_name, out_dir = out_dir, save = FALSE, keep_manual = "low_confidence", is.dir = TRUE) {
  
  ## load libraries
  #if (!require('tidyverse', lib.loc = "/mnt/coat-ns8028k/Rlibs")) install.packages('tidyverse', lib = "/mnt/coat-ns8028k/Rlibs"); library('tidyverse', lib.loc = "/mnt/coat-ns8028k/Rlibs")
  #if (!require('lubridate', lib.loc = "/mnt/coat-ns8028k/Rlibs")) install.packages('lubridate', lib = "/mnt/coat-ns8028k/Rlibs"); library('lubridate', lib.loc = "/mnt/coat-ns8028k/Rlibs")
  
  library(tidyverse)
  library(lubridate)
  
  ## clear workspace and free some memory (R will crash if all memory is used)
  suppressWarnings(remove(list = c("dat_1", "dat_2", "dat_3", "dat_all", "dat_animal_empty_bad_quality", "dat_events_manual", "dat_motion", "dat_new", 
                                   "test", "events_manual", "events_same", "events_check", "images_manual", "images_same"))) 
  gc(reset = TRUE)
  
  ## load data
  
  if (is.dir) {
    dat <- read.table(dat_name, header = TRUE, sep = ";")
    meta <- read.table(meta_name, header = TRUE, sep = ";")
  } else {
    dat <- dat_name
    meta <- meta_name
    dat_name <- paste(dat$sn_locality[1], dat$t_date[nrow(dat)])
  }
  
  
  if ("corrupted image" %in% meta$v_comment) {
    if (!all(meta$v_image_name %in% dat$v_image_name)) {
      meta <- meta[-which(meta$v_comment == "corrupted image"),]
      print(paste("images missing in classification file", dat_name))
    }
  }
  
  if(!all(dat$v_image_name %in% meta$v_image_name)) {
    print(paste("check", dat_name))
    break   
  } else {
    print(paste("processing", dat_name))
  }
  
  ## preprocess data
  dat_all <- left_join(meta, dat, by = c("sn_region", "sn_locality", "sn_section", "sc_type_of_sites_ecological", "sn_site", "t_date", "t_time", "v_image_name")) %>% # combine metadata and classifications
    mutate(event = paste(v_trigger_mode, v_event, sn_site, sep = "_"))   # add unique event-id
  
  dat_motion <- filter(dat_all, v_trigger_mode == "motion_sensor")  # only motion sensor images
  
  ## remove rawdata files and free some memory 
  remove(dat)
  remove(meta)
  gc(reset = TRUE)
  
  ## keep the image with manual classification ----------------
  
  ## keep only manual classification for images with low confidence or mustelid and quality check images for komag 2016-2018 (all images were classified manually)
  if(grepl("komagdalen", dat_name) & grepl(c("2016|2017|2018"), dat_name) & keep_manual == "low_confidence") {
    
    dat_motion$keep_manual <- ifelse(dat_motion$v_type_manual_classification %in% c("quality_check_random", "quality_check_classes", "quality_check_confidence_level", "mustelid"), "yes",
                                     ifelse(dat_motion$v_presence_manual == 1 & dat_motion$v_confidence_automatic < 0.9, "yes", "no"))
    events_manual <- unique(dat_motion$event[which(dat_motion$keep_manual == "yes")])
    dat_events_manual <- dat_motion[which(dat_motion$event %in% events_manual & dat_motion$v_presence_manual == 1),]  # dataframe with all images with manual classification
    
  } else {
    events_manual <- dat_motion$event[which(dat_motion$v_presence_manual == 1)]  # all events that have a manual classification
    dat_events_manual <- dat_motion[which(dat_motion$event %in% events_manual & dat_motion$v_presence_manual == 1),]  # dataframe with all images with manual classification
  }
  
  ## images with one manual classification
  images_1_manual <- dat_events_manual[!dat_events_manual$event %in% dat_events_manual$event[duplicated(dat_events_manual$event)],]$v_image_name
  
  ## if one image has and animal and the other is empty or has bad quality, keep the animal
  dat_events_2_manual <- dat_events_manual[!dat_events_manual$v_image_name %in% images_1_manual,]  # all events with two manual classifications
  
  events_animal_manual <- dat_events_2_manual$event[dat_events_2_manual$v_class_id %in% c("aves", "cricetidae", "lem_lem", "mus_erm", "mus_niv", "sor_sp")]  # all events with animal
  events_empty_bad_manual <- dat_events_2_manual$event[dat_events_2_manual$v_class_id %in% c("empty", "bad_quality")]  # al events with empty and bad quality images
  events_empty_animal_manual <- intersect(events_animal_manual, events_empty_bad_manual)  # events with one animal and one empty or bad qualit image
  
  images_empty_animal_manual <- dat_events_2_manual$v_image_name[dat_events_2_manual$event %in% events_empty_animal_manual & dat_events_2_manual$v_class_id %in% c("aves", "cricetidae", "lem_lem", "mus_erm", "mus_niv", "sor_sp")] # events with one animal and one empty or bad qualit image
  
  ## keep the first image of all other images
  dat_events_keep_first_manual <- dat_events_2_manual[!dat_events_2_manual$event %in% events_empty_animal_manual,]
  images_keep_first_manual <- dat_events_keep_first_manual$v_image_name[!duplicated(dat_events_keep_first_manual$event)]
  
  all(events_manual %in% c(events_empty_animal_manual, dat_events_keep_first_manual$event, dat_events_manual$event[dat_events_manual$v_image_name %in% images_1_manual]))
  
  
  ## keep the image with higher confidence if both image have the same class --------------
  
  ## only images without manual classification
  dat_1 <- dat_motion[!dat_motion$event %in% events_manual,]  
  
  ## all events where both images have the same classification
  classes <- unique(dat_all$v_class_id)
  events_same <- vector(mode = "list", length = length(classes))
  
  for (i in 1:length(classes)) {
    events_same[[i]] <- dat_1$event[which(dat_1$v_class_id == classes[i] & dat_1$v_presence_automatic == 1)]
    events_same[[i]] <- events_same[[i]][duplicated(events_same[[i]])]
  }
  
  events_same <- unlist(events_same)
  
  ## keep images with higher confidence
  images_same <- dat_1 %>% filter(event %in% events_same & !is.na(v_confidence_automatic)) %>% 
    group_by(event) %>% 
    slice(which.max(v_confidence_automatic)) %>% 
    arrange(v_image_name) %>% 
    ungroup() %>% 
    select(v_image_name) %>% 
    as.vector() %>% 
    unlist
  
  
  ## keep image with animal if only one image contains an animal --------------
  
  dat_2 <- dat_1[!dat_1$event %in% events_same,]
  
  ## all events where the two images belong to different classes (e.g. empty and animal)
  events_empty <- dat_2$event[which(dat_2$v_class_id == "empty" & dat_2$v_presence_automatic == 1)]  # all events where one of the image is empty
  events_bad_quality <- dat_2$event[which(dat_2$v_class_id == "bad_quality" & dat_2$v_presence_automatic == 1)]  # all events where one of the images has bad quality
  
  events_animal <- c()
  classes <- classes[!classes %in% c("empty", "bad_quality")]
  
  for (i in 1:length(classes)) {
    events_animal[[i]] <- dat_2$event[which(dat_2$v_class_id == classes[i] & dat_2$v_presence_automatic == 1)]
  }
  events_animal <- unlist(events_animal) # all events where one of the images has an animal
  
  
  events_animal_empty <- intersect(events_animal, events_empty)  # all events where one image is empty and the other has an animal
  events_animal_bad_quality <- intersect(events_animal, events_bad_quality)  # all events where one image has bad quality and the other has an animal
  events_empty_bad_quality <- intersect(events_empty, events_bad_quality) # all events where one image is empty and the other has bad quality
  events_animal_animal <- events_animal[!events_animal %in% c(events_animal_empty, events_animal_bad_quality)]  # all images where the two images have been classified as two different animas
  
  
  ## keep the image that has an animal
  dat_animal_empty_bad_quality <- dat_2[dat_2$event %in% c(events_animal_empty, events_animal_bad_quality),]
  images_animal <- dat_animal_empty_bad_quality$v_image_name[which(dat_animal_empty_bad_quality$v_class_id %in% classes & dat_animal_empty_bad_quality$v_presence_automatic == 1)]
  
  ## keep the image with higher confidence if one images is empty and the other has bad quality
  images_empty <- dat_2 %>% filter(event %in% events_empty_bad_quality & !is.na(v_confidence_automatic)) %>% 
    group_by(event) %>% 
    slice(which.max(v_confidence_automatic)) %>% 
    arrange(v_image_name) %>% 
    ungroup() %>% 
    select(v_image_name) %>% 
    as.vector() %>% 
    unlist
  
  ## keep the image with higher confidence if the images have been classified as different animals
  images_animal_animal <- dat_2 %>% filter(event %in% events_animal_animal & !is.na(v_confidence_automatic)) %>% 
    group_by(event) %>% 
    slice(which.max(v_confidence_automatic)) %>% 
    arrange(v_image_name) %>% 
    ungroup() %>% 
    select(v_image_name) %>% 
    as.vector() %>% 
    unlist
  
  
  ## keep single images (if there is only one image per motion sensor trigger) --------------
  dat_3 <- dat_2[!dat_2$event %in% c(events_animal_empty, events_animal_bad_quality, events_animal_animal, events_empty_bad_quality),]
  temp <- as.data.frame(table(dat_3$event))
  event_single <- temp$Var1[temp$Freq == 8]
  images_single <- unique(dat_3$v_image_name[dat_3$event %in%event_single])
  
  ## keep only selected images ------------------
  dat_new <- filter(dat_all, v_image_name %in% c(images_1_manual, images_empty_animal_manual, images_keep_first_manual, images_same, images_animal, images_empty, images_animal_animal, images_single) | v_trigger_mode == "time_lapse") %>% 
    dplyr::rename(v_comment_meta = v_comment.x, v_comment_classification = v_comment.y)
  
  ## check if all events are there
  events <- unique(dat_all$event)
  if (all(events %in% dat_new$event)) {
    print(paste("everything correct:", dat_name))
  } else {
    missing <- dat_all %>% filter(event %in% events[!events %in% dat_new$event]) %>% 
      filter(!duplicated(v_image_name)) %>% 
      select(v_image_name, event, v_comment.y)
    
    print(paste("missing images:", dat_name))
    print(missing)
  }
  
  
  ## Combine manual and automatic classification -----------------------------
  
  if(grepl("komagdalen", dat_name) & grepl(c("2016|2017|2018"), dat_name) & keep_manual == "low_confidence") {
    temp <- dat_new$v_image_name[which(grepl("manual_classification", dat_new$v_type_manual_classification) & dat_new$v_confidence_automatic >= 0.9)]
    dat_new$v_presence_manual[dat_new$v_image_name %in% temp] <- NA
  }
  
  temp <- dat_new$v_image_name[which(dat_new$v_presence_manual == 1)]
  dat_new$v_presence <- ifelse(dat_new$v_image_name %in% temp, dat_new$v_presence_manual, dat_new$v_presence_automatic)
  
  
  ## Some checking -----------------------------
  
  ## check if all images have a classification
  events_check <- dat_new$event[which(dat_new$v_presence == 1)]
  
  ## check if there are images with two classifications
  if (any(duplicated(events_check))) {
    two_species <- events_check[which(duplicated(events_check))]
    dat_two_species <- dat_new %>%  filter(event == two_species) %>% 
      select(v_image_name, v_class_id, v_presence, v_comment_classification)
    print(paste("tow species", dat_name))
    print(dat_two_species)
  }
  
  
  ## check if all images have a classification
  events_check <- events_check[!duplicated(events_check)]
  if (!length(events_check) == length(events)) {
    temp <-  events[!events %in% events_check]
    if (nrow(missing) != 0) temp <- temp[!temp %in% missing$event]
    if(length(temp) != 0) {
      print(paste("needs checking", dat_name))
    }
  }
  
  ## check if there are no duplicated events
  if(!all(as.data.frame(table(dat_new$event))$Freq == 8)) print(paste("needs checking", dat_name))
  
  #test <- dat_new %>% dplyr::group_by(v_image_name) %>% 
   # dplyr::summarise(sum_pres = sum(v_presence, na.rm = TRUE))
  
  ## check that every image has only one classification
  #if(!all(test$sum_pres == 1)) print(paste("needs checking", dat_name)) 
  
  
  ## reformat files ------------------------
  dat_final <- dat_new %>% select(-c(v_presence_automatic, v_presence_manual, event, v_image_name_original)) %>% 
    select(sn_region, sn_locality, sn_section, sc_type_of_sites_ecological, sn_site, t_date, t_time, v_image_name, v_trigger_mode, v_sequence, v_event, v_temperature, 
           v_class_id, v_presence, v_confidence_automatic, v_observer_manual, v_type_manual_classification, v_comment_classification, v_comment_meta)
  
  
  ## Save file -----------------------------
  if (save) {
    file_name <- paste0("preprocessed_image_classifications_", unique(dat_new$sn_locality), "_", tail(substr(dat_new$t_date, 1, 4), 1), ".txt")
    write.table(dat_final, paste(out_dir, file_name, sep = "/"), row.names = FALSE, sep = ";")
  }
  
  return(dat_final)
  
}
