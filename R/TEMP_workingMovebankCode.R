## WORKING movebank Code!
Require::Require("move2")
loginStored <- move2::movebank_store_credentials(username=Sys.getenv("MOVEBANK_USERNAME"), 
                             password=Sys.getenv("MOVEBANK_PASSWORD"))

a <- move2::movebank_download_study(study_id = 384182382) # potentially add , removeDuplicatedTimestamps=TRUE
  # study = paste0( 'GNWT ', "Dehcho Boreal Woodland Caribou"),
  #                           login = loginStored, removeDuplicatedTimestamps=TRUE)
a <- data.table(a)
