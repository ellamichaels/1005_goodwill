#### Mock script for Ella

# Setup

library(tidyverse)
library(rvest)
library(RCurl)
library(ggmap)

# I have my google maps api stored in my environment (Which you can do using the
# usethis package. That's best practice- otherwise you could just type the key
# directly here)

register_google(key = Sys.getenv("ggmap_key")) 

get_loc <- function(store_id){
  store_url <- paste("https://www.goodwill.org/location/?store=", store_id, sep = "")
  store <- read_html(store_url)
  text <- store %>% html_nodes(".results")
  text <- text[[1]] %>% html_text
  x <- str_split_fixed(text, "Details", 2)[2] # Get the second half of what comes after details
  x <- str_split_fixed(x,"United",2)[[1]] # Stop at "United"
  x <- str_replace(x,pattern ="\\r\\n", "") # Get rid of other unnecessary characters
  x <- str_split_fixed(x, "(?=[0-9])",2)[2]
  location <- x
  return(location)}

store_ids <- seq(1,100, 1)

location_texts <- map(store_ids_1_500, get_loc)
location_c <- as.character(location_texts)
out_500 <-  geocode(location_c, output = "latlona")
write_csv(out_500,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella1.csv")


location_texts_500_1000 <- map(seq(501,1000,1), get_loc)
location_100_c <- as.character(location_texts_500_1000)
out_1000 <-  geocode(location_100_c, output = "latlona")
write_csv(out_1000,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella2.csv")


location_texts_1000_1500 <- map(store_ids[1000:1500], get_loc)

location_texts_1500_2000 <- map(store_ids[1500:2000], get_loc)
location_1500_2000_c <- as.character(location_texts_1500_2000)
out_1500 <-  geocode(location_1500_2000_c, output = "latlona")
write_csv(out_1500,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella3.csv")

location_texts_2000_2500 <- map(store_ids[2000:2500], get_loc)
location_2000_c <- as.character(location_texts_2000_2500)
out_2000 <-  geocode(location_2000_c, output = "latlona")
write_csv(out_2000,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella4.csv")


location_texts_2500_3000 <- map(store_ids[2500:3000], get_loc)
location_texts_2500_3000_c <- as.character(location_texts_2500_3000)
out_3000 <-  geocode(location_texts_2500_3000_c, output = "latlona")
write_csv(out_3000,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella5.csv")


location_texts_3000_3500 <- map(store_ids[3000:3500], get_loc)
location_texts_3000_3500_c <- as.character(location_texts_3000_3500)
out_3500 <-  geocode(location_texts_3000_3500_c, output = "latlona")
write_csv(out_3500,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella6.csv")

location_texts_3500_4000 <- map(store_ids[3500:4000], get_loc)
location_texts_3500_4000_c <- as.character(location_texts_3500_4000 )
out_4000 <-  geocode(location_texts_3500_4000_c, output = "latlona")
write_csv(out_4000,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella7.csv")



location_texts_4000_4500 <- map(store_ids[4000:4950], get_loc)
location_texts_4000_4500_c <- as.character(location_texts_4000_4500 )
out_4950 <-  geocode(location_texts_4000_4500_c, output = "latlona")
write_csv(out_4950,"/Users/alyssahuberts/Dropbox/1_Harvard_Teaching/3_Gov1005/student_help/ella8.csv")








stores <- as.data.frame(geomapped_stores)
stores <- str_split_fixed(string = x$geomapped_stores,pattern =",", n =3)