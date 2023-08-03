##load packages##
source("packages.R")

#load all the data
unfilt_data <- read.csv("unfilt_data.csv")

#finish making dictionary 
df <- data.frame(unfilt_data$p_site_province_id,unfilt_data$p_site_province_name) %>% distinct()

#create province names in English
df$p_site_eng <- c("Tak",
                   "Mae Hong Son",
                   "Ranong",
                   "Surat Thani",
                   "Si Sa Ket",
                   "Prachuap Khiri Khan"
                   ,"Kanchanaburi"
                   ,"Chon Buri"
                   ,"Chanthaburi"
                   ,"Surin"
                   ,"Ubon Ratchathani"
                   ,"Phuket"
                   ,"Ratchaburi"
                   , "Yala"
                   , "Krabi"
                   , "Phrae"
                   , "Narathiwat"
                   ,"Chiang Mai"
                   , "Buri Ram"
                   ,"Trat"
                   ,"Phangnga"
                   ,"Kalasin"
                   , "Sa Kaeo"
                   , "Uthai Thani"
                   ,"Trang"
                   , "Chumphon"
                   , "Bangkok Metropolis"
                   ,"Songkhla"
                   , "Amnat Charoen"
                   , "Rayong"
                   ,"Pattani"
                   , "Maha Sarakham"
                   , "Pathum Thani"
                   , "Mukdahan"
                   , "Kamphaeng Phet"
                   , "Phetchaburi"
                   , "Lop Buri"
                   , "Lamphun"
                   , "Udon Thani"
                   , "Suphan Buri"
                   , "Samut Sakhon"
                   , "Phatthalung"
                   , "Chachoengsao"
                   , "Samut Prakan"
                   , "Phetchabun"
                   , "Lampang"
                   , "Chaiyaphum"
                   , "Chiang Rai"
                   , "Nakhon Pathom"
                   , "Sukhothai"
                   , "Yasothon"
                   , "Nakhon Sawan"
                   , "Phichit"
                   , "Saraburi"
                   , "Uttaradit"
                   , "Phitsanulok"
                   , "Nonthaburi"
                   , "Sakon Nakhon"
                   , "Samut Songkhram"
                   , "Prachin Buri"
                   , "Nan"
                   ,"Nakhon Si Thammarat"
                   ,"Nakhon Nayok"
                   ,"Nakhon Ratchasima"
                   , "Roi Et"
                   , "Nakhon Phanom"
                   , "Nong Khai"
                   , "Satun"
                   , "Loei"
                   , "Khon Kaen"
                   , "Phayao"
                   , "Ang Thong"
                   , "Nong Bua Lam Phu"
                   , "Phra Nakhon Si Ayutthaya"
                   , "Sing Buri"
                   ,"Bueng Kan"
                   ,"Chai Nat"
)


#filter for variables for analysis
final_df <- as_tibble(unfilt_data) %>% dplyr::select(blood_draw_date,p_site_province_id, org_type,result_code_detail_1,type_border,type_patient, p_site_district_name, p_site_subdistrict_name, p_site_mu_name, p_site_village_name,ep1_ep3_case_classification)

#convert to data frame and convert dates to numeric
final_df <- data.frame(final_df)
final_df$blood_draw_date <- as.numeric(final_df$blood_draw_date)

final_df$p_site_province_name <- df$p_site_eng[match(final_df$p_site_province_id, df$unfilt_data.p_site_province_id)]

total_data <- final_df

#format the dates
total_data$blood_draw_date <- as.Date(paste0(as.character(total_data$blood_draw_date), '01'), format='%Y%m%d')

#extract all province names
prov_name <- unique(total_data$p_site_province_name)

#load the thai map
thaimap <- read_rds("gadm36_THA_1_sp.rds")
 
#rename the borders based on definition  
total_data$type_border[total_data$type_border == "1"] <- 'Not Border'
total_data$type_border[total_data$type_border == "2"] <- 'Burma Border'
total_data$type_border[total_data$type_border == "3"] <- 'Malaysia Border'
total_data$type_border[total_data$type_border == "4"] <- 'Cambodia Border'
total_data$type_border[total_data$type_border == "5"] <- 'Laos Border'
 
#importing the mean temp across thailand 
temp_thai <- as.data.frame(read.csv("tas_timeseries_monthly_cru_1901-2021_THA.csv", header = FALSE))
#set column names 
colnames(temp_thai) <- temp_thai[3,]

#assign the month number to character values
extend_df <- data.frame(year = c(rep(2022,12)), month = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#remove the rows and format the columns 
temp_thai <- temp_thai[-1:-3,]
colnames(temp_thai)[1] <- 'year'

#format the temperature and precipitation data 
temp_thai$year <- factor(temp_thai$year)
temp_data_long <- gather(temp_thai, month, measurement, Jan:Dec, factor_key=TRUE)

temp_data_long <- temp_data_long[order(temp_data_long$year), ]
temp_data_long <- plyr::rbind.fill(temp_data_long,extend_df)

#create a model that relates temperature to fill in missing data. The purpose of the next few lines is to reformat the data
#so we can interpolate the missing values. The data format for the temperature is one read per month so we need to transform the data into daily reads.
model <- lm(temp_data_long$measurement ~ temp_data_long$month)
#predict values using the model previously built to fill missing values and interpolate missing values
prediction <- predict(model, newdata = temp_data_long)
start <- nrow(temp_data_long) - 12
temp_data_long$measurement[start:nrow(temp_data_long)] <- prediction[start:nrow(temp_data_long)]

temp_data_long$measurement <- as.numeric(temp_data_long$measurement)

temp_data_long$month <- match(temp_data_long$month,month.abb)

temp_data_long$year <- as.numeric(as.character(temp_data_long$year))

temp_data_long <- within(temp_data_long, blood_draw_date <- sprintf("%d-%02d", year, month))

temp_data_long$blood_draw_date <- as.Date(paste(temp_data_long$blood_draw_date, "-01", sep=""))

#approximate daily temperature values
approx_daily <- approx(x=temp_data_long$blood_draw_date,y=temp_data_long$measurement, xout=seq(as.Date("2012-01-01"), as.Date("2022-05-25"), "days"))

approx_daily_temp <- data.frame(approx_daily$x,approx_daily$y)

colnames(approx_daily_temp) <- c("blood_draw_date","measurement")

#importing the precipitation data 
precip_thai <- as.data.frame(read.csv("pr_timeseries_monthly_cru_1901-2021_THA.csv", header = FALSE))

#set column names 
colnames(precip_thai) <- precip_thai[3,]

#remove the rows
precip_thai <- precip_thai[-1:-3,]
colnames(precip_thai)[1] <- 'year'
precip_thai$year <- factor(precip_thai$year)

#similar to the temperature data set we need to format the data and then interpolate to find daily 
#precipitation reads
precip_data_long <- gather(precip_thai, month, measurement, Jan:Dec, factor_key=TRUE)

precip_data_long <- precip_data_long[order(precip_data_long$year), ]
precip_data_long <- plyr::rbind.fill(precip_data_long,extend_df)

#create model to relate precipitation to the month and then use this model to interpolate between
#values
model_precip <- lm(precip_data_long$measurement ~ precip_data_long$month)
prediction_precip <- predict(model_precip, newdata = precip_data_long)

start <- nrow(precip_data_long) - 12
precip_data_long$measurement[start:nrow(precip_data_long)] <- prediction[start:nrow(precip_data_long)]
precip_data_long$measurement <- as.numeric(precip_data_long$measurement)

#match the months from the precipitation data to the bigger data frame and format some other date variables
precip_data_long$month <- match(precip_data_long$month,month.abb)

precip_data_long$year <- as.numeric(as.character(precip_data_long$year))
precip_data_long <- within(precip_data_long, blood_draw_date <- sprintf("%d-%02d", year, month))
precip_data_long$blood_draw_date <- as.Date(paste(precip_data_long$blood_draw_date, "-01", sep=""))

#approximate the precipitation date
approx_daily_precip <- approx(x=precip_data_long$blood_draw_date,y=precip_data_long$measurement, xout=seq(as.Date("2012-01-01"), as.Date("2022-05-25"), "days"))
approx_daily_precip <- data.frame(approx_daily_precip$x,approx_daily_precip$y)

#rename the columns
colnames(approx_daily_precip) <- c("blood_draw_date","measurement")

 
 
 
 
 
 
 
 
 
 
 
 
 
 