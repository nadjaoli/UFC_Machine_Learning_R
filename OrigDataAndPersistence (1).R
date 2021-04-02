# General variables 

timestep <- 5 # in minutes

# The latitude and longitude of the spot
{
  lat = -9.0689 #Petrolina, PE, Brazil
  lon = -40.3197
}

# Load the original data
df <- read.csv(file = "petrolina_2013_2014_2015_2016.csv", sep = ",", na.strings = "N/A")

df$time = (df$year-min(df$year))*365*24*60*60 + (df$day-1)*24*60*60 + df$min*60 
df$time = as.POSIXlt(df$time, origin = "2013-01-01 00:00:00", tz="UTC")
df$time = as.POSIXct(df$time)

# Solar zenith angle
# https://www.rdocumentation.org/packages/GeoLight/versions/2.0.0/topics/zenith
sun <- solar(df$time)
df$zenith = zenith(sun,lon,lat)
rm(sun)

sum(is.na(df)) # Counts NAs
str(df)

# Subsets and organizes
df <- df[,c(17,2,3,4,18,5,6,11)] # Retained the temperature for future use (if necessary)

names(df)[names(df) == "glo_avg"] <- "GHI"
names(df)[names(df) == "dir_avg"] <- "DNI"
names(df)[names(df) == "tp_sfc"] <- "Temp"

df$GHI <- as.numeric(df$GHI)
df$DNI <- as.numeric(df$DNI)
df$Temp <- as.numeric(df$Temp)

# WARNING!!! - Dramatically decreases the df size - only to test the models - MUST BE COMMENTED othewise
df <- df[which(df$day<=7),]

# Takes the moving average
df$GHI_avg <- frollmean(df$GHI,timestep)
df$DNI_avg <- frollmean(df$DNI,timestep)
df$Temp_avg <- frollmean(df$Temp,timestep)

df$GHI <- NULL
df$DNI <- NULL
df$Temp <- NULL

df <- df[which(df$min%%timestep == 0),] # Takes the periodic values for the timestep

# Create the time lags for GHI
df <- df %>%
  arrange(time) %>%
  mutate(Lag1GHI_avg = lag(GHI_avg, default = first(GHI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag2GHI_avg = lag(Lag1GHI_avg, default = first(Lag1GHI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag3GHI_avg = lag(Lag2GHI_avg, default = first(Lag2GHI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag4GHI_avg = lag(Lag3GHI_avg, default = first(Lag3GHI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag5GHI_avg = lag(Lag4GHI_avg, default = first(Lag4GHI_avg)))

# Create the time lags for DNI
df <- df %>%
  arrange(time) %>%
  mutate(Lag1DNI_avg = lag(DNI_avg, default = first(DNI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag2DNI_avg = lag(Lag1DNI_avg, default = first(Lag1DNI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag3DNI_avg = lag(Lag2DNI_avg, default = first(Lag2DNI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag4DNI_avg = lag(Lag3DNI_avg, default = first(Lag3DNI_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag5DNI_avg = lag(Lag4DNI_avg, default = first(Lag4DNI_avg)))

# Create the time lags for Temp
df <- df %>%
  arrange(time) %>%
  mutate(Lag1Temp_avg = lag(Temp_avg, default = first(Temp_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag2Temp_avg = lag(Lag1Temp_avg, default = first(Lag1Temp_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag3Temp_avg = lag(Lag2Temp_avg, default = first(Lag2Temp_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag4Temp_avg = lag(Lag3Temp_avg, default = first(Lag3Temp_avg)))

df <- df %>%
  arrange(time) %>%
  mutate(Lag5Temp_avg = lag(Lag4Temp_avg, default = first(Lag4Temp_avg)))

# To mark contiguous data (timely)
df <- df %>%
  arrange(time) %>%
  mutate(timelag = time - lag(time, default = first(time)))

# To get only contiguous data (timely)
df <- df[which(df$timelag == (timestep*60)),]

# Removes the high zenith data (maintains the clear sky data)
df[] <- lapply(df, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
df = subset(df, df$zenith <= 85)

# Compute the average prediction error metrics (for the TEST SET ONLY)
RMSE.Persist.GHI = RMSE(df$GHI_avg[which(df$year==2016)], df$Lag1GHI_avg[which(df$year==2016)], na.rm = T)
MAE.Persist.GHI = MAE(df$GHI_avg[which(df$year==2016)], df$Lag1GHI_avg[which(df$year==2016)], na.rm = T)
Rsquared.Persist.GHI = R2(df$GHI_avg[which(df$year==2016)], df$Lag1GHI_avg[which(df$year==2016)], na.rm = T)
MBE.Persist.GHI = mean((df$GHI_avg[which(df$year==2016)]-df$Lag1GHI_avg[which(df$year==2016)]), na.rm = T)

RMSE.Persist.DNI = RMSE(df$DNI_avg[which(df$year==2016)], df$Lag1DNI_avg[which(df$year==2016)], na.rm = T)
MAE.Persist.DNI = MAE(df$DNI_avg[which(df$year==2016)], df$Lag1DNI_avg[which(df$year==2016)], na.rm = T)
Rsquared.Persist.DNI = R2(df$DNI_avg[which(df$year==2016)], df$Lag1DNI_avg[which(df$year==2016)], na.rm = T)
MBE.Persist.DNI = mean((df$DNI_avg[which(df$year==2016)]-df$Lag1DNI_avg[which(df$year==2016)]), na.rm = T)

RMSE.Persist.Temp = RMSE(df$Temp_avg[which(df$year==2016)], df$Lag1Temp_avg[which(df$year==2016)], na.rm = T)
MAE.Persist.Temp = MAE(df$Temp_avg[which(df$year==2016)], df$Lag1Temp_avg[which(df$year==2016)], na.rm = T)
Rsquared.Persist.Temp = R2(df$Temp_avg[which(df$year==2016)], df$Lag1Temp_avg[which(df$year==2016)], na.rm = T)
MBE.Persist.Temp = mean((df$Temp_avg[which(df$year==2016)]-df$Lag1Temp_avg[which(df$year==2016)]), na.rm = T)

df = data.table (df, key = "time")
#summary(df)

# Plots of Persistence

data = cbind(df$GHI_avg[which(df$year==2016)], df$Lag1GHI_avg[which(df$year==2016)])
colnames(data) = c("GHI_Test", "GHI_Pred")
data = as.data.frame(data)
pPersistGHI <- ggplot(data, aes(x=GHI_Test, y=GHI_Pred)) +
  geom_point(shape=21, color="black", fill="#634f4d", size=3, alpha=0.45) +
  geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Modeling \n Persistence") +
  #xlim(0,120) +
  #ylim(0,120) +
  labs(y= "Predicted GHI", x = "Measured GHI", color = "")

# with marginal histogram
pPersistGHI <- ggMarginal(pPersistGHI, type="histogram", bins=40, fill="#634f4d")

# Time series plot
data = cbind(df$time[which(df$year==2016)],
             df$GHI_avg[which(df$year==2016)],
             df$Lag1GHI_avg[which(df$year==2016)],
             df$Lag1GHI_avg[which(df$year==2016)] - df$GHI_avg[which(df$year==2016)])
colnames(data) = c("Time", "GHI_Meas", "GHI_Pred", "GHI_Error")
data <- as.data.frame(data)
data$Time <- as.POSIXct(data$Time, origin = "1970-01-01 00:00:00", tz="UTC")

tPersistGHI1 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.5) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#634f4d", size=0.5) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  #ggtitle("GHI over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tPersistGHIError1 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.5) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#634f4d", size=0.5) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Error over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tPersistGHIFinal1 <- tPersistGHI1 + tPersistGHIError1 +
                     plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

tPersistGHI2 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.5) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#634f4d", size=0.5) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI over Time \n during the first two days of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tPersistGHIError2 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.5) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#634f4d", size=0.5) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI Error over Time \n during the the first two days of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tPersistGHIFinal2 <- tPersistGHI2 + tPersistGHIError2 +
                     plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

rm(data)

# Now do the same plots for DNI...