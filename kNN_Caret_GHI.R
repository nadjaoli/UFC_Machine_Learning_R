
df_TEMP  <- na.omit(df) # Caret doesn't accept NAs

# Split the data into training and test set
train.data <- df_TEMP[which(df_TEMP$year !=2016),]
test.data <- df_TEMP[which(df_TEMP$year ==2016),]

train.data <- train.data[,c(3:6,9:13)] # Reorganize columns
test.data <- test.data[,c(3:6,9:13)]

# Fit the model on the training set
set.seed(2020)
my.grid = expand.grid(k = 1:50)
modelkNN.GHI <- train(
  GHI_avg ~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 10),
  #preProc = c("center", "scale"),
  tuneGrid = my.grid
)

plot(modelkNN.GHI)

# Best tuning parameter mtry
modelkNN.GHI$bestTune

# Make predictions on the test data
predictionskNN.GHI <- modelkNN.GHI %>% predict(test.data)
head(predictionskNN.GHI)

# Compute the average prediction error RMSE and R2
RMSE.kNN.GHI = RMSE(predictionskNN.GHI, test.data$GHI_avg)
Rsquared.kNN.GHI = R2(predictionskNN.GHI, test.data$GHI_avg)
MAE.kNN.GHI = MAE(predictionskNN.GHI, test.data$GHI_avg)
MBE.kNN.GHI = mean((predictionskNN.GHI - test.data$GHI_avg), na.rm = T)
FS.kNN.GHI = 1 - RMSE.kNN.GHI/RMSE.Persist.GHI

# Plots of kNN

data = cbind(test.data$GHI_avg, predictionskNN.GHI)
colnames(data) = c("GHI_Test", "GHI_Pred")
data = as.data.frame(data)
pkNN.GHI <- ggplot(data, aes(x=GHI_Test, y=GHI_Pred)) +
  geom_point(shape=21, color="black", fill="#fff700", size=3, alpha=0.45) +
  geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Modeling \n kNN") +
  #xlim(0,120) +
  #ylim(0,120) +
  labs(y= "Predicted GHI", x = "Measured GHI", color = "")

# with marginal histogram
pkNN.GHI <- ggMarginal(pkNN.GHI, type="histogram", bins=40, fill="#fff700")

# Plot of the model training 
pModelkNN.GHI <- plot(modelkNN.GHI)

# Plot of the variable importance
vkNN.GHI <- varImp(modelkNN.GHI)$importance
vkNN.GHI <- cbind(row.names(vkNN.GHI), vkNN.GHI$Overall)
colnames(vkNN.GHI) <- c("Variable", "Importance")
vkNN.GHI <- as.data.frame(vkNN.GHI)

pVarImpkNN.GHI <- ggplot(vkNN.GHI, aes(x=vkNN.GHI$Variable, y=as.numeric(as.character(vkNN.GHI$Importance)))) +
  geom_segment( aes(x=vkNN.GHI$Variable, xend=vkNN.GHI$Variable, y=0, yend=as.numeric(as.character(vkNN.GHI$Importance))), color="skyblue", size=1.5) +
  geom_point( color="blue", size=3, alpha=1.0) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_light() +
  coord_flip() +
  scale_x_discrete(limits=rev(vkNN.GHI$Variable)) +
  theme(plot.title = element_text(hjust = 0.6, size=rel(0.6)),
        axis.text=element_text(size=rel(0.8)),
        axis.title=element_text(size=rel(1.0))) +
  xlab("Variable") +
  ylab("Importance")

# https://topepo.github.io/caret/model-training-and-tuning.html
trellis.par.set(caretTheme())
pDenskNN.GHI <- densityplot(modelkNN.GHI, pch = "|")

# Time series plot
data = cbind(df_TEMP[which(df_TEMP$year ==2016),]$time,
             test.data$GHI_avg,
             predictionskNN.GHI,
             predictionskNN.GHI - test.data$GHI_avg)
colnames(data) = c("Time", "GHI_Meas", "GHI_Pred", "GHI_Error")
data <- as.data.frame(data)
data$Time <- as.POSIXct(data$Time, origin = "1970-01-01 00:00:00", tz="UTC")

tkNNGHI1 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#fff700", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  #ggtitle("GHI over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tkNNGHIError1 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#fff700", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Error over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tkNNGHIFinal1 <- tkNNGHI1 + tkNNGHIError1 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

tkNNGHI2 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#fff700", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI over Time \n during the first two days of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tkNNGHIError2 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#fff700", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI Error over Time \n during the the first two days of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tkNNGHIFinal2 <- tkNNGHI2 + tkNNGHIError2 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

rm(data)
