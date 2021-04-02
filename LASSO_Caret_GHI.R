
df_TEMP  <- na.omit(df) # Caret doesn't accept NAs

# Split the data into training and test set
train.data <- df_TEMP[which(df_TEMP$year !=2016),]
test.data <- df_TEMP[which(df_TEMP$year ==2016),]

train.data <- train.data[,c(3:6,9:13)] # Reorganize columns
test.data <- test.data[,c(3:6,9:13)]

# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/153-penalized-regression-essentials-ridge-lasso-elastic-net/#using-caret-package
# Fit the model on the training set
set.seed(2020)
lambda <- 10^seq(-7, 3, length = 200)
my.grid = expand.grid(alpha = 1, lambda = lambda) #alpha = 0 for Ridge, comment for Elastic Net
modelLASSO.GHI <- train(
    GHI_avg ~., data = train.data, method = "glmnet",
    trControl = trainControl("cv", number = 10),
    #preProc = c("center", "scale"),
    tuneGrid = my.grid # Change by "tuneLength = 10" for Elastic Net
)

# Model coefficients
coef(modelLASSO.GHI$finalModel, modelLASSO.GHI$bestTune$lambda)

plot(modelLASSO.GHI)

# Best tuning parameter mtry
modelLASSO.GHI$bestTune

# Make predictions on the test data
predictionsLASSO.GHI <- modelLASSO.GHI %>% predict(test.data)
head(predictionsLASSO.GHI)

# Compute the average prediction error RMSE and R2
RMSE.LASSO.GHI = RMSE(predictionsLASSO.GHI, test.data$GHI_avg)
Rsquared.LASSO.GHI = R2(predictionsLASSO.GHI, test.data$GHI_avg)
MAE.LASSO.GHI = MAE(predictionsLASSO.GHI, test.data$GHI_avg)
MBE.LASSO.GHI = mean((predictionsLASSO.GHI - test.data$GHI_avg), na.rm = T)
FS.LASSO.GHI = 1 - RMSE.LASSO.GHI/RMSE.Persist.GHI

# Plots of LASSO

data = cbind(test.data$GHI_avg, predictionsLASSO.GHI)
colnames(data) = c("GHI_Test", "GHI_Pred")
data = as.data.frame(data)
pLASSO.GHI <- ggplot(data, aes(x=GHI_Test, y=GHI_Pred)) +
  geom_point(shape=21, color="black", fill="#00e8ff", size=3, alpha=0.45) +
  geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Modeling \n LASSO") +
  #xlim(0,120) +
  #ylim(0,120) +
  labs(y= "Predicted GHI", x = "Measured GHI", color = "")

# with marginal histogram
pLASSO.GHI <- ggMarginal(pLASSO.GHI, type="histogram", bins=40, fill="#00e8ff")

# Plot of the model training 
pModelLASSO.GHI <- plot(modelLASSO.GHI)

# Plot of the variable importance
vLASSO.GHI <- varImp(modelLASSO.GHI)$importance
vLASSO.GHI <- cbind(row.names(vLASSO.GHI), vLASSO.GHI$Overall)
colnames(vLASSO.GHI) <- c("Variable", "Importance")
vLASSO.GHI <- as.data.frame(vLASSO.GHI)

pVarImpLASSO.GHI <- ggplot(vLASSO.GHI, aes(x=vLASSO.GHI$Variable, y=as.numeric(as.character(vLASSO.GHI$Importance)))) +
  geom_segment( aes(x=vLASSO.GHI$Variable, xend=vLASSO.GHI$Variable, y=0, yend=as.numeric(as.character(vLASSO.GHI$Importance))), color="skyblue", size=1.5) +
  geom_point( color="blue", size=3, alpha=1.0) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_light() +
  coord_flip() +
  scale_x_discrete(limits=rev(vLASSO.GHI$Variable)) +
  theme(plot.title = element_text(hjust = 0.6, size=rel(0.6)),
        axis.text=element_text(size=rel(0.8)),
        axis.title=element_text(size=rel(1.0))) +
  xlab("Variable") +
  ylab("Importance")

# https://topepo.github.io/caret/model-training-and-tuning.html
trellis.par.set(caretTheme())
pDensLASSO.GHI <- densityplot(modelLASSO.GHI, pch = "|")

# Time series plot
data <- cbind(df_TEMP[which(df_TEMP$year ==2016),]$time,
             test.data$GHI_avg,
             predictionsLASSO.GHI,
             predictionsLASSO.GHI - test.data$GHI_avg)
colnames(data) = c("Time", "GHI_Meas", "GHI_Pred", "GHI_Error")
data <- as.data.frame(data)
data$Time <- as.POSIXct(data$Time, origin = "1970-01-01 00:00:00", tz="UTC")

tLASSOGHI1 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#00e8ff", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  #ggtitle("GHI over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tLASSOGHIError1 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#00e8ff", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Error over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tLASSOGHIFinal1 <- tLASSOGHI1 + tLASSOGHIError1 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

tLASSOGHI2 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#00e8ff", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI over Time \n during the first two days of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tLASSOGHIError2 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#00e8ff", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI Error over Time \n during the the first two days of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tLASSOGHIFinal2 <- tLASSOGHI2 + tLASSOGHIError2 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

rm(data)
