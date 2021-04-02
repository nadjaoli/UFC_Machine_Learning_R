
df_TEMP  <- na.omit(df) # Caret doesn't accept NAs

# Split the data into training and test set
train.data <- df_TEMP[which(df_TEMP$year !=2016),]
test.data <- df_TEMP[which(df_TEMP$year ==2016),]

train.data <- train.data[,c(3:6,9:13)] # Reorganize columns
test.data <- test.data[,c(3:6,9:13)]

# Fit the model on the training set
set.seed(2020)
grid_default <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.1, 0.2),
  max_depth = c(9, 10),
  gamma = c(0.15, 0.20),
  colsample_bytree = c(0.8, 1.0),
  min_child_weight = c(3, 4),
  subsample = c(0.75, 1.0)
  #eta = c(0.025, 0.05, 0.1, 0.3, 0.4),
  #max_depth = c(2, 3, 4),
  #gamma = c(0, 0.05),
  #colsample_bytree = c(0.4, 0.6, 0.8),
  #min_child_weight = c(1, 2, 3),
  #subsample = c(0.5, 0.75, 1.0)
)
modelXGBoost.GHI <- train(
  GHI_avg ~., data = train.data, method = "xgbTree",
  #preProc = c("center", "scale"),
  tuneGrid = grid_default,
  trControl = trainControl("cv", number = 10)
)

plot(modelXGBoost.GHI)

# Best tuning parameter mtry
modelXGBoost.GHI$bestTune

# Make predictions on the test data
predictionsXGBoost.GHI <- modelXGBoost.GHI %>% predict(test.data)
head(predictionsXGBoost.GHI)

# Compute the average prediction error RMSE and R2
RMSE.XGBoost.GHI = RMSE(predictionsXGBoost.GHI, test.data$GHI_avg)
Rsquared.XGBoost.GHI = R2(predictionsXGBoost.GHI, test.data$GHI_avg)
MAE.XGBoost.GHI = MAE(predictionsXGBoost.GHI, test.data$GHI_avg)
MBE.XGBoost.GHI = mean((predictionsXGBoost.GHI - test.data$GHI_avg), na.rm = T)
FS.XGBoost.GHI = 1 - RMSE.XGBoost.GHI/RMSE.Persist.GHI

# Plots of XGBoost

data = cbind(test.data$GHI_avg, predictionsXGBoost.GHI)
colnames(data) = c("GHI_Test", "GHI_Pred")
data = as.data.frame(data)
pXGBoost.GHI <- ggplot(data, aes(x=GHI_Test, y=GHI_Pred)) +
  geom_point(shape=21, color="black", fill="#8f1d21", size=3, alpha=0.45) +
  geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Modeling \n XGBoost") +
  #xlim(0,120) +
  #ylim(0,120) +
  labs(y= "Predicted GHI", x = "Measured GHI", color = "")

# with marginal histogram
pXGBoost.GHI <- ggMarginal(pXGBoost.GHI, type="histogram", bins=40, fill="#8f1d21")

# Plot of the model training 
pModelXGBoost.GHI <- plot(modelXGBoost.GHI)

# Plot of the variable importance
vXGBoost.GHI <- varImp(modelXGBoost.GHI)$importance
vXGBoost.GHI <- cbind(row.names(vXGBoost.GHI), vXGBoost.GHI$Overall)
colnames(vXGBoost.GHI) <- c("Variable", "Importance")
vXGBoost.GHI <- as.data.frame(vXGBoost.GHI)

pVarImpXGBoost.GHI <- ggplot(vXGBoost.GHI, aes(x=vXGBoost.GHI$Variable, y=as.numeric(as.character(vXGBoost.GHI$Importance)))) +
  geom_segment( aes(x=vXGBoost.GHI$Variable, xend=vXGBoost.GHI$Variable, y=0, yend=as.numeric(as.character(vXGBoost.GHI$Importance))), color="skyblue", size=1.5) +
  geom_point( color="blue", size=3, alpha=1.0) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_light() +
  coord_flip() +
  scale_x_discrete(limits=rev(vXGBoost.GHI$Variable)) +
  theme(plot.title = element_text(hjust = 0.6, size=rel(0.6)),
        axis.text=element_text(size=rel(0.8)),
        axis.title=element_text(size=rel(1.0))) +
  xlab("Variable") +
  ylab("Importance")

# https://topepo.github.io/caret/model-training-and-tuning.html
trellis.par.set(caretTheme())
pDensXGBoost.GHI <- densityplot(modelXGBoost.GHI, pch = "|")

# Time series plot
data = cbind(df_TEMP[which(df_TEMP$year ==2016),]$time,
             test.data$GHI_avg,
             predictionsXGBoost.GHI,
             predictionsXGBoost.GHI - test.data$GHI_avg)
colnames(data) = c("Time", "GHI_Meas", "GHI_Pred", "GHI_Error")
data <- as.data.frame(data)
data$Time <- as.POSIXct(data$Time, origin = "1970-01-01 00:00:00", tz="UTC")

tXGBoostGHI1 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#8f1d21", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  #ggtitle("GHI over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tXGBoostGHIError1 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#8f1d21", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  #ggtitle("GHI Error over Time \n during the first week of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tXGBoostGHIFinal1 <- tXGBoostGHI1 + tXGBoostGHIError1 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

tXGBoostGHI2 <- ggplot(data, aes(x=Time, y=GHI_Meas)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Pred), color="#8f1d21", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI over Time \n during the first two days of the test set") +
  #xlim(0,120) +
  ylim(0,1200) +
  labs(y= "GHI (W/m2)", x = NULL, color = "")

tXGBoostGHIError2 <- ggplot(data, aes(x=Time, y=GHI_Error)) +
  geom_line(color="black", size=0.7) +
  geom_line(data=data, aes(x=Time, y=GHI_Error), color="#8f1d21", size=0.7) +
  #geom_smooth(method=lm , color="red", fill="black", se=TRUE) +
  theme_ipsum() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(limits = ymd_h(c("2016-01-01 08", "2016-01-02 20"))) +
  #ggtitle("GHI Error over Time \n during the the first two days of the test set") +
  #xlim(0,120) +
  ylim(-1200,1200) +
  labs(y= "GHI Error (W/m2)", x = "Date-Time (UTC)", color = "")

tXGBoostGHIFinal2 <- tXGBoostGHI2 + tXGBoostGHIError2 +
  plot_layout(ncol = 1, heights = c(4, 1)) # Using the 'patchwork' package

rm(data)
