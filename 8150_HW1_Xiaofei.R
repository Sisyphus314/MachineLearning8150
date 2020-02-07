# Q2
# a)
fat <- read.csv("/Users/xiaofeixue/Desktop/8150/fat.csv", header = TRUE)

train <- fat[c(1:200), ]
test_df <- fat[c(201:252), ]


fat_lr <- lm(brozek ~ siri+
               density+
               age+
               weight+
               height+
               adipos+
               free+
               neck+
               chest+
               abdom+
               hip+
               thigh+
               knee+
               ankle+
               biceps+
               forearm+wrist, data=train)
summary(fat_lr)
# R^2 = 0.9994. I think this model fits the data pretty good since R^2 is very
# close to 1.

# b)
# alpha = 0.05
# Problematic features are:
# age,
# height,
# adipos,
# neck,
# chest,
# abdom,
# hip,
# ankle,
# biceps,
# forearm,
# wrist.

# c)
test_pred <- predict(fat_lr, test_df)
test_pred
compare_df <- data.frame(cbind(real=test_df$brozek, guess=test_pred))
compare_df
pred_error1 <- sqrt(sum((compare_df$real-compare_df$guess)^2))
pred_error1
# pred_error1 = 0.7537424

# d)
fat["density"]
fat_den <- fat
fat_den
fat_den["density"] <- lapply(fat_den["density"], function(x) 1/x)
fat_den["density"]
train_den <- fat_den[c(1:200), ]
train_den
fat_den_lr <- lm(brozek ~ siri+density+age+weight+height+adipos+
                   free+neck+chest+abdom+hip+thigh+knee+ankle+
                   biceps+forearm+wrist, data=train_den)
summary(fat_den_lr)
# R^2 is 0.9994 and it shows that the model is still good.

# e)
test_den <- fat_den[c(201:252), ]
den_pred <- predict(fat_den_lr, test_den)
compare_den <- data.frame(cbind(real=test_den$brozek, guess=den_pred))
compare_den
pred_error2 <- sqrt(sum((compare_den$real-compare_den$guess)^2))
pred_error2
# pred_error2 = 0.7542754

# f)
fat_sd <- fat[, c("brozek", "siri", "density")]
fat_sd["density_inv"] <- lapply(fat_sd["density"], function(x) 1/x)
fat_sd
fat_sd["siri_squ"] <- lapply(fat_sd["siri"], function(x) x^2)
fat_sd

train_sd <- fat_sd[c(1:200), ]
train_sd
test_sd <- fat_sd[c(201:252), ]
test_sd

fat_sd_lr<- lm(brozek ~ siri+density+siri_squ,
               data=train_sd)
summary(fat_sd_lr)
# R^2 is 0.9993. The model is still good.
# The problematic feature seems to be siri_squ since its p value is 0.9068,
# higher than alpha = 0.05.

# g) No features seems to be problematic.

# h)
sd_pred <- predict(fat_sd_lr, test_sd)
compare_sd <- data.frame(cbind(real=test_sd$brozek, guess=sd_pred))
compare_sd
pred_error3 <- sqrt(sum((compare_sd$real-compare_sd$guess)^2))
pred_error3
# The error for model 3 is 0.5639797.

# i)
# based on the error values, I will choose model 3 due to 2 reasons:
# 1. model 3 has the smallest predicted error.
# 2. model 3 has least features in the model. More features will add more
# variances and uncertainties into the model.

# Q3
# a)
fat_ext <- fat
fat_ext
fat_ext["siri_squ"] <- lapply(fat_ext["siri"], function(x) x^2)
fat_ext
fat_ext["density_inv"] <- lapply(fat_ext["density"], function(x) 1/x)
fat_ext

train_ext <- fat_ext[c(1:200), ]
train_ext
test_ext <- fat_ext[c(201:252), ]
test_ext

ext_lr <- lm(brozek ~ siri+density+age+weight+height+adipos+
                   free+neck+chest+abdom+hip+thigh+knee+ankle+
                   biceps+forearm+wrist+siri_squ+density_inv,
             data=train_ext)
summary(ext_lr)

# b)
ext_pred <- predict(ext_lr, test_ext)
compare_ext <- data.frame(cbind(real=test_ext$brozek, guess=ext_pred))
compare_ext
e_full <- sqrt(sum((compare_ext$real-compare_ext$guess)^2))
e_full
# error full is 0.8565467

# d)
# height      -4.702e-04  4.916e-03  -0.096  0.92391
# drop height
ext_1 <- fat_ext[,!names(fat_ext) %in% c("height")]
ext_1

train_ext_1 <- ext_1[c(1:200), ]
test_ext_1 <- ext_1[c(201:252), ]

ext_1_lr <- lm(brozek ~ siri+density+age+weight+adipos+
               free+neck+chest+abdom+hip+thigh+knee+ankle+
               biceps+forearm+wrist+siri_squ+density_inv,
             data=train_ext_1)
summary(ext_1_lr)
# abdom       -6.333e-04  5.367e-03  -0.118  0.90621
# drop abdom
ext_2 <- ext_1[,!names(ext_1) %in% c("abdom")]
ext_2

train_ext_2 <- ext_2[c(1:200), ]
test_ext_2 <- ext_2[c(201:252), ]
ext_2_lr <- lm(brozek ~ siri+density+age+weight+adipos+
                 free+neck+chest+hip+thigh+knee+ankle+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_2)
summary(ext_2_lr)

# hip          7.278e-04  7.271e-03   0.100  0.92039
# drop hip
ext_3 <- ext_2[,!names(ext_2) %in% c("hip")]
ext_3

train_ext_3 <- ext_3[c(1:200), ]
test_ext_3 <- ext_3[c(201:252), ]
ext_3_lr <- lm(brozek ~ siri+density+age+weight+adipos+
                 free+neck+chest+thigh+knee+ankle+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_3)
summary(ext_3_lr)

# neck        -2.200e-03  1.088e-02  -0.202  0.83993
# drop neck
ext_4 <- ext_3[,!names(ext_3) %in% c("neck")]
ext_4

train_ext_4 <- ext_4[c(1:200), ]
test_ext_4 <- ext_4[c(201:252), ]
ext_4_lr <- lm(brozek ~ siri+density+age+weight+adipos+
                 free+chest+thigh+knee+ankle+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_4)
summary(ext_4_lr)

# age         -7.400e-04  1.511e-03  -0.490  0.62495
# drop age
ext_5 <- ext_4[,!names(ext_4) %in% c("age")]
ext_5

train_ext_5 <- ext_5[c(1:200), ]
test_ext_5 <- ext_5[c(201:252), ]
ext_5_lr <- lm(brozek ~ siri+density+weight+adipos+
                 free+chest+thigh+knee+ankle+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_5)
summary(ext_5_lr)

# ankle        7.123e-03  9.774e-03   0.729  0.46708
# drop ankle
ext_6 <- ext_5[,!names(ext_5) %in% c("ankle")]
ext_6

train_ext_6 <- ext_6[c(1:200), ]
test_ext_6 <- ext_6[c(201:252), ]
ext_6_lr <- lm(brozek ~ siri+density+weight+adipos+
                 free+chest+thigh+knee+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_6)
summary(ext_6_lr)

# chest        4.902e-03  5.041e-03   0.972  0.33207
# drop check
ext_7 <- ext_6[,!names(ext_6) %in% c("chest")]
ext_7

train_ext_7 <- ext_7[c(1:200), ]
test_ext_7 <- ext_7[c(201:252), ]
ext_7_lr <- lm(brozek ~ siri+density+weight+adipos+
                 free+thigh+knee+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_7)
summary(ext_7_lr)

# adipos      -1.674e-02  1.088e-02  -1.538  0.12576
# drop adipos
ext_8 <- ext_7[,!names(ext_7) %in% c("adipos")]
ext_8

train_ext_8 <- ext_8[c(1:200), ]
test_ext_8 <- ext_8[c(201:252), ]
ext_8_lr <- lm(brozek ~ siri+density+weight+
                 free+thigh+knee+
                 biceps+forearm+wrist+siri_squ+density_inv,
               data=train_ext_8)
summary(ext_8_lr)

# wrist        2.611e-02  2.327e-02   1.122   0.2632
# drop wrist
ext_9 <- ext_8[,!names(ext_8) %in% c("wrist")]
ext_9

train_ext_9 <- ext_9[c(1:200), ]
test_ext_9 <- ext_9[c(201:252), ]
ext_9_lr <- lm(brozek ~ siri+density+weight+
                 free+thigh+knee+
                 biceps+forearm+siri_squ+density_inv,
               data=train_ext_9)
summary(ext_9_lr)

# density      4.635e+02  2.566e+02   1.807   0.0724
# drop density
ext_10 <- ext_9[,!names(ext_9) %in% c("density")]
ext_10

train_ext_10 <- ext_10[c(1:200), ]
test_ext_10 <- ext_10[c(201:252), ]
ext_10_lr <- lm(brozek ~ siri+weight+
                 free+thigh+knee+
                 biceps+forearm+siri_squ+density_inv,
               data=train_ext_10)
summary(ext_10_lr)

# siri_squ    -0.0004530  0.0002218  -2.042  0.04254
# drop siri_squ
ext_11 <- ext_10[,!names(ext_10) %in% c("siri_squ")]
ext_11

train_ext_11 <- ext_11[c(1:200), ]
test_ext_11 <- ext_11[c(201:252), ]
ext_11_lr <- lm(brozek ~ siri+weight+
                  free+thigh+knee+
                  biceps+forearm+density_inv,
                data=train_ext_11)
summary(ext_11_lr)

# free        -0.007353   0.005048  -1.457   0.1469
# drop free
ext_12 <- ext_11[,!names(ext_11) %in% c("free")]
ext_12

train_ext_12 <- ext_12[c(1:200), ]
test_ext_12 <- ext_12[c(201:252), ]
ext_12_lr <- lm(brozek ~ siri+weight+thigh+knee+
                  biceps+forearm+density_inv,
                data=train_ext_12)
summary(ext_12_lr)

# weight       0.001416   0.001208   1.172  0.24246
# drop weight
ext_13 <- ext_12[,!names(ext_12) %in% c("weight")]
ext_13

train_ext_13 <- ext_13[c(1:200), ]
test_ext_13 <- ext_13[c(201:252), ]
ext_13_lr <- lm(brozek ~ siri+thigh+knee+
                  biceps+forearm+density_inv,
                data=train_ext_13)
summary(ext_13_lr)

# biceps      -0.015399   0.007764  -1.984  0.04873 *
# drop biceps
ext_14 <- ext_13[,!names(ext_13) %in% c("biceps")]
ext_14

train_ext_14 <- ext_14[c(1:200), ]
test_ext_14 <- ext_14[c(201:252), ]
ext_14_lr <- lm(brozek ~ siri+thigh+knee+forearm+density_inv,
                data=train_ext_14)
summary(ext_14_lr)

# forearm      0.013048   0.008225   1.586   0.1143
# drop forearm
ext_15 <- ext_14[,!names(ext_14) %in% c("forearm")]
ext_15

train_ext_15 <- ext_15[c(1:200), ]
test_ext_15 <- ext_15[c(201:252), ]
ext_15_lr <- lm(brozek ~ siri+thigh+knee+density_inv,
                data=train_ext_15)
summary(ext_15_lr)

# density_inv  9.436457   4.697943   2.009  0.04596
# drop density_inv
ext_16 <- ext_15[,!names(ext_15) %in% c("density_inv")]
ext_16

train_ext_16 <- ext_16[c(1:200), ]
test_ext_16 <- ext_16[c(201:252), ]
ext_16_lr <- lm(brozek ~ siri+thigh+knee,
                data=train_ext_16)
summary(ext_16_lr)
# Now all the p values are less than 0.03

# d)
pred_16 <- predict(ext_16_lr, test_ext_16)
compare_16 <- data.frame(cbind(real=test_ext_16$brozek, guess=pred_16))
compare_16
e_sel <- sqrt(sum((compare_16$real-compare_16$guess)^2))
e_sel
# error for selected feature is 0.6684466

# e)
# error for selected features is less than error for full model.
# feature selection seems to reduce the overfitting.

# f)
# The error for model 3 is 0.5639797, which is less than error for
# selected features 0.6684466.
# In terms of test accuracy, the feature selection does not find the best model.

