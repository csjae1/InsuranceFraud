rm(list = ls()) 
gc() 
# Set Random Seed for Reproducibility
set.seed(1234)

# Exploratory Data Analysis
# load libraries ----
library(data.table)
library(ggplot2)
library(dplyr)
library(psych)
library(caret)
library(class)
library(e1071) # svm
library(nortest)
library(rpart) # decision tree
library(rpart.plot)
library(ROCR) # roc
library(GGally) #correlation analysis package ***
library(mice)



# Load full data set ----
custDF<- fread(input = "data/cust_data.csv",
               header = TRUE)
claimDF<- fread(input = "data/claim_data.csv",
                header = TRUE)
cnttDF<- fread(input = "data/cntt_data.csv",
               header = TRUE)
fmlyDF<- fread(input = "data/fmly_data.csv",
               header = TRUE)
fpinfoDF<- fread(input = "data/fpinfo_data.csv",
                 header = TRUE)
# 전처리 ----
## cust data cleaning
# custDF[SIU_CUST_YN == "",] 
str(custDF)
custDF <- custDF[SIU_CUST_YN != "", -2]
custDF$SIU_CUST_YN <- as.factor(custDF$SIU_CUST_YN)
colSums(is.na(custDF)) #na value check

# RESI_TYPE_CODE(residence), CHLD_CNT(child count), LTBN_CHLD_AGE(youngest children age) na values,
# When OCCP_GRP_2 == "", insert into [기타], 
# Insert JPBASE_HSHD_INCM into is.na(JPBASE_HSHD_INCM)

custDF$RESI_TYPE_CODE[is.na(custDF$RESI_TYPE_CODE)] <- 99
custDF$CHLD_CNT[is.na(custDF$CHLD_CNT)] <- 0
custDF$LTBN_CHLD_AGE[is.na(custDF$LTBN_CHLD_AGE)] <- 0

custDF$OCCP_GRP_2 <- ifelse(custDF$OCCP_GRP_2 == "",
                            sort(unique(custDF$OCCP_GRP_2),decreasing = TRUE)[15],
                            custDF$OCCP_GRP_2)
custDF <- merge(custDF,
                custDF[ , .(mean = mean(JPBASE_HSHD_INCM,na.rm = TRUE)),
                        by = "OCCP_GRP_2"],
                by = "OCCP_GRP_2", all.x = TRUE)

custDF[is.na(custDF$JPBASE_HSHD_INCM), ]$JPBASE_HSHD_INCM <- custDF[is.na(custDF$JPBASE_HSHD_INCM), ]$mean
custDF$mean <- NULL
custDF[OCCP_GRP_1 == "",]$OCCP_GRP_1 <- "8.기타"

str(claimDF)
summary(claimDF)
nrow(claimDF)
colSums(is.na(claimDF))
table(claimDF$HOSP_SPEC_DVSN)
claimDF[is.na(claimDF$HOSP_SPEC_DVSN),]$HOSP_SPEC_DVSN <- 10 # Mode 10 


# Creating Variables ----- 
custDF <- custDF[,c(2:24,1)]

exampleDF <- merge(claimDF, custDF[,c(1,2)], all.x = TRUE, by = "CUST_ID")
exampleDF <- exampleDF[SIU_CUST_YN != "",] 


resn_code <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
resn_code_colnames <- c("사망", "입원", "통원", "장해",
                        "수술", "진단", "치료", "해지/무효")

for (i in 1:length(sort(unique(claimDF$DMND_RESN_CODE)))) {
  sort(unique(claimDF$DMND_RESN_CODE))[i]
  resn_code <- merge(resn_code,
                     claimDF[DMND_RESN_CODE == sort(unique(claimDF$DMND_RESN_CODE))[i] ,
                               .N,
                               by = "CUST_ID,DMND_RESN_CODE"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(resn_code)[1+i] <- resn_code_colnames[i]
}
resn_code[is.na(resn_code)] <- 0
ratio_fun <- function(y){
  y$sum <-apply(y[,-c("CUST_ID")], 1, sum)
  data.table(CUST_ID = sort(unique(custDF$CUST_ID)),
             round(sapply(y[,-c("CUST_ID")], 
                          function(x){x/y$sum}),
                   digits = 2))[,-c("sum")]
}
resn_code <- ratio_fun(resn_code)
resn_code



# Hosp_spec variable 
hosp_spec <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
hosp_spec_colname <- c( "종합병원", "병원", "요양병원", "의원", 
                        "치과병원", "치과의원", "보건의료원", 
                        "약국", "한방병원", "한의원", "해외", "의료기관이외")
for (i in 1:length(sort(unique(claimDF$HOSP_SPEC_DVSN)))) {
  sort(unique(claimDF$HOSP_SPEC_DVSN))[i]
  hosp_spec <- merge(hosp_spec,
                     exampleDF[HOSP_SPEC_DVSN == sort(unique(claimDF$HOSP_SPEC_DVSN))[i] ,
                               .N,
                               by = "CUST_ID,HOSP_SPEC_DVSN"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(hosp_spec)[1+i] <- sort(unique(claimDF$HOSP_SPEC_DVSN))[i]
  colnames(hosp_spec)[1+i] <- hosp_spec_colname[i]
}
hosp_spec[is.na(hosp_spec)] <- 0
hosp_spec <- ratio_fun(hosp_spec)
# Categorizing Accidents
acci_dvsn <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
acci_colname <- c("재해","교통재해","질병")
for (i in 1:length(sort(unique(claimDF$ACCI_DVSN)))) {
  sort(unique(exampleDF$ACCI_DVSN))[i]
  acci_dvsn <- merge(acci_dvsn,
                     claimDF[ACCI_DVSN == sort(unique(claimDF$ACCI_DVSN))[i] ,
                               .N,
                               by = "CUST_ID,ACCI_DVSN"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(acci_dvsn)[1+i] <- sort(unique(claimDF$ACCI_DVSN))[i]
  colnames(acci_dvsn)[1+i] <- acci_colname[i]
}
acci_dvsn[is.na(acci_dvsn)] <- 0
acci_dvsn <- ratio_fun(acci_dvsn)
# Claim Number Variable
claim_num <- exampleDF[,.(청구건수 = .N),by="CUST_ID"]
# Total Hospitalization Variable
vldi_sum <- exampleDF[,.("총입원일수" = sum(VLID_HOSP_OTDA)), by="CUST_ID"]
# Change fp Variable
change_fp <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
acci_colname <- c("No","Y")
for (i in 1:length(sort(unique(claimDF$CHANG_FP_YN)))) {
  sort(unique(claimDF$CHANG_FP_YN))[i]
  change_fp <- merge(change_fp,
                     claimDF[CHANG_FP_YN == sort(unique(claimDF$CHANG_FP_YN))[i] ,
                               .N,
                               by = "CUST_ID,CHANG_FP_YN"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(change_fp)[1+i] <- sort(unique(claimDF$CHANG_FP_YN))[i]
}
change_fp[is.na(change_fp)] <- 0
colnames(change_fp)[2:3] <- c("chang_N", "chang_Y")
change_fp <- change_fp[,":="(chang_ratio = chang_Y / (chang_Y + chang_N)), 
                       by = "CUST_ID"][,c("CUST_ID","chang_ratio")]

# Hospital Related to Fraud Variable
heed_hosp <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
heed_colname <- c("No","Y")
for (i in 1:length(sort(unique(claimDF$HEED_HOSP_YN)))) {
  sort(unique(claimDF$HEED_HOSP_YN))[i]
  heed_hosp <- merge(heed_hosp,
                     claimDF[HEED_HOSP_YN == sort(unique(claimDF$HEED_HOSP_YN))[i],
                               .N,
                               by = "CUST_ID,HEED_HOSP_YN"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(heed_hosp)[1+i] <- sort(unique(claimDF$HEED_HOSP_YN))[i]
}
heed_hosp[is.na(heed_hosp)] <- 0
colnames(heed_hosp)[2:3] <- c("heed_N", "heed_Y")
heed_hosp
heed_hosp <- heed_hosp[,":="(heed_ratio = heed_Y / (heed_Y + heed_N)),
                       by = "CUST_ID"][,c("CUST_ID","heed_ratio")]
summary(heed_hosp)

# Demand Amount Variable
dmnd_amt <- exampleDF[,c("CUST_ID","SIU_CUST_YN","DMND_AMT")]
dmnd_amt <- merge(dmnd_amt[,.(DMND_mean = mean(DMND_AMT)),by = "CUST_ID"],
                  custDF[,1:2], by = "CUST_ID", all.x = TRUE)
by(dmnd_amt$DMND_mean, dmnd_amt$SIU_CUST_YN, ad.test) # 정규성깨짐
wilcox.test(dmnd_amt$DMND_mean ~ dmnd_amt$SIU_CUST_YN, #윌콕슨 순위합 검정
            alternative = "two.sided")
dmnd_amt
# Cust Role Variable
cust_role <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
role_colname <- unique(cnttDF$CUST_ROLE)
for (i in 1:length(sort(unique(cnttDF$CUST_ROLE)))) {
  sort(unique(cnttDF$CUST_ROLE))[i]
  cust_role <- merge(cust_role,
                     cnttDF[CUST_ROLE == sort(unique(cnttDF$CUST_ROLE))[i] ,
                            .N,
                            by = "CUST_ID,CUST_ROLE"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(cust_role)[1+i] <- sort(unique(cnttDF$CUST_ROLE))[i]

}
cust_role[is.na(cust_role)] <- 0
cust_role <- ratio_fun(cust_role)

# Channel Code Variable
chnl_code <- data.table(CUST_ID = sort(unique(custDF$CUST_ID)))
chnl_colname <- c("설계사", "법인", "홈페이지", "전화영업",
                  "방카슈랑스", "남성전문조직", "보험대리점")
for (i in 1:length(sort(unique(cnttDF$SALE_CHNL_CODE)))) {
  sort(unique(cnttDF$SALE_CHNL_CODE))[i]
  chnl_code <- merge(chnl_code,
                     cnttDF[SALE_CHNL_CODE == sort(unique(cnttDF$SALE_CHNL_CODE))[i] ,
                            .N,
                            by = "CUST_ID,SALE_CHNL_CODE"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(chnl_code)[1+i] <- chnl_colname[i]
}
chnl_code[is.na(chnl_code)] <- 0
chnl_code <- ratio_fun(chnl_code)

# Analysis Data
analysisDF <- merge(custDF, resn_code, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, hosp_spec , by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, acci_dvsn, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, claim_num, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, vldi_sum, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, heed_hosp, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, dmnd_amt[,1:2], by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, cust_role, by = "CUST_ID", all.x = TRUE)
analysisDF <- merge(analysisDF, chnl_code, by = "CUST_ID", all.x = TRUE)

# Occupation Variable
occp_grp2 <- data.table(CUST_ID = sort(unique(analysisDF$CUST_ID)))
occp_colname <- c( "사무직", "2차산업 종사자", "3차산업 종사자", 
                   "고위 공무원", "자영업", "공무원", 
                   "대학교수/강사", "운전직", "주부", 
                   "단순 노무직", "1차산업 종사자", 
                   "기타", "교육관련직", "교사", 
                   "전문직", "의료직 종사자", "예체능계 종사자", 
                   "학생", "고소득 전문직", "단순 사무직",
                   "기업/단체 임원", "학자/연구직", "고소득의료직", 
                   "종교인/역술인", "법무직 종사자")
sort(unique(occp_colname))
for (i in 1:length(sort(unique(occp_colname)))) {
  sort(unique(occp_colname))[i]
  occp_grp2 <- merge(occp_grp2,
                     analysisDF[OCCP_GRP_2 == sort(unique(analysisDF$OCCP_GRP_2))[i],
                                .N,
                                by = "CUST_ID,OCCP_GRP_2"][,-2],
                     all.x = TRUE,
                     by = "CUST_ID")
  colnames(occp_grp2 )[1+i] <- sort(occp_colname)[i]
}
occp_grp2 [is.na(occp_grp2 )] <- 0
occp_grp2 
analysisDF <- merge(analysisDF, occp_grp2, by = "CUST_ID", all.x = TRUE)




# Analysis of Variables----

##Clean-up
str(analysisDF)
colSums(is.na(analysisDF))
analysisDF$SEX <- as.factor(analysisDF$SEX)
analysisDF$FP_CAREER <- as.factor(analysisDF$FP_CAREER)
analysisDF$CTPR <- as.factor(analysisDF$CTPR)
analysisDF$OCCP_GRP_1 <- as.factor(analysisDF$OCCP_GRP_1)
analysisDF$OCCP_GRP_2 <- as.factor(analysisDF$OCCP_GRP_2)
analysisDF$WEDD_YN <- as.factor(analysisDF$WEDD_YN)
analysisDF$MATE_OCCP_GRP_1 <- as.factor(analysisDF$MATE_OCCP_GRP_1)
analysisDF$MATE_OCCP_GRP_2 <- as.factor(analysisDF$MATE_OCCP_GRP_2)

colnames(analysisDF)

# ggpairs(analysisDF[, c(2:8,10)])
# ggpairs(analysisDF[, c(2,11:15)])
# ggpairs(analysisDF[, c(2,17:23)])
# ggpairs(analysisDF[, c(2,25:32)]) 
# ggpairs(analysisDF[, c(2,33:44)]) #병원
# ggpairs(analysisDF[, c(2,45:49)])


colnames(analysisDF)

# Housing Price
bx <- ggplot(data=analysisDF, aes(x=SIU_CUST_YN, y=log(RESI_COST)))
bx + geom_boxplot(aes(color=SIU_CUST_YN))

h <- ggplot(data=analysisDF, aes(x=log(RESI_COST)))
h + geom_histogram(aes(colour=SIU_CUST_YN, fill=SIU_CUST_YN))


# cust_incm(Customer Income)
# na 4823
bx <- ggplot(data=analysisDF, aes(x=SIU_CUST_YN, y=CUST_INCM))
bx + geom_boxplot(aes(color=SIU_CUST_YN))
analysisDF[CUST_INCM>1,]

bx <- ggplot(data=analysisDF[CUST_INCM>1,], aes(x=SIU_CUST_YN, y=CUST_INCM))
bx + geom_boxplot(aes(color=SIU_CUST_YN)) #+ geom_jitter(width=0.1, aes(colour=SIU_CUST_YN))
 #+ geom_jitter(width=0.1, aes(colour=SIU_CUST_YN))

custDF[CUST_INCM == 0&SIU_CUST_YN=="Y",]
custDF[CUST_INCM == 0,]
h <- ggplot(data=analysisDF, aes(x=CUST_INCM))
h + geom_histogram(aes(colour=SIU_CUST_YN, fill=SIU_CUST_YN))

# RCBASE_HSHD_INCM
bx <- ggplot(data=analysisDF, aes(x=SIU_CUST_YN, y=RCBASE_HSHD_INCM))
bx + geom_boxplot(aes(color = SIU_CUST_YN)) #+ geom_jitter(width=0.1, aes(colour
bx <- ggplot(data=analysisDF[RCBASE_HSHD_INCM>1,], aes(x=SIU_CUST_YN, y=RCBASE_HSHD_INCM))
bx + geom_boxplot(aes(color = SIU_CUST_YN)) #+ geom_jitter(width=0.1, aes(colour=SIU_CUST_YN))

plot(analysisDF$RCBASE_HSHD_INCM, analysisDF$RESI_COST)

h <- ggplot(data=analysisDF, aes(x=RCBASE_HSHD_INCM))
h + geom_histogram(aes(colour=SIU_CUST_YN, fill=SIU_CUST_YN))

# JPBASE_HSHD_INCM
bx <- ggplot(data=analysisDF[RCBASE_HSHD_INCM>1,], aes(x=SIU_CUST_YN, y=JPBASE_HSHD_INCM))
bx + geom_boxplot(aes(color = SIU_CUST_YN)) #+ geom_jitter(width=0.1, aes(colour=SIU_CUST_YN))

h <- ggplot(data=analysisDF, aes(x=JPBASE_HSHD_INCM))
h + geom_histogram(data=analysisDF, aes(colour=SIU_CUST_YN, fill=SIU_CUST_YN))


# TOTALPREM -(Total Payment) NA : 5321

box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = TOTALPREM))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))

box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(TOTALPREM)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))
h <- ggplot(data=analysisDF, aes(x=log(TOTALPREM)))
h + geom_histogram(data=analysisDF, aes(colour=SIU_CUST_YN, fill=SIU_CUST_YN))


# MINCRDT
bar <- ggplot(data = analysisDF[analysisDF$MINCRDT<12], aes(MINCRDT))
bar + geom_bar(aes(fill=SIU_CUST_YN))

box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(TOTALPREM)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))


# MAXCRDT
bar <- ggplot(data = analysisDF[analysisDF$MAXCRDT<12], aes(MAXCRDT))
bar + geom_bar(aes(fill=SIU_CUST_YN))



# Children Age
box <- ggplot(data = analysisDF[LTBN_CHLD_AGE>0,], aes(x = SIU_CUST_YN, y = LTBN_CHLD_AGE))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))
hist <- ggplot(data = analysisDF[LTBN_CHLD_AGE>0,], aes(x = LTBN_CHLD_AGE, fill = SIU_CUST_YN))
hist <- geom_histogram()

# Children Count
box <- ggplot(data = analysisDF[CHLD_CNT>0,], aes(x = SIU_CUST_YN, y = CHLD_CNT))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))

b <- ggplot(data = analysisDF, aes(x = CHLD_CNT))
b + geom_bar(aes(fill = SIU_CUST_YN))

# MAX_PRM
hist <- ggplot(data = analysisDF[MAX_PRM<5500000], aes(x=MAX_PRM, fill = SIU_CUST_YN))
hist + geom_histogram()

hist <- ggplot(data = analysisDF, aes(x=log(MAX_PRM), fill = SIU_CUST_YN))
hist + geom_histogram()

box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(MAX_PRM)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))

# Fraud by Age
age <- analysisDF[,c(1:2,4)]
age$group <- cut(x      = age$AGE,
                 breaks = seq(from = 0, to = 90, by = 10),
                 left   = FALSE)
levels(age$group) <- c("10세미만", "10대", "20대", "30대", "40대", "50대",
                       "60대", "70대", "80대")
table(age$group, age$SIU_CUST_YN)
h <- ggplot(data = age, aes(x = AGE))
h + geom_histogram( bins = 8,aes(fill = SIU_CUST_YN))

par(mfrow = c(1,1))


# Application Number
box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = 청구건수))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))

box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(청구건수)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))


analysisDF$청구건수1 <- analysisDF$청구건수+100
h <- ggplot(data = analysisDF, aes(x = log(청구건수1)))
h + geom_histogram(aes(fill = SIU_CUST_YN))

h <- ggplot(data = analysisDF, aes(x = log(청구건수)))
h + geom_histogram(aes(fill = SIU_CUST_YN))



# Average Claim Amount
box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = DMND_mean))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))


box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(DMND_mean)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))

h <- ggplot(data = analysisDF, aes(x = DMND_mean))
h + geom_histogram(aes(fill = SIU_CUST_YN))
h <- ggplot(data = analysisDF, aes(x = log(DMND_mean)))
h + geom_histogram(aes(fill = SIU_CUST_YN))



# Total Hospitalization Days
box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = 총입원일수))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))


box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = log(총입원일수)))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))
h <- ggplot(data = analysisDF, aes(x = log(총입원일수)))
h + geom_histogram(aes(fill = SIU_CUST_YN))

analysisDF$총입원일수1 <- analysisDF$총입원일수+100
h <- ggplot(data = analysisDF, aes(x = log(총입원일수1)))
h + geom_histogram(aes(fill = SIU_CUST_YN))


# heed_ratio
box <- ggplot(data = analysisDF, aes(x = SIU_CUST_YN, y = heed_ratio))
box + geom_boxplot(outlier.color = "red", outlier.size = 2, aes(color = SIU_CUST_YN))


# Histogram by jobs
h <- ggplot(data = analysisDF, aes(x = OCCP_GRP_2,
                                   fill = SIU_CUST_YN))
h + geom_bar()



# Job Code
par(mfrow = c(2,4))
for (i in 52:58) {
  boxplot(unlist(analysisDF[,..i]) ~ analysisDF$SIU_CUST_YN, 
          data = analysisDF,
          main = colnames(analysisDF[,..i]))
}


# Accident Analysis
par(mfrow = c(1,3))
for (i in 45:47) {
  boxplot(unlist(analysisDF[,..i]) ~ analysisDF$SIU_CUST_YN, 
          data = analysisDF,
          main = colnames(analysisDF[,..i]))
}



# Claim Reason Code
par(mfrow = c(2,4))
for (i in 25:32) {
  boxplot(unlist(analysisDF[,..i]) ~ analysisDF$SIU_CUST_YN, 
          data = analysisDF,
          main = colnames(analysisDF[,..i]))
}


# Hospital
par(mfrow = c(3,4))
for (i in 33:44) {
  boxplot(unlist(analysisDF[,..i]) ~ analysisDF$SIU_CUST_YN, 
          data = analysisDF,
          main = colnames(analysisDF[,..i]))
}

# Sell Code
colnames(analysisDF)

par(mfrow = c(2,4))
for (i in 84:90) {
  boxplot(unlist(analysisDF[,..i]) ~ analysisDF$SIU_CUST_YN, 
          data = analysisDF,
          main = colnames(analysisDF[,..i]),
          outline = F)
}

# Replacing NA values----
  
colSums(is.na(analysisDF))
nafill <- mice(analysisDF[, c("AGE", "LTBN_CHLD_AGE", "MAX_PRM", 
                                 "청구건수","DMND_mean")],
                 method = "norm.predict", m = 1)
maxprmfill <- complete(nafill)
colSums(is.na(maxprmfill))
analysisDF$MAX_PRM <- maxprmfill$MAX_PRM

analysisDF$MAX_PRM <- log(analysisDF$MAX_PRM)
analysisDF$총입원일수 <- log(analysisDF$총입원일수+100)
analysisDF$DMND_mean <- log(analysisDF$DMND_mean+100)
analysisDF$청구건수 <- log(analysisDF$청구건수)
analysisDF$RESI_COST <- log(analysisDF$RESI_COST+100)


# Choosing Variables for Analysis----
select_col <- c("CUST_ID","SIU_CUST_YN","SEX", "AGE",
                "RESI_COST", "LTBN_CHLD_AGE", "CHLD_CNT",
                "MAX_PRM", "청구건수", "DMND_mean",
                "총입원일수", "0", "1","2", "재해", "교통재해",
                "질병", "입원", "종합병원", "병원", "의원",
                "한방병원", "의료기관이외")

analysisDF2 <- analysisDF[,..select_col]

analysisDF2$SIU_CUST_YN <- ifelse(analysisDF2$SIU_CUST_YN == "Y", 1,0)
analysisDF2$SIU_CUST_YN <- as.factor(analysisDF2$SIU_CUST_YN)





# Removing Outlier 
outlier <- c("RESI_COST", "MAX_PRM", "청구건수","총입원일수", "DMND_mean")

for (i in outlier) {
  boxplot(analysisDF2[,i,with=FALSE])
  top <- 1.5*IQR(unlist(analysisDF2[,i,with = FALSE]), na.rm = TRUE) + summary(unlist(analysisDF2[,i,with = FALSE]), na.rm = TRUE)[5]
  bottom <- summary(unlist(analysisDF2[,i,with = FALSE]), na.rm = TRUE)[2] - 1.5*IQR(unlist(analysisDF2[,i,with = FALSE]), na.rm = TRUE)
  analysisDF2[unlist(analysisDF2[,i,with = FALSE]) > top][,i] <- top
  analysisDF2[unlist(analysisDF2[,i,with = FALSE]) < bottom][,i] <- bottom
}




# Logistic Regression----
##Sampling 1
# index <- createDataPartition(analysisDF2$SIU_CUST_YN, p=0.7, list = FALSE)
# train <- analysisDF2[index,-1]
# 
# set.seed(1234)
# sampleN <- train[SIU_CUST_YN == 0,] %>% sample_n(5060)#
# 
# train <- rbind(sampleN, train[SIU_CUST_YN == 1,]) 
# test <- analysisDF2[-index,-1]


# Sampling 2
analysisDF3 <- rbind(analysisDF2[SIU_CUST_YN == 1,],analysisDF2[SIU_CUST_YN == 0,] %>% sample_n(9030))


set.seed(1234)
index <- createDataPartition(analysisDF3$SIU_CUST_YN, p=0.7, list = FALSE)
train <- analysisDF3[index,]
test <- analysisDF3[-index,]


train <- train[,-1]
test <- test[,-1]

logit.model <- glm(SIU_CUST_YN ~ ., family=binomial(link='logit'), data=train)
summary(logit.model)
pred.logit <- predict(logit.model, newdata=test, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN))  # 0.4509 ->0.4991 / pc9 0.439 pc7 0.4284 pc30 0.4372 pc 37 0.4386 / pc9-이상치 0.3683 / pc9 -이상치조정 결측치많은변수제거 0.4381
pred.logit <- predict(logit.model, newdata=analysisDF2, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.logit), as.factor(analysisDF2$SIU_CUST_YN))  # 0.4509 ->0

library(car)


###교차검증
# 교차검증 (Caret 패키지)
## 모형 생성
# * Titanic 데이터와 glm 알고리듬 사용
# * number-겹으로 repeats 번 반복
# * 분류를 위해 Survived를 factor로 변형
library(caret)
control <- trainControl(method="repeatedcv", number=20, repeats=10)
model.cv <- train( SIU_CUST_YN ~ .,
                   data=train, method="glm", na.action=na.omit,
                   family=binomial(link='logit'), trControl=control)
print(model.cv)

##혼동행렬
pred.cv <- predict(model.cv, test, type = "raw", na.action = na.pass)
confusionMatrix(as.factor(pred.cv), as.factor(test$SIU_CUST_YN))

# step
step(object = logit.model, direction = "both")

logit.stepmodel <- glm(SIU_CUST_YN ~ SEX + AGE + RESI_COST + LTBN_CHLD_AGE + 
                         청구건수 + DMND_mean + 총입원일수 + `0` + `1` + 
                         `2` + 재해  + 질병 + 입원 + 종합병원 + 
                         병원 + 의료기관이외, family=binomial(link='logit'), data=train)
summary(logit.stepmodel)
pred.steplogit <- predict(logit.stepmodel, newdata=test, type='response')
pred.steplogit <- ifelse(pred.steplogit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.steplogit), as.factor(test$SIU_CUST_YN))

vif(logit.stepmodel)




# # pca이용
# par(mfrow = c(1,1))
# # pca_col <- c("SEX", "AGE",
# #              "RESI_COST", "LTBN_CHLD_AGE", "CHLD_CNT",
# #              "MAX_PRM", "청구건수", "DMND_mean",
# #              "총입원일수", "0", "1", "재해", "교통재해",
# #              "질병", "입원", "종합병원", "병원", "의원",
# #              "한방병원", "의료기관이외")
# colnames(analysisDF)
# colSums(is.na(analysisDF))
# pca_col <- c("SEX", "AGE",
#              "RESI_COST", "LTBN_CHLD_AGE", "CHLD_CNT",
#              "MAX_PRM", "청구건수", "DMND_mean",
#              "총입원일수", "0", "1", "재해", "교통재해",
#              "질병", "입원","통원", "장해", "수술", "진단",            
#              "치료", "해지/무효", "종합병원", "병원", "의원",
#              "한방병원", "의료기관이외","주부","자영업")
# 
# 
# pca_colDF <- analysisDF2[,pca_col,with = FALSE]
# pca_analysisDF <- prcomp(pca_colDF, center = TRUE, scale. = TRUE)
# screeplot(pca_analysisDF, npcs = 20, type = "lines")
# summary(pca_analysisDF)
# pca_analysisDF <- as.matrix(pca_colDF)%*%pca_analysisDF$rotation[,1:11] 
# pca_analysisDF <- data.table(analysisDF2[,1:2] , pca_analysisDF)
# 
# train <- pca_analysisDF[index,-1]
# set.seed(1234)
# sampleN <- train[SIU_CUST_YN == 0,] %>% sample_n(5060)#
# train <- rbind(sampleN, train[SIU_CUST_YN == 1,]) 
# test <- pca_analysisDF[-index,-1]
# 
# # 로지스틱
# logit.model <- glm(SIU_CUST_YN ~ ., family=binomial(link='logit'), data=train)
# summary(logit.model)
# pred.logit <- predict(logit.model, newdata=test, type='response')
# pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
# confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN)) 
# 
# 
# library(caret)
# control <- trainControl(method="repeatedcv", number=5, repeats=10)
# model.cv <- train( SIU_CUST_YN ~ .,
#                    data=train, method="glm", na.action=na.omit,
#                    family=binomial(link='logit'), trControl=control)
# print(model.cv)
# 
# ##혼동행렬
# pred.cv <- predict(model.cv, test, type = "raw", na.action = na.pass)
# confusionMatrix(as.factor(pred.cv), as.factor(test$SIU_CUST_YN))
# 


# 의사결정나무

fit.df <- rpart(SIU_CUST_YN ~ .,
                method = "class", 
                data   = train)
summary(fit.df)
rpart.plot(fit.df)
## 예측
pred.df <- predict(fit.df, test, type = "class")
## 혼동행렬
confusionMatrix(pred.df, as.factor(test$SIU_CUST_YN))


# Linear Kernel
## SVM 훈련
# * Support Vectors: 잘못 분류된 데이터
# * SVM-Kernel: Kernel의 종류
# * cost: SVM의 cost
# * gamma: kernel에 사용되는 모수 (linear kernel에서는 사용되지 않음)

svm.linear <- svm(SIU_CUST_YN ~ ., 
                  data=train, 
                  kernel="linear")
summary(svm.linear)

## 예측/혼동행렬
pred.linear <- predict(svm.linear, test)
confusionMatrix(pred.linear, test$SIU_CUST_YN)

# Radial Kernel
## SVM 훈련

svm.radial <- svm(SIU_CUST_YN ~ ., 
                  data=train,
                  kernel="radial")
summary(svm.radial)


## 예측/혼동행렬
pred.radial <- predict(svm.radial, test)
confusionMatrix(pred.radial, test$SIU_CUST_YN)


## 모수 선택 (Radial Kernel, 5-fold CV)
# * gamma = 1/200, 2/200, 3/200
# * cost = 8, 10, 12, 14
# grid search
tune.radial.cross <- tune(svm, SIU_CUST_YN ~ ., data = train, kernel="radial",
                          ranges = list(gamma = c(1:3)/200, cost = c(1:5)*2),
                          tunecontrol = tune.control(sampling = "cross", cross=5))

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  6 
#      gamma:  0.01 

tune.radial.cross <- tune(svm, SIU_CUST_YN ~ ., data = train, kernel="radial",
                          ranges = list(gamma = c(1:3)/200, cost = c(3:8)*2),
                          tunecontrol = tune.control(sampling = "cross", cross=5))
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  8 
#      gamma:  0.01 


tune.radial.cross <- tune(svm, SIU_CUST_YN ~ ., data = train, kernel="radial",
                          ranges = list(gamma = c(1:3)/200, cost = c(7:10)*2),
                          tunecontrol = tune.control(sampling = "cross", cross=5))


plot(tune.radial.cross)
tune.radial.cross$best.performance
tune.radial.cross$best.model

# 선택한 모델 예측
pred.tuned <- predict(tune.radial.cross$best.model, test[,-1])
confusionMatrix(pred.tuned, test$SIU_CUST_YN)


## 모수 선택 (Radial Kernel, bootstrap 5번)
# * gamma = 1/200, 2/200, 3/200
# * cost = 8, 10, 12, 14

# grid search
tune.radial.cross <- tune(svm, SIU_CUST_YN ~ ., data = train, kernel="radial",
                          ranges = list(gamma = c(1:3)/200, cost = c(2:5)*2),
                          tunecontrol = tune.control(sampling = "bootstrap", 
                                                     boot.size=0.8, nboot=5))

plot(tune.radial.cross)

tune.radial.cross$best.performance
tune.radial.cross$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  10 
# gamma:  0.005 
# Number of Support Vectors:  1599

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  8 
# gamma:  0.005 


# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  2 
#      gamma:  0.015 

test
pred.tuned <- predict(tune.radial.cross$best.model, test)
confusionMatrix(pred.tuned, test$SIU_CUST_YN)
length(pred.tuned)
length(test$SIU_CUST_YN)

library(caret)
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)
colnames(train)
colnames(train)[11:13] <- c("zero","one","two")
colnames(test)[11:13] <- c("zero","one","two")


train.bagging3 <- randomForest(SIU_CUST_YN ~ ., data = train,
                               mtry=3, ntree = 500, importance = TRUE)
train.bagging4 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=4, ntree = 500, importance = TRUE)
train.bagging5 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=5, ntree = 500, importance = TRUE)
train.bagging6 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=6, ntree = 500, importance = TRUE)
train.bagging7 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=7, ntree = 500, importance = TRUE)
train.bagging8 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=8, ntree = 500, importance = TRUE)
train.bagging9 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=9, ntree = 500, importance = TRUE)
train.bagging10 <- randomForest(SIU_CUST_YN ~ ., data = train, 
                               mtry=10, ntree = 500, importance = TRUE)

pred.rf3 <- predict(train.bagging3, newdata=test)
pred.rf4 <- predict(train.bagging4, newdata=test)
pred.rf5 <- predict(train.bagging5, newdata=test)
pred.rf6 <- predict(train.bagging6, newdata=test)
pred.rf7 <- predict(train.bagging7, newdata=test)
pred.rf8 <- predict(train.bagging7, newdata=test)
pred.rf9 <- predict(train.bagging7, newdata=test)
pred.rf10 <- predict(train.bagging10, newdata=test)

mean((as.integer(data.frame(pred.rf3)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf4)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf5)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf6)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf7)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf8)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf9)[,1]) - as.integer(test$SIU_CUST_YN))^2)
mean((as.integer(data.frame(pred.rf10)[,1]) - as.integer(test$SIU_CUST_YN))^2)

confusionMatrix(pred.rf3, test$SIU_CUST_YN)
confusionMatrix(pred.rf4, test$SIU_CUST_YN)
confusionMatrix(pred.rf5, test$SIU_CUST_YN)
confusionMatrix(pred.rf6, test$SIU_CUST_YN)
confusionMatrix(pred.rf7, test$SIU_CUST_YN)
confusionMatrix(pred.rf8, test$SIU_CUST_YN)
confusionMatrix(pred.rf9, test$SIU_CUST_YN)
confusionMatrix(pred.rf10, test$SIU_CUST_YN)


importance(train.bagging10)
varImpPlot(train.bagging10)

library(xgboost)
library(caret)
library(rpart) # kyphosis
colnames(train)



str(train)
train <- data.frame(train)
train.boost <- xgboost(data  = data.matrix(sapply(train[,2:22], as.numeric)), 
                       label = train[,1],
                       max_depth = 4, eta = 0.1, nrounds = 100,
                       objective = "multi:softmax", num_class = 2)


train.boost <- xgboost(data = data.matrix(sapply(train[,2:22], as.numeric)),
                        label = train[,1],
                        max_depth = 3, eta = 0.3, nrounds = 150,
                        eval_metric = "rmse"
)
pred.train <- predict(train.boost,
                       newdata = data.matrix(sapply(test[,-1], as.numeric)),
                       ntreelimit = 150)
mean((pred.mtcars-test$SIU_CUST_YN)^2)
pred.train

importance.train <- xgb.importance(feature_names = colnames(train[,-1]),
                                    model = train.boost)
importance.train

xgb.plot.importance(importance_matrix = importance.train)

train_control <- trainControl(method="cv", number=5)
grid = expand.grid(
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1,
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.2, 0.3, 0.4, 0.5),
  nrounds = 150
)

train.boost.caret <- train(x = data.matrix(sapply(train[,-1], as.numeric)),
                           y = train[,1],
                           trControl=train_control, tuneGrid=grid,
                           method="xgbTree")
print(train.boost.caret)

pred.train.caret <- predict(train.boost.caret,
                             newdata = data.matrix(sapply(test[,-1], as.numeric)),
                             ntreelimit = 150)
mean((pred.mtcars.caret-mtcars.test$mpg)^2)


confusionMatrix(pred.train.caret, test$SIU_CUST_YN)








# 변수선택
?step
step(logit.model, direction = "both")
step(logit.model, direction = "backward")
step(logit.model, direction = "forward")


# step이후 로지스틱 회귀분석
logit.model <- glm(SIU_CUST_YN ~ SEX + AGE + LTBN_CHLD_AGE + CHLD_CNT + 
                     청구건수 + DMND_mean + 총입원일수 + `1` + 교통재해 + 
                     질병 + 입원 + 종합병원 + 병원 + 의원 + 한방병원 + 
                     의료기관이외, family=binomial(link='logit'), data=train[,-1])
summary(logit.model)
pred.logit <- predict(logit.model, newdata=test, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN))





train <- pca_analysisDF[index,]
test <- pca_analysisDF[-index,]

logit.model <- glm(SIU_CUST_YN ~ ., family=binomial(link='logit'), data=train[,-1])
summary(logit.model)
pred.logit <- predict(logit.model, newdata=test, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)

confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN))  # 0.4509 ->0.4991 / pc9 0.439 pc7 0.4284 pc30 0.4372 pc 37 0.4386 / pc9-이상치 0.3683 / pc9 -이상치조정 결측치많은변수제거 0.4381





# 이상치 제거
outlier <- c("RESI_COST", "MAX_PAYM_YM", "MAX_PRM", "CUST_INCM", "RCBASE_HSHD_INCM", 
             "JPBASE_HSHD_INCM", "청구건수", "총입원일수")

for (i in outlier) {
  boxplot(train[,i,with=FALSE])
  top <- 1.5*IQR(unlist(train[,i,with = FALSE]), na.rm = TRUE) + summary(unlist(train[,i,with = FALSE]), na.rm = TRUE)[5]
  train[unlist(train[,i,with = FALSE]) > top][,i] <- top
}
for (i in c("TOTALPREM", "DMND_mean")) {
  boxplot(log(train[,i,with=FALSE]))
  top <- 1.5*IQR(unlist(log(train[,i,with=FALSE])), na.rm = TRUE) + summary(unlist(log(train[,i,with=FALSE])), na.rm = TRUE)[5]
  bottom <- summary(unlist(log(train[,i,with=FALSE])), na.rm = TRUE)[2] - 1.5*IQR(unlist(log(train[,i,with=FALSE])), na.rm = TRUE)
  train[unlist(log(train[,i,with=FALSE])) > top][,i] <- exp(top)
  train[unlist(log(train[,i,with=FALSE])) < bottom][,i] <- exp(bottom)
}



# train / test
index <- createDataPartition(analysisDF$SIU_CUST_YN, p=0.7, list = FALSE)

train <- analysisDF[index,]
test <- analysisDF[-index,]

# 분석1 - 로지스틱 회귀분석

## PCA 분석 (차원축소)

analysisDF$SIU_CUST_YN <- ifelse(analysisDF$SIU_CUST_YN == "Y", 1,0)
analysisDF$FP_CAREER <- ifelse(analysisDF$FP_CAREER == "Y", 1,0)
analysisDF$WEDD_YN <- ifelse(analysisDF$WEDD_YN == "Y", 1,0)
analysisDF$SIU_CUST_YN <- as.factor(analysisDF$SIU_CUST_YN)

colnames(analysisDF)
colSums(is.na(analysisDF))
pca_col <- c("사망", "입원", "통원", "장해",
             "수술", "진단", "치료", "해지/무효",
             "종합병원", "병원", "요양병원", "의원",
             "치과병원", "치과의원", "보건의료원", 
             "약국", "한방병원", "한의원", "해외", 
             "의료기관이외", "재해", "교통재해", 
             "질병", "0", "1", "2", 
             "3", "4", "5", "21", "1차산업 종사자", 
             "2차산업 종사자", "3차산업 종사자", 
             "고소득 전문직", "고소득의료직", 
             "고위 공무원", "공무원", "교사", 
             "교육관련직", "기업/단체 임원", "기타", 
             "단순 노무직","단순 사무직", 
             "대학교수/강사", "법무직 종사자", 
             "사무직", "예체능계 종사자", "운전직", 
             "의료직 종사자", "자영업", "전문직", 
             "종교인/역술인","주부", "학생","학자/연구직",
             "SEX", "AGE", "RESI_COST", "RESI_TYPE_CODE",  
             "FP_CAREER", "WEDD_YN", "CHLD_CNT", "LTBN_CHLD_AGE",
             "JPBASE_HSHD_INCM", "DMND_mean", 
             "청구건수", "총입원일수", "heed_ratio")


pca_colDF <- analysisDF[,pca_col,with = FALSE]
pca_analysisDF <- prcomp(pca_colDF, center = TRUE, scale. = TRUE)
screeplot(pca_analysisDF, npcs = 68, type = "lines")
summary(pca_analysisDF)
analysisDF2 <- as.matrix(pca_colDF)%*%pca_analysisDF$rotation[,1:35] 
analysisDF <- cbind(analysisDF, as.data.table(analysisDF2))
analysisDF <- analysisDF[,-pca_col, with = FALSE]
analysisDF

head(analysisDF)
resn_code #  청구 사유
hosp_spec # 병원종별구분
acci_dvsn # 사고구분
claim_num # 청구 건수
vldi_sum # 총입원일수
heed_hosp # 유의병원 여부
dmnd_amt # 청구금액
cust_role #고객역할코드
change_fp # fp변경
cust_role # 고객역할할 비율





fp_y_ratio <- merge(cnttDF[,c(1:2,8)], train[,1:2], by = "CUST_ID")
ratio <- fp_y_ratio[,.N,by = "CLLT_FP_PRNO,CUST_ID,SIU_CUST_YN"]
ratio <- round(prop.table(table(ratio$CLLT_FP_PRNO, 
                                ratio$SIU_CUST_YN),margin = 1), digits = 2)
ratio
ratio <- data.table(CLLT_FP_PRNO=as.integer(rownames(ratio)), 
                    y_ratio = ratio[,2])
colSums(is.na(ratio))
colSums(is.na(fp_y_ratio))
fp_y_ratio <- merge(cnttDF, ratio, by = "CLLT_FP_PRNO", all.x = TRUE)
fp_y_ratio[is.na(y_ratio),]$y_ratio <- 0


fp_y_ratio <- fp_y_ratio[,.(fp_y_ratio = round(geometric.mean(y_ratio),digits = 2)), 
                         by="CUST_ID"]

train <- merge(train, fp_y_ratio, by = "CUST_ID", all.x = TRUE)
test <- merge(test, fp_y_ratio, by = "CUST_ID", all.x = TRUE)





claimDF[is.na(HOSP_CODE),]$HOSP_CODE <- 0
colnames(claimDF)

hosp_y_ratio <- merge(claimDF[,c(1:2,23)], train[,1:2], by = "CUST_ID")
ratio <- hosp_y_ratio[,.N,by = "HOSP_CODE,CUST_ID,SIU_CUST_YN"]
ratio <- round(prop.table(table(ratio$HOSP_CODE, 
                                ratio$SIU_CUST_YN),margin = 1), digits = 2)
ratio
ratio <- data.table(HOSP_CODE=as.integer(rownames(ratio)), 
                    y_ratio = ratio[,2])

hosp_y_ratio <- merge(claimDF, ratio, by = "HOSP_CODE", all.x = TRUE)
hosp_y_ratio[is.na(y_ratio),]$y_ratio <- 0


hosp_y_ratio <- hosp_y_ratio[,.(hosp_y_ratio = round(geometric.mean(y_ratio),
                                                     digits = 2)), 
                             by="CUST_ID"]

train <- merge(train, hosp_y_ratio, by = "CUST_ID", all.x = TRUE)
test <- merge(test, hosp_y_ratio, by = "CUST_ID", all.x = TRUE)


colSums(is.na(train))
removeVar <- c("CUST_RGST","CTPR","OCCP_GRP_1","TOTALPREM",
               "MINCRDT","MAXCRDT","MATE_OCCP_GRP_1","MATE_OCCP_GRP_2",
               "MAX_PAYM_YM","MAX_PRM","CUST_INCM","RCBASE_HSHD_INCM","OCCP_GRP_2")
train <- train %>% select(-one_of(removeVar))
test <- test %>% select(-one_of(removeVar))


##### Logistic Regression

train <- analysisDF[index,]
test <- analysisDF[-index,]

logit.model <- glm(SIU_CUST_YN ~ ., family=binomial(link='logit'), data=train[,-1])
summary(logit.model)
pred.logit <- predict(logit.model, newdata=test, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN))  # 0.4509 ->0.4991 / pc9 0.439 pc7 0.4284 pc30 0.4372 pc 37 0.4386 / pc9-이상치 0.3683 / pc9 -이상치조정 결측치많은변수제거 0.4381
library(car)
vif(logit.model)
# 변수선택
?step
step(logit.model, direction = "both")
step(logit.model, direction = "backward")
step(logit.model, direction = "forward")


logit.fit <-  glm( SIU_CUST_YN ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                     PC8 + PC9 + PC10 + PC11 + PC13 + PC15 + PC16 + PC17 + PC18 + 
                     PC19 + PC20 + PC21 + PC24 + PC25 + PC26 + PC28 + PC29 + PC30 + 
                     PC31 + PC32 + PC33 + PC35, 
                  family = binomial(link = "logit"), data = train[, -1])
summary(logit.fit)
vif(logit.fit)
pred.logit <- predict(logit.fit, newdata=test, type='response')
pred.logit <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN))  # 0.4519 ->0.4999



###교차검증
# 교차검증 (Caret 패키지)
## 모형 생성
# * Titanic 데이터와 glm 알고리듬 사용
# * number-겹으로 repeats 번 반복
# * 분류를 위해 Survived를 factor로 변형
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=5)
model.cv <- train( SIU_CUST_YN ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                     PC8 + PC9 + PC10 + PC11 + PC13 + PC15 + PC16 + PC17 + PC18 + 
                     PC19 + PC20 + PC21 + PC24 + PC25 + PC26 + PC28 + PC29 + PC30 + 
                     PC31 + PC32 + PC33 + PC35,
                          data=train, method="glm", na.action=na.omit,
                          family=binomial(link='logit'), trControl=control)
print(model.cv)

##혼동행렬
pred.cv <- predict(model.cv, test, type = "raw", na.action = na.pass)
confusionMatrix(as.factor(pred.cv), as.factor(test$SIU_CUST_YN))



# pc9 0.4409 / pc9 이상치 결측치변수제거 0.437
# test에 예측값 추가
test$logit.model <- pred.logit
confusionMatrix(as.factor(pred.logit), as.factor(test$SIU_CUST_YN)) 
vif(logit.fit)
##### Decision Tree
tree.model <- rpart(SIU_CUST_YN ~ SEX + FP_CAREER + JPBASE_HSHD_INCM + 
                      청구건수 + 총입원일수 + PC1 + PC5 + PC7 + PC8 + 
                      fp_y_ratio + hosp_y_ratio, 
                    method="class", data = train) 
summary(tree.model)
par(family = "AppleGothic")
# theme_update(text=element_text(family="AppleGothic"))
rpart.plot(tree.model)
# decision tree : 예측
tree.model <- rpart(SIU_CUST_YN ~ .
                    , data = train) 
colnames(test)
pred.cu <- predict(tree.model, test[,1:25], type="class")
confusionMatrix(pred.cu, as.factor(test$SIU_CUST_YN)) # Kappa : 0.2607-> 0.3044
# pc9 이상치 결측치 0.2607
# test에 예측값 추가
test$dt.model <- pred.cu
##### RF
# rf.model <- randomForest(SIU_CUST_YN ~ ., data = train, ntree=500, importance=TRUE)
# rf.model
# varImpPlot(rf.model)
# pred.rf <- predict(rf.model, newdata=test[,1:22])
# confusionMatrix(pred.rf, as.factor(test$SIU_CUST_YN)) #0.2991
# # test에 예측값 추가
# test$rf.model1 <- pred.rf
rf.model2 <- randomForest(SIU_CUST_YN ~ SEX + RESI_COST + LTBN_CHLD_AGE + 
                            청구건수 + 총입원일수 + PC1 + PC2 + PC3 + PC4 + 
                            PC8 + fp_y_ratio, data=train, ntree=500, mtry = 4, importance=TRUE)
rf.model2
varImpPlot(rf.model2)
pred.rf2 <- predict(rf.model2, newdata = test[,1:24])
confusionMatrix(pred.rf2, as.factor(test$SIU_CUST_YN)) #0.3029  ->0.3387  /train1 0.3413
# pc9 이상치 결측치 0.3017
# test에 예측값 추가
test$rf.model2 <- pred.rf2
##### SVM
# kernel = linear
# svm.linear <- svm(SIU_CUST_YN ~ ., data=train, kernel="linear")
# summary(svm.linear)
# 
# plot(svm.linear, test, fp_y_ratio ~ 총입원일수)
# 
# pred.linear <- predict(svm.linear, test[ , 1:22])
# confusionMatrix(pred.linear, test$SIU_CUST_YN) #0.4583
# 
# # kernel = radial
# svm.radial <- svm(SIU_CUST_YN ~ ., data=train, kernel="radial")
# summary(svm.radial)
# 
# plot(svm.linear, test, fp_y_ratio ~ 총입원일수)
# 
# pred.radial <- predict(svm.radial, test[,1:22])
# confusionMatrix(pred.radial, test$SIU_CUST_YN) #0.1985
# 
# # tuning
# tune.linear.cross <- tune(svm, SIU_CUST_YN ~ ., data = train, kernel="linear",
#                           ranges = list(cost = c(0.1:4)*2),
#                           # tunecontrol = tune.control(sampling = "cross", cross=5)
# )
# plot(tune.linear.cross)
# tune.linear.cross$best.performance
# 
# 
## 
a <- test[,c("SIU_CUST_YN","logit.model","dt.model","rf.model2")]
a$res <- ifelse(a$logit.model == 1 | a$dt.model == 1 | 
                  a$rf.model2 == 1,1,0)
confusionMatrix(as.factor(a$res), as.factor(a$SIU_CUST_YN)) # 0.3494 -> 0.4513 -> 0.4966   
# pc9 이상치 결측치 0.4308