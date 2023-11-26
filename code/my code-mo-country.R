################### Nordpred prediction
##预设函数
function_sum_year5 <- function(table_name, start_year, end_year, current_year){
  
  remain <- current_year - floor((current_year - start_year)/5) * 5 
  year_names <- NULL
  for (i in start_year:end_year) {
    if((i - current_year)/5 - floor((i - current_year)/5) == 0){
      if(i == remain){
        temp <- paste(start_year, i, sep = '-')
        year_names <- append(year_names, temp)
      }
      else{
        temp <- paste(i-4, i, sep = '-')
        year_names <- append(year_names, temp)
      }
    }
  }
  
  table_name <- as.data.frame(table_name)
  new_years <- seq(start_year,end_year,1)
  new_table <- as.data.frame(matrix(data = rep(0, length(year_names)*nrow(table_name)), ncol = length(year_names), nrow = nrow(table_name)))  %>% as.data.frame()
  colnames(new_table) <- year_names
  
  j = 1
  for (i in 1:(end_year - start_year + 1)){
    if((new_years[i] - current_year)/5 - floor((new_years[i] - current_year)/5) != 0){
      new_table[, year_names[j]] <- new_table[,year_names[j]] + table_name[,as.character(new_years[i])]
    }
    else{
      if(j == 1){
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])])
      }
      else{
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])])
      }
      j = j + 1
    }
  }
  return(new_table)
}


### input data
EC <-  read.csv('D:/BAPC/BAPC_packages/ECC.csv')

age_stand <- read.csv('D:/BAPC/BAPC_packages/GBD2019 world population age standard.csv')
####stratification-disease burden
ages <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
          "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
          "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", 
          "90 to 94", "95 plus")

####stratification-total population 
ages_2 <- c("<1 year","1 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
            "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", 
            "90 to 94", "95 plus")

#### stratification-BAPC model
age_3 <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
           "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
           "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")

### A adjust age standard population data
age_stand <- subset(age_stand,age %in% ages_2)
wstand<- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(), 
           age_stand$std_population[3:18] %>% as.numeric(),
           age_stand$std_population[19:21] %>% as.numeric() %>% sum())/sum(age_stand$std_population[1:21])
### B adjust disease burden data——Male
EC_male_Deaths <- subset(EC,age %in% ages & 
                           sex == 'Male' &
                           metric == 'Number' &
                           measure == 'Deaths' &
                           location== "Yemen")[,c(3,4,7,8)]
EC_male_Deaths_n <- dcast(data = EC_male_Deaths, age~year, value.var = "val")
rownames(EC_male_Deaths_n) <- EC_male_Deaths_n$age
EC_male_Deaths_n <- EC_male_Deaths_n[,-1]
# 修改第19行的名字为'85+'
rownames(EC_male_Deaths_n)[19] <- "85+"
# 修改第1行的名字为'0-4'
rownames(EC_male_Deaths_n)[1] <- "0 to 4"
#sum(wstand)
EC_male_Deaths_n[19,] <- EC_male_Deaths_n[19,] + EC_male_Deaths_n[20,] + EC_male_Deaths_n[21,]
EC_male_Deaths_n <- EC_male_Deaths_n[-c(20,21),]
EC_male_Deaths_n[1,] <- EC_male_Deaths_n[1,] + EC_male_Deaths_n[2,] 
EC_male_Deaths_n <- EC_male_Deaths_n[-c(2),]
# 按年龄从小到大排序
start_ages <- as.numeric(regmatches(rownames(EC_male_Deaths_n), regexpr("^\\d+", rownames(EC_male_Deaths_n))))# 如果有'85+'这一行，则把它的起始年龄设置为85
if("85+" %in% rownames(EC_male_Deaths_n)) {
  start_ages[rownames(EC_male_Deaths_n) == "85+"] <- 85
}
EC_male_Deaths_n <- EC_male_Deaths_n[order(start_ages),]
# adjust disease burden data further
rownames(EC_male_Deaths_n) <- age_3
EC_male_Deaths_n <- apply(EC_male_Deaths_n, c(1,2), as.numeric) %>% as.data.frame()

### B adjust disease burden data——Female
EC_female_Deaths <- subset(EC,age %in% ages & 
                             sex == 'Female' &
                             metric == 'Number' &
                             measure == 'Deaths' &
                             location== "Yemen")[,c(3,4,7,8)]
EC_female_Deaths_n <- dcast(data = EC_female_Deaths, age~year, value.var = "val")
rownames(EC_female_Deaths_n) <- EC_female_Deaths_n$age
EC_female_Deaths_n <- EC_female_Deaths_n[,-1]
# 修改第19行的名字为'85+'
rownames(EC_female_Deaths_n)[19] <- "85+"
# 修改第1行的名字为'0-4'
rownames(EC_female_Deaths_n)[1] <- "0 to 4"
#sum(wstand)
EC_female_Deaths_n[19,] <- EC_female_Deaths_n[19,] + EC_female_Deaths_n[20,] + EC_female_Deaths_n[21,]
EC_female_Deaths_n <- EC_female_Deaths_n[-c(20,21),]
EC_female_Deaths_n[1,] <- EC_female_Deaths_n[1,] + EC_female_Deaths_n[2,] 
EC_female_Deaths_n <- EC_female_Deaths_n[-c(2),]
# 按年龄从小到大排序
start_ages <- as.numeric(regmatches(rownames(EC_female_Deaths_n), regexpr("^\\d+", rownames(EC_female_Deaths_n))))# 如果有'85+'这一行，则把它的起始年龄设置为85
if("85+" %in% rownames(EC_female_Deaths_n)) {
  start_ages[rownames(EC_female_Deaths_n) == "85+"] <- 85
}
EC_female_Deaths_n <- EC_female_Deaths_n[order(start_ages),]
# adjust disease burden data further
rownames(EC_female_Deaths_n) <- age_3
EC_female_Deaths_n <- apply(EC_female_Deaths_n, c(1,2), as.numeric) %>% as.data.frame()


##### C input GBD 2019 population
dirname <- dir("D:/BAPC/BAPC_packages/GBD_Population")  
file <- paste0("D:/BAPC/BAPC_packages/GBD_Population/",dirname)  
var_name <- c("location_name","sex_name","year_id","age_group_name","val")  

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name))) 
names(GBD_population)=var_name
for (a in file) {
  data <- fread(a) %>% select(var_name) %>% 
    filter(age_group_name %in% ages_2)
  GBD_population <- rbind(GBD_population,data)
}  

GBD_population$sex_name[GBD_population$sex_name=='both'] <- 'Both'
GBD_population$sex_name[GBD_population$sex_name=='male'] <- 'Male'
GBD_population$sex_name[GBD_population$sex_name=='female'] <- 'Female'
GBD_population <- GBD_population[!duplicated(GBD_population),]
#### organize prediction data
prediction_var_name <- c("location_name","sex","year_id","age_group_name","val")
GBD_population_prediction <- fread('D:/BAPC/BAPC_packages/GBDPR1.csv') %>% 
  select(prediction_var_name) %>% 
  filter(age_group_name %in% ages_2 & year_id %in% 2020:2039)
names(GBD_population_prediction) <- var_name

# conbine for prediction data
GBD <- rbind(GBD_population,GBD_population_prediction)
#combine 0-4
GBD_age4 <- GBD %>% subset(age_group_name %in% c("<1 year","1 to 4")) %>% 
  group_by(location_name,sex_name,year_id) %>% 
  summarize(val=sum(val))
GBD_age4$age_group_name <- '0 to 4'
GBD_age4 <- GBD_age4[,c(1:3,5,4)]
# combine 85+
GBD_age85 <- GBD %>% subset(age_group_name %in% c("85 to 89","90 to 94", "95 plus")) %>% 
  group_by(location_name,sex_name,year_id) %>% 
  summarize(val=sum(val))
GBD_age85$age_group_name <- '85 plus'
GBD_age85 <- GBD_age85[,c(1:3,5,4)]

GBD <- subset(GBD, age_group_name %in% age_3[-c(1,19)])
GBD <- rbind(GBD,GBD_age4,GBD_age85)
GBD <- GBD %>% mutate(age_group_name=fct_relevel(age_group_name,age_3)) %>% 
  arrange(age_group_name)
# unique(GBD$age_group_name)

# organize furthe
GBD_Yemen_male <- subset(GBD,location_name=="Yemen" & sex_name == 'Male')
GBD_Yemen_female <- subset(GBD,location_name=="Yemen" & sex_name == 'Female')

GBD_Yemen_male_n <- dcast(data = GBD_Yemen_male, age_group_name ~ year_id,value.var = c("val"))
GBD_Yemen_female_n <- dcast(data = GBD_Yemen_female, age_group_name ~ year_id,value.var = c("val"))

GBD_Yemen_male_n <- GBD_Yemen_male_n[,-1]
GBD_Yemen_female_n <- GBD_Yemen_female_n[,-1]
#5 years gap

EC_male_Deaths_g <- function_sum_year5(EC_male_Deaths_n,1990,2019,2019)
EC_female_Deaths_g <- function_sum_year5(EC_female_Deaths_n,1990,2019,2019)
GBD_Yemen_male_g <- function_sum_year5(GBD_Yemen_male_n,1990,2039,2019)
GBD_Yemen_female_g <- function_sum_year5(GBD_Yemen_female_n,1990,2039,2019)
rownames(EC_male_Deaths_g) <- rownames(EC_male_Deaths_n)
rownames(EC_female_Deaths_g) <- rownames(EC_female_Deaths_n)
rownames(GBD_Yemen_female_g) <- rownames(EC_female_Deaths_n)
rownames(GBD_Yemen_male_g) <- rownames(EC_male_Deaths_n)

GBD_Yemen_male_g <- apply(GBD_Yemen_male_g, c(1,2), as.numeric) %>% as.data.frame()
GBD_Yemen_female_g <- apply(GBD_Yemen_female_g, c(1,2), as.numeric) %>% as.data.frame()
EC_male_Deaths_g <- apply(EC_male_Deaths_g, c(1,2), as.numeric) %>% as.data.frame()
EC_female_Deaths_g <- apply(EC_female_Deaths_g, c(1,2), as.numeric) %>% as.data.frame()

#### nordpred prediction
male_res <- nordpred(EC_male_Deaths_g, GBD_Yemen_male_g, 
                     noperiods = 4:6, startestage = 5, startuseage = 5, 
                     cuttrend = c(0, .25, .5, .75), linkfunc = "power5", recent = NULL)

female_res <- nordpred(EC_female_Deaths_g, GBD_Yemen_female_g, 
                       noperiods = 4:6, startestage = 5, startuseage = 5,
                       cuttrend = c(0, .25, .5, .75), linkfunc = "power5", recent = NULL)

####calculate the number of cases and incidene of different ages
male_age_rate <- round(nordpred.getpred(male_res, incidence = TRUE, standpop = NULL), 2) 
male_age_standardized <- round(nordpred.getpred(male_res, incidence = TRUE, standpop = wstand), 2) %>% as.data.frame() 
male_age_count <- round(nordpred.getpred(male_res, incidence = F, standpop = NULL), 2) 
male_sum_year <- apply(male_age_count, 2, sum) %>% as.data.frame()

rownames(male_age_rate) <- age_3
rownames(male_age_count) <- age_3
male_age_standardized$year <- rownames(male_age_standardized)
names(male_age_standardized)[1] <- 'ASR'
colnames(male_sum_year) <- 'number'
male_sum_year$year <- rownames(male_sum_year)

female_age_rate <- round(nordpred.getpred(female_res, incidence =  TRUE, standpop = NULL), 2) 
female_age_standardized <- round(nordpred.getpred(female_res,  incidence =   TRUE, standpop = wstand), 2) %>% as.data.frame()
female_age_count <- round(nordpred.getpred(female_res,  incidence =   F, standpop = NULL), 2) 
female_sum_year <- apply(female_age_count, 2, sum) %>% as.data.frame()

rownames(female_age_rate) <- age_3
rownames(female_age_count) <- age_3
female_age_standardized$year <- rownames(female_age_standardized)
names(female_age_standardized)[1] <- 'ASR'
colnames(female_sum_year) <- 'number'
female_sum_year$year <- rownames(female_sum_year)


###预测男女总和
GBD_Yemen_all_g<- GBD_Yemen_female_g+GBD_Yemen_male_g
All_age_cout<- female_age_count+male_age_count
All_age_rate<- All_age_cout/GBD_Yemen_all_g*10^5
All_age_standardized<-matrix(nrow=0,ncol=2)%>%as.data.frame()
names(All_age_standardized)<-c("ASR","year")

for(i in 1:ncol (All_age_cout)){
  asr=ageadjust.direct (count= All_age_cout[,i],pop=GBD_Yemen_all_g[,i],
                        stdpop=wstand)
  All_age_standardized[i,1:2]<-c(round(100000*asr,2)[2],names(All_age_cout)[i])
}
All_sum_year <- (male_sum_year[,1]+female_sum_year[,1])%>% as.data.frame()
colnames(All_sum_year)<"number"
All_sum_year$year <- rownames(female_sum_year)

###绘图
# 为两个数据框添加一个标识列
male_age_standardized$gender <- "Male"
female_age_standardized$gender<- "Female"
All_age_standardized$gender<-"Both"
# 合并两个数据框
combined_data <- rbind(All_age_standardized, male_age_standardized, female_age_standardized)
combined_data$ASR <- as.numeric(as.character(combined_data$ASR))
# 使用ggplot绘图
plot <- ggplot(combined_data, aes(x = year, y = ASR, color = gender, group = gender)) +
  # 实线部分
  geom_line(data = subset(combined_data, year <= "2015-2019"), aes(linetype = "Solid"), size = 1.5) + # size调整为1.5
  # 虚线部分
  geom_line(data = subset(combined_data, year > "2010-2014"), aes(linetype = "Dashed"), size = 1.5) + # size调整为1.5
  geom_point(size = 5) + # size调整为3
  labs(
    x = "Year",
    y = "ASMR"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold", family = "Arial", color = "black"),
    axis.title.y = element_text(size = 14, face = "bold", family = "Arial", color = "black"),
    axis.text.x = element_text(size = 12, family = "Arial", color = "black"),
    axis.text.y = element_text(size = 12, family = "Arial", color = "black"),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("Both"="#7c93bd", "Male" = "#96dafa", "Female" = "#cbb6ff")) +
  scale_linetype_manual(values = c("Solid" = "solid", "Dashed" = "dashed")) +
  scale_x_discrete(breaks = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024", "2025-2029", "2030-2034","2035-2039"))
plot


# 合并第一个和第二个数据框
Yemen_standardized_in <- merge(male_age_standardized[, c("year", "ASR")], female_age_standardized[, c("year", "ASR")], by = "year")
# 再合并第三个数据框
Yemen_standardized_in <- merge(Yemen_standardized_in, All_age_standardized[, c("year", "ASR")], by = "year")
# 给合并后的数据框列命名
colnames(Yemen_standardized_in) <- c("year", "both", "male", "female")
# 将数据框写入Excel文件
write.xlsx(Yemen_standardized_in, file = "Yemen_ASR.xlsx")