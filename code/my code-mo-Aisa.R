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
                              location== 'Asia')[,c(3,4,7,8)]
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
                                location== 'Asia')[,c(3,4,7,8)]
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
GBD_Asia_male <- subset(GBD,location_name=='Asia' & sex_name == 'Male')
GBD_Asia_female <- subset(GBD,location_name=='Asia' & sex_name == 'Female')

GBD_Asia_male_n <- dcast(data = GBD_Asia_male, age_group_name ~ year_id,value.var = c("val"))
GBD_Asia_female_n <- dcast(data = GBD_Asia_female, age_group_name ~ year_id,value.var = c("val"))

GBD_Asia_male_n <- GBD_Asia_male_n[,-1]
GBD_Asia_female_n <- GBD_Asia_female_n[,-1]
#5 years gap

EC_male_Deaths_g <- function_sum_year5(EC_male_Deaths_n,1990,2019,2019)
EC_female_Deaths_g <- function_sum_year5(EC_female_Deaths_n,1990,2019,2019)
GBD_Asia_male_g <- function_sum_year5(GBD_Asia_male_n,1990,2039,2019)
GBD_Asia_female_g <- function_sum_year5(GBD_Asia_female_n,1990,2039,2019)
rownames(EC_male_Deaths_g) <- rownames(EC_male_Deaths_n)
rownames(EC_female_Deaths_g) <- rownames(EC_female_Deaths_n)
rownames(GBD_Asia_female_g) <- rownames(EC_female_Deaths_n)
rownames(GBD_Asia_male_g) <- rownames(EC_male_Deaths_n)

GBD_Asia_male_g <- apply(GBD_Asia_male_g, c(1,2), as.numeric) %>% as.data.frame()
GBD_Asia_female_g <- apply(GBD_Asia_female_g, c(1,2), as.numeric) %>% as.data.frame()
EC_male_Deaths_g <- apply(EC_male_Deaths_g, c(1,2), as.numeric) %>% as.data.frame()
EC_female_Deaths_g <- apply(EC_female_Deaths_g, c(1,2), as.numeric) %>% as.data.frame()

#### nordpred prediction
male_res <- nordpred(EC_male_Deaths_g, GBD_Asia_male_g, 
                     noperiods = 4:6, startestage = 5, startuseage = 5, 
                     cuttrend = c(0, .25, .5, .75), linkfunc = "power5", recent = NULL)

female_res <- nordpred(EC_female_Deaths_g, GBD_Asia_female_g, 
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
GBD_Asia_all_g<- GBD_Asia_female_g+GBD_Asia_male_g
All_age_cout<- female_age_count+male_age_count
All_age_rate<- All_age_cout/GBD_Asia_all_g*10^5
All_age_standardized<-matrix(nrow=0,ncol=2)%>%as.data.frame()
names(All_age_standardized)<-c("ASR","year")

for(i in 1:ncol (All_age_cout)){
  asr=ageadjust.direct (count= All_age_cout[,i],pop=GBD_Asia_all_g[,i],
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
  theme_minimal(base_size = 12) + 
  theme(
    panel.background = element_blank(), # 设置图形背景透明
    panel.grid.major = element_line(), # 设置主要辅助线为浅灰色
    panel.grid.minor = element_line(), # 设置次要辅助线为点线
    axis.text.x = element_text(size = 20, family = "Arial", colour = "black"),  
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.title.x = element_text(size = 22, family = "Arial", colour = "black", face = "bold"), 
    axis.title.y = element_text(size = 22, family = "Arial", colour = "black", face = "bold"),
    plot.title = element_text(size = 20, family = "Arial", colour = "black"),
    legend.position = "none"
  ) +
  geom_vline(xintercept = "2015-2019", linetype = "dashed", color = "black", size = 0.5) +
  scale_color_manual(values = c("Both"="#7c93bd", "Male" = "#96dafa", "Female" = "#cbb6ff")) +
  scale_linetype_manual(values = c("Solid" = "solid", "Dashed" = "dashed")) +
  scale_x_discrete(breaks = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024", "2025-2029", "2030-2034","2035-2039"))+
  scale_y_continuous(limits = c(0.2, 3), breaks = seq(0.2, 3, by = 0.5))
plot


######------------------------------------------------------------------------±1%对比图
###—————————————————————————————————————————-------------------------——male 
##------------------1990-2017年之间观察到的死亡人数（1990-2019图）
EC_male <- subset(EC,age=="All Ages" & 
                    sex == 'Male' &
                    metric == 'Number' &
                    measure == 'Deaths' &
                    location== 'Asia')[,c(7,8)]

ggplot(data = EC_male,aes(x = year, y = val)) + geom_point()
##--------------2019年的各年龄组发病率（2019预测2039）
#number
EC_male_Deaths <- subset(EC,age %in% ages & 
                           sex == 'Male' &
                           metric == 'Number' &
                           measure == 'Deaths' &
                           location== 'Asia')[,c(3,4,7,8)]
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

rate_2019 <- EC_male_Deaths_n[,30]
rate_2019 <- data.frame(rate_2019)
colnames(rate_2019)[1] <- "number"
rownames(rate_2019) <- c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29",
                         "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                         "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")

##population
rownames(GBD_Asia_male_n) <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
                               "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
                               "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")


pop_2019<- GBD_Asia_male_n %>% as.data.table()
pop_2019 <-  pop_2019[, .(age_name = c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
                                       "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
                                       "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus"), 
                          val = pop_2019$'2019')]

rate_2019$val <- 100000 * rate_2019$number/pop_2019$val


new_years <- seq(2020, 2039, 1) %>% as.character()

Asia_male_3 <- data.frame(year = new_years, number = rep(0, 20))

for (i in 1:20) {
  Asia_male_3$number[i] <- sum(GBD_Asia_male_n[[new_years[i]]] * (rate_2019$val ))/ 100000
}

##----------------------------------------1% up
rate_2019_u1 <- rate_2019 %>% as.data.frame()

new_years <- seq(2020, 2039, 1) %>% as.character()
for (i in 1:20){
  rate_2019_u1[, new_years[i]] <- rate_2019_u1$val * (1.01 ^ (i - 1))
}

Asia_male_4 <- data.frame(year = rep(0, 20), number = rep(0, 20))

for (i in 1:20) {
  Asia_male_4$year[i] <- new_years[i]
  Asia_male_4$number[i] <- sum(GBD_Asia_male_n[[ new_years[i]]] * rate_2019_u1[[new_years[i]]]) / 100000
}

##-------------------------------------------1% down
rate_2019_u2 <- rate_2019 %>% as.data.frame()

new_years <- seq(2020, 2039, 1) %>% as.character()
for (i in 1:20){
  rate_2019_u2[, new_years[i]] <- rate_2019_u2$val * (0.99 ^ (i - 1))
}

Asia_male_5 <- data.frame(year = rep(0, 20), number = rep(0, 20))
for (i in 1:20) {
  Asia_male_5$year[i] <- new_years[i]
  Asia_male_5$number[i] <- sum(GBD_Asia_male_n[[ new_years[i]]] *rate_2019_u2[[new_years[i]]]) / 100000
}
#作图
Asia_male_4$number_up <- Asia_male_4$number
Asia_male_4_5 <- cbind(Asia_male_5, Asia_male_4[3])

ggplot() + 
  geom_line(data = Asia_male_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_male_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_male, aes(x = year, y = val)) 

##-----------------------------------------------2018-2042年间APC预测值
#方法3：以每个5年组的预测发病率和每年的人口情况计算该组的发病数
#5年率分成历年率
years <- colnames(male_age_rate)
temp <- NULL
for (i in 1:10) {
  temp <- append(temp, rep(years[i], 5))
}
male_age_rate<- male_age_rate[, temp]
colnames(male_age_rate) <- seq(1990, 2039, 1) %>% as.character()
new_years <- colnames(male_age_rate)
#对应每年总共人口
Asia_pop <- GBD_Asia_male_n
#计算发病人数
Asia_male_pre_3 <- data.frame(year = rep(0, 50), number = rep(0, 50), se = seq(1:50))
for (i in 1:50) {
  Asia_male_pre_3$year[i] <- new_years[i]
  Asia_male_pre_3$number[i] <- sum(Asia_pop[[ new_years[i]]] * male_age_rate[[new_years[i]]]) / 100000
}                            
Asia_male_pre_3$new_year <- seq(1990, 2039, 1)
#绘图
showtext_auto(enable = TRUE)

plot_male <- ggplot() + 
  geom_line(data = Asia_male_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#00298a") +
  geom_ribbon(data = Asia_male_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#9cdeeb",color ="#9cd1dd", alpha = 0.4) +
  geom_point(data = EC_male, aes(x = year, y = val), color = "#2a519f") +
  geom_line(data=Asia_male_pre_3, aes(x = new_year, y = number), color="#ecbece", size=0.8) + 
  geom_point(data=Asia_male_pre_3, aes(x = new_year, y = number), color="#b71a66", size=1) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 18, family = "Arial", colour = "black"),  
    axis.text.y = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.x = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.y = element_text(size = 18, family = "Arial", colour = "black"),
    plot.title = element_text(size = 18, family = "Arial", colour = "black")
  ) +
  scale_y_continuous(breaks = seq(30000, 200000, by = 50000), limits = c(30000, 200000)) +
  labs(x = "Year", y = "Numbers of Deaths cases") # 设置轴标题

plot_male
##方法1：以每个5年组的预测发病率和该5年组的平均人口情况计算该组的发病数，点落在中位数年份
#五年率
result <- round(nordpred.getpred(male_res, incidence = TRUE, standpop = NULL), 2) 
#5年组的平均人口
Asia_annual <- GBD_Asia_male_g/5
Asia_annual$age_names <- rownames(Asia_annual)
years <- colnames(Asia_annual)
#历年发病数
Asia_male_pre_1 <- data.frame(year = rep(0, 10), number = rep(0, 10), se = seq(1:10))
for (i in 1:10) {
  Asia_male_pre_1$year[i] <- years[i]
  Asia_male_pre_1$number[i] <- sum(Asia_annual[, years[i]] * result[, years[i]]) / 100000
}
male_sum_year$new_year <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)
Asia_male_pre_1$new_year<- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)
#绘图：
ggplot() + 
  geom_line(data = Asia_male_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_male_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_male, aes(x = year, y = val))  + 
  geom_line(data = Asia_male_pre_1, aes(x = as.numeric(new_year), y = number), color = 'red')
##方法2：以每个5年组的预测发病率和该5年组的平均人口情况计算该组的发病数，点落在每组的每个年份上。
#五年发病人数变历年发病人数
Asia_male_years <- NULL
for(i in 1:10){
  Asia_male_years <- append(Asia_male_years, rep(Asia_male_pre_1 $number[i], 5))
}
# Now create a separate data frame for the new years and numbers
Asia_male_years<- data.frame(year = seq(1990,2039,1), number = Asia_male_years)
#绘图
ggplot() + 
  geom_line(data = Asia_male_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_male_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_male, aes(x = year, y = val))  + 
  geom_line(data=Asia_male_pre_3,aes(x = new_year, y = number),color="green")+ geom_point()+
  geom_line(data = Asia_male_pre_1, aes(x = as.numeric(new_year), y = number), color = 'red')+
  geom_line(data = Asia_male_years, aes(x = year, y =number),color="blue")

###-----------------------------------------------------------------------female 
##------------------1990-2017年之间观察到的死亡人数（1990-2019图）
EC_female <- subset(EC,age=="All Ages" & 
                      sex == 'Female' &
                      metric == 'Number' &
                      measure == 'Deaths' &
                      location== 'Asia')[,c(7,8)]

ggplot(data = EC_female,aes(x = year, y = val)) + geom_point()
##--------------2019年的各年龄组发病率（2019预测2039）
#number
EC_female_Deaths <- subset(EC,age %in% ages & 
                             sex == 'Female' &
                             metric == 'Number' &
                             measure == 'Deaths' &
                             location== 'Asia')[,c(3,4,7,8)]
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

rate_2019 <- EC_female_Deaths_n[,30]
rate_2019 <- data.frame(rate_2019)
colnames(rate_2019)[1] <- "number"
rownames(rate_2019) <- c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29",
                         "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                         "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")

##population
rownames(GBD_Asia_female_n) <- c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
                                 "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
                                 "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")


pop_2019<- GBD_Asia_female_n %>% as.data.table()
pop_2019 <-  pop_2019[, .(age_name = c("0 to 4", "5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
                                       "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",        
                                       "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus"), 
                          val = pop_2019$'2019')]

rate_2019$val <- 100000 * rate_2019$number/pop_2019$val


new_years <- seq(2020, 2039, 1) %>% as.character()

Asia_female_3 <- data.frame(year = new_years, number = rep(0, 20))

for (i in 1:20) {
  Asia_female_3$number[i] <- sum(GBD_Asia_female_n[[new_years[i]]] * (rate_2019$val ))/ 100000
}

##--------------------------------------------------------1% up
rate_2019_u1 <- rate_2019 %>% as.data.frame()

new_years <- seq(2020, 2039, 1) %>% as.character()
for (i in 1:20){
  rate_2019_u1[, new_years[i]] <- rate_2019_u1$val * (1.01 ^ (i - 1))
}

Asia_female_4 <- data.frame(year = rep(0, 20), number = rep(0, 20))

for (i in 1:20) {
  Asia_female_4$year[i] <- new_years[i]
  Asia_female_4$number[i] <- sum(GBD_Asia_female_n[[ new_years[i]]] * rate_2019_u1[[new_years[i]]]) / 100000
}

##--------------------------------------------------------1% down
rate_2019_u2 <- rate_2019 %>% as.data.frame()

new_years <- seq(2020, 2039, 1) %>% as.character()
for (i in 1:20){
  rate_2019_u2[, new_years[i]] <- rate_2019_u2$val * (0.99 ^ (i - 1))
}

Asia_female_5 <- data.frame(year = rep(0, 20), number = rep(0, 20))
for (i in 1:20) {
  Asia_female_5$year[i] <- new_years[i]
  Asia_female_5$number[i] <- sum(GBD_Asia_female_n[[ new_years[i]]] *rate_2019_u2[[new_years[i]]]) / 100000
}
##作图
Asia_female_4$number_up <- Asia_female_4$number
Asia_female_4_5 <- cbind(Asia_female_5, Asia_female_4[3])

ggplot() + 
  geom_line(data = Asia_female_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_female_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_female, aes(x = year, y = val)) 

##-----------------------------------------------2018-2042年间APC预测值
#方法3：以每个5年组的预测发病率和每年的人口情况计算该组的发病数
#5年率分成历年率
years <- colnames(female_age_rate)
temp <- NULL
for (i in 1:10) {
  temp <- append(temp, rep(years[i], 5))
}
female_age_rate<- female_age_rate[, temp]
colnames(female_age_rate) <- seq(1990, 2039, 1) %>% as.character()
new_years <- colnames(female_age_rate)

Asia_pop <- GBD_Asia_female_n
Asia_female_pre_3 <- data.frame(year = rep(0, 50), number = rep(0, 50), se = seq(1:50))
for (i in 1:50) {
  Asia_female_pre_3$year[i] <- new_years[i]
  Asia_female_pre_3$number[i] <- sum(Asia_pop[[ new_years[i]]] * female_age_rate[[new_years[i]]]) / 100000
}                            
Asia_female_pre_3$new_year <- seq(1990, 2039, 1)

#绘图
showtext_auto(enable = TRUE)

plot_female <- ggplot() + 
  geom_line(data = Asia_female_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#00298a") +
  geom_ribbon(data = Asia_female_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#9cdeeb",color ="#9cd1dd", alpha = 0.4) +
  geom_point(data = EC_female, aes(x = year, y = val), color = "#2a519f") +
  geom_line(data=Asia_female_pre_3, aes(x = new_year, y = number), color="#ecbece", size=0.8) + 
  geom_point(data=Asia_female_pre_3, aes(x = new_year, y = number), color="#b71a66", size=1) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 18, family = "Arial", colour = "black"),  
    axis.text.y = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.x = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.y = element_text(size = 18, family = "Arial", colour = "black"),
    plot.title = element_text(size = 18, family = "Arial", colour = "black")
  ) +
  scale_y_continuous(breaks = seq(10000, 70000, by = 10000), limits = c(1500, 70000)) +
  labs(x = "Year", y = "Numbers of Deaths cases") # 设置轴标题

plot_female



##方法1：以每个5年组的预测发病率和该5年组的平均人口情况计算该组的发病数，点落在中位数年份
result <- round(nordpred.getpred(female_res, incidence= TRUE, standpop = NULL), 2) 
Asia_annual <- GBD_Asia_female_g/5
Asia_annual$age_names <- rownames(Asia_annual)

years <- colnames(Asia_annual)
Asia_female_pre_1 <- data.frame(year = rep(0, 10), number = rep(0, 10), se = seq(1:10))

for (i in 1:10) {
  Asia_female_pre_1$year[i] <- years[i]
  Asia_female_pre_1$number[i] <- sum(Asia_annual[, years[i]] * result[, years[i]]) / 100000
}
female_sum_year$new_year <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)

Asia_female_pre_1$new_year<- c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)
ggplot() + 
  geom_line(data = Asia_female_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_female_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_female, aes(x = year, y = val))  + 
  geom_line(data = Asia_female_pre_1, aes(x = as.numeric(new_year), y = number), color = 'red')
##方法2：以每个5年组的预测发病率和该5年组的平均人口情况计算该组的发病数，点落在每组的每个年份上。
Asia_female_years <- NULL
for(i in 1:10){
  Asia_female_years <- append(Asia_female_years, rep(Asia_female_pre_1 $number[i], 5))
}
# Now create a separate data frame for the new years and numbers
Asia_female_years<- data.frame(year = seq(1990,2039,1), number = Asia_female_years)

ggplot(data = Asia_female_years, aes(x = year, y =number)) + geom_line()    

ggplot() + 
  geom_line(data = Asia_female_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_female_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_female, aes(x = year, y = val))  + 
  geom_line(data=Asia_female_pre_3,aes(x = new_year, y = number),color="green")+ geom_point()+
  geom_line(data = Asia_female_pre_1, aes(x = as.numeric(new_year), y = number), color = 'red')+
  geom_line(data = Asia_female_years, aes(x = year, y =number),color="blue")

###-------------------------------------------------------------------both ±1%对比图
library(dplyr)
library(reshape2)
library(data.table)
#点图
rbind_EC_both<- rbind (EC_male,EC_female )
EC_both <- rbind_EC_both %>%
  group_by(year) %>%
  summarise(val = sum(val, na.rm = TRUE))
ggplot(data =EC_both,aes(x = year, y = val)) + geom_point()

rind_Asia_both_3<- rbind (Asia_male_3 ,Asia_female_3)
Asia_both_3 <- rind_Asia_both_3 %>%
  group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

#±1%
rind_Asia_both_4<- rbind (Asia_male_4 ,Asia_female_4)
Asia_both_4 <- rind_Asia_both_4 %>%
  group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

rind_Asia_both_5<- rbind (Asia_male_5 ,Asia_female_5)
Asia_both_5 <- rind_Asia_both_5 %>%
  group_by(year) %>%
  summarise(number = sum(number, na.rm = TRUE))

Asia_both_4$number_up <- Asia_both_4$number
Asia_both_4_5 <- cbind(Asia_both_5, Asia_both_4[3])

ggplot() + 
  geom_line(data = Asia_both_3, aes(x = as.numeric(year), y = number), linetype = 'dashed') +
  geom_ribbon(data = Asia_both_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), alpha = 0.4) +
  geom_point(data = EC_both, aes(x = year, y = val)) 
# prediciton
rbind_Asia_both_pre_3 <- rbind(Asia_male_pre_3, Asia_female_pre_3)
Asia_both_pre_3 <- rbind_Asia_both_pre_3 %>%
  group_by(new_year) %>%
  summarise(number = sum(number, na.rm = TRUE))
#绘图
showtext_auto(enable = TRUE)

plot_both <- ggplot() + 
  geom_line(data = Asia_both_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#00298a") +
  geom_ribbon(data = Asia_both_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#9cdeeb",color ="#9cd1dd", alpha = 0.4) +
  geom_point(data = EC_both, aes(x = year, y = val), color = "#2a519f") +
  geom_line(data=Asia_both_pre_3, aes(x = new_year, y = number), color="#ecbece", size=0.8) + 
  geom_point(data=Asia_both_pre_3, aes(x = new_year, y = number), color="#b71a66", size=1) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 18, family = "Arial", colour = "black"),  
    axis.text.y = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.x = element_text(size = 18, family = "Arial", colour = "black"), 
    axis.title.y = element_text(size = 18, family = "Arial", colour = "black"),
    plot.title = element_text(size = 18, family = "Arial", colour = "black")
  ) +
  scale_y_continuous(breaks = seq(30000, 280000, by = 50000), limits = c(30000, 280000)) +
  labs(x = "Year", y = "Numbers of Deaths cases") # 设置轴标题

plot_both
###最后汇总图（男、女，both）
#rbind
plot_Deaths <- ggplot() + 
  geom_line(data = Asia_male_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#52bce1", size=0.9) +
  geom_ribbon(data = Asia_male_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#96dafa", color ="#96dafa", alpha = 0.2) +
  geom_point(data = EC_male, aes(x = year, y = val), color = "#52bce1", size=2.5) +
  geom_line(data=Asia_male_pre_3, aes(x = new_year, y = number), color="#96dafa", size=2, alpha=0.8) +  
  
  geom_line(data = Asia_female_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#6f55c4", size=0.9) +
  geom_ribbon(data = Asia_female_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#cbb6ff", color ="#cbb6ff", alpha = 0.2) +
  geom_point(data = EC_female, aes(x = year, y = val), color = "#6f55c4", size=2.5) +
  geom_line(data=Asia_female_pre_3, aes(x = new_year, y = number), color="#cbb6ff", size=2, alpha=0.8) + 
  
  geom_line(data = Asia_both_3, aes(x = as.numeric(year), y = number), linetype = 'dashed', color = "#577cb1", size=0.9) +
  geom_ribbon(data = Asia_both_4_5, aes(x = as.numeric(year), ymin = number, ymax = number_up), fill = "#7c93bd", color ="#7c93bd", alpha = 0.2) +
  geom_point(data = EC_both, aes(x = year, y = val), color = "#577cb1", size=2.5) +
  geom_line(data=Asia_both_pre_3, aes(x = new_year, y = number), color="#7c93bd", size=2, alpha=0.8) + 
  
  geom_vline(xintercept = as.numeric(2019), linetype = "dashed", color = "black") + # 添加2019年的虚线
  
  theme_minimal(base_size = 12) + 
  theme(
    panel.background = element_blank(), # 设置图形背景透明
    panel.grid.major = element_line(), # 设置主要辅助线为浅灰色
    panel.grid.minor = element_line(), # 设置次要辅助线为点线
    axis.text.x = element_text(size = 20, family = "Arial", colour = "black"),  
    axis.text.y = element_text(size = 20, family = "Arial", colour = "black"), 
    axis.title.x = element_text(size = 22, family = "Arial", colour = "black", face = "bold"), 
    axis.title.y = element_text(size = 22, family = "Arial", colour = "black", face = "bold"),
    plot.title = element_text(size = 20, family = "Arial", colour = "black")
  ) +
  scale_y_continuous(breaks = seq(3000, 110000, by = 20000), limits = c(3000, 110000)) +
  labs(x = "Year", y = "Numbers of Deaths") # 设置轴标题

plot_Deaths


