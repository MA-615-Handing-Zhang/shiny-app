##  for data info see
##  https://quickstats.nass.usda.gov/

##  Note: CV Coefficient of variation. Available for the 2012 Census of Agriculture
##        only. County-level CVs are generalized.

library(tidyverse)
library(magrittr)

#############################################################
## load data

strawb <- read.csv("Strawberries.csv")

#############################################################
## Drop the no-info columns

#去掉没有信息的列

drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

#############################################################

#把"Data.Item"分成4个部分

strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

## explore

#看看straw里的"Strawberries", "items", "discription", "units"这几列有哪些类信息

distinct(strawb, Strawberries)
distinct(strawb, items)
distinct(strawb, discription)
distinct(strawb, units)
?distinct



#############################################################
## Separate Domain into 2 columns

#把"Domain"分成2个部分

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

distinct(strawb, dname)
distinct(strawb, type)

#############################################################
## make a copy of Domain.Category

#创建"Chemicals",内容等于"Domain.Category"

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 

## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals

#把"Chemicals"里写了"CHEM"的挑出来,然后看有多少个

bb <- strawb$Chemicals %>% str_detect("CHEM")
bb
sum(bb)
?str_detect


## index 

#dim(x)讲解：http://www.stat.umn.edu/macanova/htmlhelp/node95.htm
#dim(x)[1]是行数，dim(x)[2]是列数

ind_C <- (!bb)*(1:dim(strawb)[1])

#去掉ind_C中的0
#r1即为chemical 列中不含字符串“CHEM”的项的index
r1 <- ind_C[ind_C > 0]
## set entries in Chemicals column to " " if they don't start with CHEM

#将"Chemicals"里没有写"CHEM"的项变成空的

strawb$Chemicals[r1] <- " "

strawb$Chemicals[r1]

#############################################################
## now we need a list of chemicals

#把"Chemicals"分成2个部分

# This line of code change the setting, helping us skip any entry that cannot be translated.
# https://stackoverflow.com/questions/4993837/r-invalid-multibyte-string
Sys.setlocale("LC_ALL", "C")


strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = '[:]',
                     fill = "right",)






#str_trim()从字符串的开头和结尾删除空格
#str_squish()还减少了字符串内的重复空格
library(stringr)
?str_extract()

# ----------------------------------
distinct(strawb, details)
unique(str_trim(strawb$details))
unique(strawb$details)
unique(str_extract(str_trim(strawb$details) ,"[^(].*[^)]")) 
#其中[^(]意思是以(开头的 .的意思不明 *意思是重复此操作多次 [^)]意思是匹配)
# 见10:03   https://www.youtube.com/watch?v=PcUJXrN-C_E&ab_channel=RProgramming

unique(str_extract(str_trim(strawb$details) ,"[^(].+[^)]"))
# 这种方式也可取出括号中的内容
# https://stackoverflow.com/questions/38231081/extract-info-inside-parenthesis-in-r
unique(str_match(str_trim(strawb$details), "\\((.*)\\)"))

# -----------------------------------

# 取出detail列的括号中的内容(除去括号)，原理见上
strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )
# 除掉type列的空格
strawb %<>% mutate(type = str_trim(type))


distinct(strawb, details)
distinct(strawb, type)


#############################################################
## Note that the 3021 rows in strawb have been spit into three subsets here
## But, be careful -- the columns are sill not sorted out
## 根据化学类型（type列）把数据分成了四个类别
strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))

strawb_other <- strawb %>% filter(type=="OTHER")

strawb_na <- strawb %>% filter(is.na(type)==TRUE)

#############################################################
## look at herbicides in particular

strawb_herb <- strawb %>% 
  filter(type=="HERBICIDE")

distinct(strawb_herb, details)

## more exploration

distinct(strawb_chem, details)
distinct(strawb_other, details)
distinct(strawb_na, details)

#############################################################
#############################################################
## explore California

strawb_chem <- drop_no_info_cols(strawb_chem)
strawb_other <- drop_no_info_cols(strawb_other)
names(strawb_chem)
names(strawb)

chem_list <- distinct(strawb_chem, details)

chem_list <- rbind2(chem_list,distinct(strawb_other, details))

strawb_california_2020 <- strawb %>% filter((State == "CALIFORNIA")&(Year==2020))

## aa <- strawb$Year[4]
## GLUFOSINATE-AMMONIUM = 128850

strawb__2019_Californi_GLUFOSINATE_AMMONIUM <- strawb_herb %>% filter((State=="CALIFORNIA")&
                                                                        (Year==2019)&
                                                                        (details=="GLUFOSINATE-AMMONIUM = 128850"))

## this table is full of repetition and multiple related measurements!!

#############################################################
### let's look at the other states

distinct(strawb_chem, State)

#############################################################
#############################################################
### focus on Florida

strawb_chem_FLORIDA_2020 <- strawb_chem %>% 
  filter((State == "FLORIDA")&(Year==2020))


head(strawb_chem_FLORIDA_2020)

## there's nothing in strawb_chem_FLORIDA_2020 -- California was the same way
## probably drop year 2020

strawb_chem_FLORIDA_2019 <- strawb_chem %>% 
  filter((State == "FLORIDA")&(Year==2019))

head(strawb_chem_FLORIDA_2019)

strawb_herb_FLORIDA_2019  <- strawb_herb %>% 
  filter((State == "FLORIDA")&(Year==2019))

head(strawb_herb_FLORIDA_2019)
## get rid of as much redundant data as possible

strawb_herb_FLORIDA_2019 <- drop_no_info_cols(strawb_herb_FLORIDA_2019)
head(strawb_herb_FLORIDA_2019)
str(strawb_herb_FLORIDA_2019)


## this is a table for Florida Strawberries Herbicides 2019
## get rid of the redundant Domain.Category column

strawb_herb_FLORIDA_2019 %<>% 
  select(-Domain.Category)




head(strawb_herb_FLORIDA_2019)

# strawb_herb_FLORIDA_2019_a <- straw_h %>% 
#   pivot_wider()

## Note that there is still a lot of clean-up left to do -- 
## which could have been done earlier in the larger table
## but we'll do it here 
## also note that there is no data available -- except for the identity
## of the herbicides used

strawb_herb_FLORIDA_2019 %<>% separate(col = items,
                                       into = c("it", "what"), 
                                       sep = "-",
                                       fill = "right")


## drop column it

strawb_herb_FLORIDA_2019 %<>%
  select(-it)

## relocate details to the first column

strawb_herb_FLORIDA_2019 %<>%  
  relocate(details, .before = what)

head(strawb_herb_FLORIDA_2019)

## rename "details column to "herbicides"
strawb_herb_FLORIDA_2019 %<>% 
  rename(herbicides = details)

## Now you can clearly see that this is a small table of 5 herbicides measured in 5 ways
## use pivot to make your table wider.  

## This R script has taken you down a path from the large table about strawberries
## to this table about herbicide application in Florida.

## Now, you're ready to go back to the start and deliver your exploration of the entire dataset.
## You can get tables for insecticides, herbicides, fungicides, and fertilizers for each state.

## You don't know the toxicity for each chemical, but you have some examples of both dangerous
## chemicals and benign chemicals.  

## you also don't have CV.  the NASS system has indicated the CV is only available 
## for the year 2021,  

#############################################################
#############################################################
pesticides <- read_csv("Pesticides(1).csv")
pesticides

strawb_select <- select(strawb, Year, State, items, discription, units, dname, type, details, Value)
strawb_select
names(strawb_select)
head(strawb_select)

#去除"Pesticide"和"details"的NA项
pesticides <- pesticides %>% 
  filter(!is.na(Pesticide))
 
strawb_select <- strawb_select %>% 
  filter(!is.na(details))


strawb_select %<>% separate(col = details, 
                            into = c("chemical_name", "chemical_id"), 
                            sep = "=", 
                            fill = "right")
head(strawb_select)


#使"Pesticide"变成大写，这样可以match数据集strawb里的大写


pesticides <- pesticides %>% 
  mutate(Pesticide = toupper(Pesticide))

head(pesticides$Pesticide)

#trimws：删除前导/尾随空格
strawb_select <- strawb_select %>% 
  mutate(chemical_name = str_trim(chemical_name))

# view(strawb_select$chemical_name)

# strawb_select <- mutate(strawb_select, chemical_name = trimws(chemical_name))

# distinct(strawb_select, chemical_name)
# view(distinct(pesticides, Pesticide))


#-----------------------------------------------------------------
#joined <- inner_join(strawb_select, pesticides, 
#                     by = c("chemical_name" = "Pesticide"))

#--------------------------------------------------------------------


pesticides1 <- pesticides %>% 
  rename(chemical_name = Pesticide)
# install.packages("prob")
# library(prob)
## Since inner_join drops all observations that do not match, I choose left_join here.
joined2 <- left_join(strawb_select, pesticides1,
                     by = "chemical_name")

library(dplyr)
# detach("package:stats")
# detach("timeSeries")

length(intersect(pesticides1$chemical_name, strawb_select$chemical_name))
length(unique(pesticides1$chemical_name))
length(unique(strawb_select$chemical_name))

head(pesticides1)
names(pesticides1)




#############################################################

strawb <- read.csv("Strawberries.csv")
joined2
unique(joined2$discription)


# 有几个州，几种计量单位。
distinct(joined2,State)
distinct(joined2, discription)

#按周subset数据
california <- joined2 %>% 
  filter(State == "CALIFORNIA")
florida <- joined2 %>% 
  filter(State == "FLORIDA")
oregon <- joined2 %>% 
  filter(State == "OREGON")
washington <- joined2 %>% 
  filter(State == "WASHINGTON")

# 按化学物质种类subset 加州的数据

cali_chem <- california %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))

flo_chem <- florida %>% filter((type=="FUNGICIDE")|
                                     (type=="HERBICIDE")|
                                     (type=="INSECTICIDE"))

ore_chem <- oregon %>% filter((type=="FUNGICIDE")|
                                     (type=="HERBICIDE")|
                                     (type=="INSECTICIDE"))

wash_chem <- washington %>% filter((type=="FUNGICIDE")|
                                     (type=="HERBICIDE")|
                                     (type=="INSECTICIDE"))






cali_other <- california %>% filter(type=="OTHER")

#no chemical
cali_na <- california %>% filter(is.na(type)==TRUE)


#其中， 含化学物质的按照年份，化学物质种类 计量单位分类。

cali_chem1 <- cali_chem %>% 
  group_by(Year, type, discription) %>% 
  summarise(count = n())


flo_chem1 <- flo_chem %>% 
  group_by(Year, type, discription) %>% 
  summarise(count = n())

ore_chem1 <- ore_chem %>% 
  group_by(Year, type, discription) %>% 
  summarise(count = n())

wash_chem1 <- wash_chem %>% 
  group_by(Year, type, discription) %>% 
  summarise(count = n())






# 我们发现1.加州2017和2020年数据缺失 2. 五种计量单位的使用频率差不多。

#讲value一栏转化为numeric
cali_chem$Value <- as.numeric(cali_chem$Value)
flo_chem$Value <- as.numeric(flo_chem$Value)
ore_chem$Value <- as.numeric(ore_chem$Value)
wash_chem$Value <- as.numeric(wash_chem$Value)




# 在分类总结中加入平均的value, 出现了错误。
# cali_chem1 <- cali_chem %>% 
#   group_by(Year, type, discription) %>% 
#   summarise(count = n(), value_sum = sum(Value))
# 
# flo_chem1 <- cali_chem %>%
#   group_by(Year, type, discription) %>%
#   summarise(count = n(), value_sum = sum(Value))
# 
# cali_chem1 <- cali_chem %>%
#   group_by(Year, type, discription) %>%
#   summarise(count = n(), value_sum = sum(Value))
# 
# cali_chem1 <- cali_chem %>%
#   group_by(Year, type, discription) %>%
#   summarise(count = n(), value_sum = sum(Value))






# 我们发现measured in lb明显大与其它计量单位的量。此处应查pdf关于计量单位的描述作出相应调整。



aa <- !is.na(cali_chem$`Bee Toxins`)
ind_C1 <- (!aa)*(1:dim(cali_chem)[1])
r2 <- ind_C1[ind_C1>0]
cali_chem$`Bee Toxins`[r2] <- "none"



cali_chem_summary <- cali_chem %>% 
  group_by(Year,`Bee Toxins`) %>% 
  summarise(total_usage = sum(Value, na.rm = T))


bb <- !is.na(flo_chem$`Bee Toxins`)
ind_C2 <- (!bb)*(1:dim(flo_chem)[1])
r3 <- ind_C2[ind_C2>0]
flo_chem$`Bee Toxins`[r3] <- "none"



flo_chem_summary <- flo_chem %>% 
  group_by(Year,`Bee Toxins`) %>% 
  summarise(total_usage = sum(Value, na.rm = T))


cc <- !is.na(ore_chem$`Bee Toxins`)
ind_C3 <- (!cc)*(1:dim(ore_chem)[1])
r4 <- ind_C3[ind_C3>0]
ore_chem$`Bee Toxins`[r4] <- "none"


ore_chem_summary <- ore_chem %>% 
  group_by(Year,`Bee Toxins`) %>% 
  summarise(total_usage = sum(Value, na.rm = T))



dd <- !is.na(wash_chem$`Bee Toxins`)
ind_C4 <- (!dd)*(1:dim(wash_chem)[1])
r5 <- ind_C4[ind_C4>0]
wash_chem$`Bee Toxins`[r5] <- "none"

wash_chem_summary <- wash_chem %>% 
  group_by(Year,`Bee Toxins`) %>% 
  summarise(total_usage = sum(Value, na.rm = T))




#尝试给slight 和 none换个顺序

yy <- factor(as.factor(cali_chem_summary$`Bee Toxins`), levels = c("none", "slight" , "moderate", "high"))

cali_chem_summary$`Bee Toxins` <- yy

cali_chem_summary_ordered <- cali_chem_summary %>% 
  arrange(Year,`Bee Toxins`)


# 算出每年总用量
cali_chem_summary1 <- cali_chem_summary %>% 
  group_by(Year) %>% 
  summarise(useageofyear = sum(total_usage))

flo_chem_summary <- flo_chem_summary %>% 
  group_by(Year) %>% 
  summarise(useageofyear = sum(total_usage))

ore_chem_summary <- ore_chem_summary %>% 
  group_by(Year) %>% 
  summarise(useageofyear = sum(total_usage))

wash_chem_summary <- wash_chem_summary %>% 
  group_by(Year) %>% 
  summarise(useageofyear = sum(total_usage))






#整理出每年的关于蜜蜂毒性的高中低无的使用量，百分比
perc_col = matrix(cbind(cali_chem_summary_ordered$total_usage[1:4] / 6046.092 , cali_chem_summary_ordered$total_usage[5:8] / 9177.122, cali_chem_summary_ordered$total_usage[9:12] / 8457.764), nrow = 12)
cali_chem_summary_ordered$percentage <- perc_col

# perc_col = matrix(cbind(flo_chem_summary$total_usage[1:4] / 6046.092 , cali_chem_summary_ordered$total_usage[5:8] / 9177.122, cali_chem_summary_ordered$total_usage[9:12] / 8457.764), nrow = 12)
# cali_chem_summary_ordered$percentage <- perc_col
# 
# 



# ggplot(cali_chem_summary) +
#  geom_histogram(aes(x = Year, y = percentage), stat = "identity", fill = Bee Toxins, width = 0.4)
 # geom_histogram(aes(x = ages, y = value), stat = "identity", fill = "blue", width = 0.5, position = "dodge")

ggplot(cali_chem_summary_ordered, aes(x = Year, y = percentage, fill = `Bee Toxins`)) +
  geom_col(position = "dodge")



#
#stacked plot
ggplot(cali_chem_summary_ordered, aes(fill = `Bee Toxins`, y = percentage, x = Year)) + 
  geom_bar(position="stack", stat="identity")

# About map. Shiny: scroller: year
#                color: f that maps percentage of useage in high level bee toxin chemicals in this state. use select input.
# make the stacked 





