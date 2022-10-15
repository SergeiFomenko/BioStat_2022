library(dplyr)
library(ggplot2)
library(DAAG)
path_to_file <- 'mc_donalds_data.csv'
mc_menu_data <- read.csv(path_to_file,
                         header = TRUE, sep = ',', encoding = 'UTF-8')

### TASK 1 

#1.1 Отобрать все данные по категории "Snacks & Sides" (2 балла)
mc_snacks <- mc_menu_data [mc_menu_data$Category=="Snacks & Sides",]

#1.2 Отобрать все данные по категориям "Desserts" и "Salads" (2 балла)
salas_dessert <- mc_menu_data [mc_menu_data$Category=="Desserts" | mc_menu_data$Category=="Salads",]

#1.3 Отобрат все данные кроме категории "Breakfast" (2 балла)
no_breakfast <- mc_menu_data [mc_menu_data$Category!="Breakfast",]

#1.4 Посчитать число блюд, где калорийность больше чем 500 (2 балла)
mc_menu_data[is.na(mc_menu_data)] <- 0
sum((mc_menu_data$Calories)>500*1)

#1.5 Найти самое калорийное блюдо (2 балла)
max_cal <- max(mc_menu_data$Calories)
mc_menu_data[mc_menu_data$Calories==max_cal,]$Item


### TASK 2 

#2.1 Найдите число блюд в каждой из категорий, для которых витаминная ценность равна нулю (2 балла)
select(md,contains('Vitamin')) %>% filter_all(all_vars(.==0)) %>% count

#2.2 Посчитайте долю калорий, приходящихся на жиры, для блюд из завтраков и округлите значения до 3 знака (2 балла)
filter(md,Category=='Breakfast') %>% mutate(fat_proportion = round(Calories.from.Fat / Calories,3))

#2.3 Посчитайте среднее значение, медиану и разницу между ними для холестерола (2 балла)
summarise(md, Chol_mean = mean(Cholesterol), Chol_median = median(Cholesterol), mean_median_diff = mean(Cholesterol) - median(Cholesterol))

#2.4 Для каждой категории найдите блюдо с самым высоким отношением сахаров к углеводам (2 балла)
group_by(md,Category) %>% summarise(max(Sugars/Carbohydrates,na.rm = T))

data("leafshape")
leafshape$arch <- factor(leafshape$arch, labels = c("Plagiotropic", "Orthotropic"))

### TASK 3

#3.1 Распределение (boxplot) длины листа, в зависимости от локации и архитектуры (5 баллов)
ggplot(leafshape, aes(x = location, y = bladelen, color = factor(arch))) + 
  geom_boxplot() + 
  labs(x = "Location", y = "Length", title = "Leaf length distribution in different locations", color = "Architecture") + 
  theme_bw(16) 


# Больше графиков богу графиков
#3.2 Сложный график -- violin plot, с разделением по архитекутре листьев (5 баллов)
ggplot(leafshape, aes(x = factor(arch), y = bladelen)) + 
  geom_violin() + 
  facet_grid(vars(location)) + 
  xlab("Leaf architecture") + 
  ylab("Leaf length")
