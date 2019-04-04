library("tidyverse")  #целая вселенная
library("readr")      #функция read_csv()
library("stringr")    #функция str_replace_all
library("dplyr")      #функции: filter(); arrange(); select(); mutate(); summarize(); group_by(); sample_n()
library("ggplot2")    #Графики функцией qplot()

#Считываем файл онлайн
#Или считываем файл офлайн. 
#При этом пропускаем первую строку, заменяем все не числовые значения на NA, и игнорируем строчки с символом "["
#eddypro = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
setwd("C:/Users/Борис/Desktop/Учеба/R/MathMod Shamrai/MathMod")
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))

#
# Блок подготовки данных
#

#Удаляем ненужную пустую первую строку
eddypro = eddypro[-1, ]
#Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro, -(roll))
#Преобразуем в факторы (factor) столбы типа char (символ)
eddypro = eddypro %>% mutate_if(is.character, factor)
#Заменим спец. символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_") #проверка ниже выдала ошибку, т.к. появилась переменная начинающаяся с "_z", а символ "_" в начале переменной недопустим. По этому сделаем замену например на "L_"
#Возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)

#Удалим строки в которых содержится NA, так как они содержат неполные данные и только мешают
eddypro = drop_na(eddypro)
#Отфильтруем по заданию данные только за весенний период. С начала марта (60 день) по конец мая (151 день)
eddypro = filter(eddypro, DOY >= 60 & DOY < 152)
#Отфильтруем данные по заданию только за ночное время
eddypro = filter(eddypro, daytime == FALSE)
#Получим таблицу, состоящую только из чисел. Будем работать с ней
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#Получим таблицу содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

#
# Блок создания модели регрессионого анализа
#

#Создадим обучающую и тестирующую непересекающиеся выборки с помошью базового функционала для обучения и тестирования моделей
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

#Создадим Модель 1, добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)
#Получим информацию о моделе и коэффициенты
summary(mod1)
#Проанализируем переменные по значимости
anova(mod1)
#Выведем графики
plot(mod1) 

#Создадим Модель 2, добавив в нее значимые переменные из результатов функции anova() (со значимость до 0.01, соответсвенно ***, ** и * пометки)
mod2 = lm(co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux + rand_err_co2_flux + h2o_flux
          + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + air_temperature + air_pressure
          + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + RH + Tdew + u_unrot + v_unrot + w_unrot
          + v_rot + w_rot + wind_dir + yaw + pitch + TKE + L + L_z_minus_dL__div_L + bowen_ratio + T_star_ + x_peak + x_offset + x_10_perc_
          + x_30_perc_ + x_50_perc_ + x_70_perc_ + un_Tau + H_scf + un_LE + un_co2_flux + un_h2o_flux + co2_spikes + co2_1
          , data = teaching_tbl)
#Получим информацию о моделе и коэффициенты
summary(mod2)
#Проанализируем переменные по значимости
anova(mod2)
#Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod2, mod1)
#Выведем графики
plot(mod2) 

#Создадим Модель 3, повторив отбрасывание
mod3 = lm(co2_flux~ DOY + Tau + qc_Tau + qc_H + rand_err_H + qc_LE + rand_err_co2_flux + h2o_flux
          + H_strg + co2_v_minus_adv + co2_molar_density + h2o_molar_density + h2o_time_lag + air_pressure
          + u_unrot + v_unrot + w_unrot + v_rot + yaw + bowen_ratio + T_star_ + x_peak + un_co2_flux
          , data = teaching_tbl)
#Получим информацию о моделе и коэффициенты
summary(mod3)
#Проанализируем переменные по значимости
anova(mod3)
#Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod3, mod2)
#Выведем графики
plot(mod3) 

#Проведем корреляционный анализ переменных
#Выберем из таблицы только участвующие у линейной моделе переменные
cor_teaching_tbl = select(teaching_tbl, co2_flux, DOY, Tau, qc_Tau, qc_H, rand_err_H, qc_LE, rand_err_co2_flux, h2o_flux,
                          H_strg, co2_v_minus_adv, co2_molar_density, h2o_molar_density, h2o_time_lag, air_pressure,
                          u_unrot, v_unrot, w_unrot, v_rot, yaw, bowen_ratio, T_star_, x_peak, un_co2_flux)
#Получаем таблицу коэффициентов корреляций. И подправляем модель 3, убирая из модели одну из двух коррелирующих между собой переменных (начиная от коэффициента >= 0.7)
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#
# Графики по полученной моделе
#

#Проверка модели
#Построим точки co2_flux от co2_flux на значениях ОБУЧАЮЩЕЙ выборки. Наложим предсказанные значения по модели 3 на ОБУЧАЮЩЕЙ выборке сверху в виде линии
#В идеале линия должна  пройти через все точки. А так как у нас график co2_flux от самой себя, то он должен идти под 45градусов
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Теперь сделаем тоже самое на ТЕСТИРУЮЩЕЙ выборе
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

#Так как у нас модель зависит от множества переменных, мы можем вывести много графиков зависимостей co2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке
#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))