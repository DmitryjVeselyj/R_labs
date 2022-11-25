load("Lab2/ExpImp.RData")
# first task

# 4. Используйте файл ExpImp.Rdata
# Создайте data.frame в котором содержится только информация по субъектам федерации.
# Добавьте в него колонки, в которых содержится суммарный экспорт и импорт по субъектам федерации.
# Напишите функцию, которая в полученном Вами data.frame находит субъекты федерации в которых суммарные экспорт превышает суммарный импорт.

fix_data <- function(df) {
    change <- function(x) {
        vec <- as.numeric(gsub("-", "0", x))
        ifelse(is.na(vec), x, vec)
    }
    data.frame(lapply(df, change))
}

export_ge_import <- function(df) {
    df[which(df$СуммЭкспорт > df$СуммИмпорт), ]$Регион
}

df <- subset(ExpImp, !grepl("федеральный округ|Федерация|в том числе", Регион))
new_df <- fix_data(df)
new_df$СуммЭкспорт <- rowSums(new_df[, grep("Экспорт", colnames(new_df))])
new_df$СуммИмпорт <- rowSums(new_df[, grep("Импорт", colnames(new_df))])
print(export_ge_import(new_df))

# # second task
# 19. Используйте файл Payment_and_value_of_Care-Hospital.csv
# Напишите функцию, которая на вход принимает название города
# и название медицинской процедуры/заболевания,а на выход выдает
# именованный список,
# в котором в качестве имен используется название больницы,
# в которой можно получить медицинскую помощь,
# а качестве значений – усредненная стоимость услуги в данной больнице.
# Список должен быть упорядочен по возрастанию значения стоимости услуги.

get_named_list <- function(df, city, measure_name) {
    tmp_df <- subset(df, City == city & Payment.Measure.Name == paste("Payment for", measure_name, "patients"))
    tmp_df$Payment <- as.numeric(gsub(",", "", substr(tmp_df$Payment, start = 2, stop = nchar(tmp_df$Payment))))
    another <- aggregate(tmp_df$Payment, list(tmp_df$Facility.Name), FUN = mean)
    another <- split(another$x, another$Group.1)
    sorted_lst <- another[order(unlist(another), decreasing = FALSE)]

    return(sorted_lst)
}
df <- read.csv("Lab2/Payment_and_Value_of_Care-Hospital.csv")

print(get_named_list(df, "LOS ANGELES", "pneumonia"))
# tmp_df <-df[df$City == city & df$Payment.Measure.Name == paste("Payment for", measure_name, "patients"), ]
# tmp_df$Lower.Estimate <- as.numeric(gsub(",", "", substr(tmp_df$Lower.Estimate, start = 2, stop = nchar(tmp_df$Lower.Estimate))))
# tmp_df$Higher.Estimate <- as.numeric(gsub(",", "", substr(tmp_df$Higher.Estimate, start = 2, stop = nchar(tmp_df$Higher.Estimate))))
# another <- split( (tmp_df$Lower.Estimate + tmp_df$Higher.Estimate)/2, tmp_df$Facility.Name)
# sorted_another <- another[order(unlist(another), decreasing = FALSE)]
# print(sorted_another)
