read_spec_data <- function(x) {
    # чтение данных
    data <- read_delim(x, ";", 
                       escape_double = FALSE, trim_ws = TRUE, 
                       skip = 2, col_types = cols(`14` = col_skip()), 
                       locale = locale(encoding = "UTF-8"))
    colnames(data)[colnames(data) == "14_1"] <- "14"
    
    # "транспонирование" таблицы
    data <- as_tibble(t(data[, -1]), 
                      .name_repair = ~data[[1]])
    data <- tibble("Дни" = rownames(data)[-nrow(data)], data[-nrow(data), ])
    
    # чтение и очистка метаданных с помощью stringr
    meta_data <- read_lines(x, skip = 1, n_max = 1)
    meta_data <- str_split(meta_data, pattern = ";")[[1]]
    meta_data <- unique(str_remove_all(meta_data, "[\"]"))[-1]
    meta_data <- na_if(meta_data, "NA")
    
    # подсоединение к полученному датафрейму готовых метаданных    
    data$`Организация отдыха` <- meta_data[2]
    data$`Вид отдыха` <- meta_data[4]
    data$`Номер заезда/смены` <- meta_data[6]
    data$`Период заезда` <- meta_data[8]
    
    # видимо, одна из организаций не провела смену, и поэтому ее данные пустые.
    # специально для такого случая напишем условие:
    if(any(str_detect(data$`Дни`, "[\\_]"))) {
        data <- data[c(1, 5),]
        return(data)
    } else {
        return(data) 
    }
}
