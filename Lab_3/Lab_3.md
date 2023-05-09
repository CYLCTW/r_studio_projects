### Подключение нужных пакетов

    library(arrow)

    ## 
    ## Присоединяю пакет: 'arrow'

    ## Следующий объект скрыт от 'package:utils':
    ## 
    ##     timestamp

    library(dplyr)

    ## 
    ## Присоединяю пакет: 'dplyr'

    ## Следующие объекты скрыты от 'package:stats':
    ## 
    ##     filter, lag

    ## Следующие объекты скрыты от 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(stringr)
    library(lubridate)

    ## 
    ## Присоединяю пакет: 'lubridate'

    ## Следующий объект скрыт от 'package:arrow':
    ## 
    ##     duration

    ## Следующие объекты скрыты от 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    library(ggplot2)

### Импорт датасета

    dataset <- arrow::read_csv_arrow("D:\\Lab_RStudio\\gowiththeflow_20190826\\gowiththeflow_20190826.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

# Задание 3: Найдите утечку данных 3

### Еще один нарушитель собирает содержимое электронной почты и отправляет в Интернет используя порт, который обычно используется для другого типа трафика. Атакующий пересылает большое количество информации используя этот порт, которое нехарактерно для других хостов, использующих этот номер порта. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителей из предыдущих задач.

### 1. Сколько всего данных отправлено на каждый порт

### 2. Исходя из графика, нужно найти только те порты, на которые отправлено меньше всего данных

    dataset %>%
      select(src, dst, bytes,port) %>%
      mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)") & !str_detect(dst,"^((12|13|14)\\.)"))) %>%
      filter(outside_traffic == TRUE) %>%
      group_by(port) %>%
      summarise(total_data=sum(bytes)) %>%
      filter(total_data < 5*10^9) %>%
      select(port) %>%
      collect() -> ports

    ports <- unlist(ports)
    ports <- as.vector(ports,'numeric')
