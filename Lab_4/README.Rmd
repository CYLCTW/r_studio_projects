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

### Импорт датасета

    dataset <- arrow::read_csv_arrow("D:\\Lab_RStudio\\gowiththeflow_20190826\\gowiththeflow_20190826.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

# Задание 1: Найдите утечку данных из Вашей сети

## Важнейшие документы с результатами нашей исследовательской деятельности в области создания вакцин скачиваются в виде больших заархивированных дампов. Один из хостов в нашей сети используется для пересылки этой информации – он пересылает гораздо больше информации на внешние ресурсы в Интернете, чем остальные компьютеры нашей сети. Определите его IP-адрес.

### Определение IP-адреса, который пересылает больше информации на внешние ресурсы.

    filter(dataset,str_detect(src,"^((12|13|14)\\.)"),
             str_detect(dst,"^((12|13|14)\\.)",negate=TRUE)) %>% 
      select(src,bytes) %>%
      group_by(src)%>% 
      summarise(bytes=sum(bytes))%>%
      slice_max(bytes)%>%
      select(src)

    ## # A tibble: 1 × 1
    ##   src         
    ##   <chr>       
    ## 1 13.37.84.125
### Ответ на задание - 13.37.84.125
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

# Задание 2: Надите утечку данных 2

## Другой атакующий установил автоматическую задачу в системном планировщике cron для экспорта содержимого внутренней wiki системы. Эта система генерирует большое количество траффика в нерабочие часы, больше чем остальные хосты. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителя из предыдущей задачи.

### Построив графики распределения количества пакетов и данных за каждый час, видно, что:
      С 0:00 по 15:00 активность наименьшая по сравнению  с 16:00 по 24:00
### Следовательно нерабочее время - 0-15.
    Sys.setlocale("LC_TIME", "Russian")

    ## [1] "Russian_Russia.1251"

    dataset %>%
    select(timestamp, src, dst, bytes) %>%
    mutate(outside_traffic = (!str_detect(src,"^((12|13|14)\\.)")&str_detect(dst,"^((12|13|14)\\.)")), hour = hour(as.POSIXlt(timestamp/1000, origin = "1970-01-01"))) %>%
    filter(outside_traffic == TRUE, hour >= 0 | hour <= 15) %>%
    group_by(src) %>%
    summarise(total_bytes = sum(bytes)) %>%
    arrange(desc(total_bytes))  %>%
    head(10)

    ## # A tibble: 10 × 2
    ##    src           total_bytes
    ##    <chr>               <int>
    ##  1 18.123.106.55   904203418
    ##  2 16.120.48.114   885500734
    ##  3 15.15.33.102    854630369
    ##  4 16.22.121.27    842525939
    ##  5 15.103.68.28    819858455
    ##  6 15.30.41.61     799231586
    ##  7 17.101.52.27    787755479
    ##  8 19.70.89.67     785682368
    ##  9 17.86.43.89     770490494
    ## 10 16.42.73.76     769727715
### Ответ на задание - 18.123.106.55
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

### Импорт датасета

    dataset <- arrow::read_csv_arrow("D:\\Lab_RStudio\\gowiththeflow_20190826\\gowiththeflow_20190826.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

#Задание 3: Найдите утечку данных 3
#Еще один нарушитель собирает содержимое электронной почты и #отправляет в Интернет используя порт, который обычно используется #для другого типа трафика. Атакующий пересылает большое количество #информации используя этот порт, которое нехарактерно для других #хостов, использующих этот номер порта. Определите IP этой системы. #Известно, что ее IP адрес отличается от нарушителей из предыдущих #задач.
### Нужно найти только те порты, на которые отправлено меньше всего данных

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

### Выбрать данные с нужными номерами портов

    dataset %>%
      select(src, dst, bytes,port) %>%
      mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)") & !str_detect(dst,"^((12|13|14)\\.)"))) %>%
      filter(outside_traffic == TRUE) %>%
      filter(port %in% ports) %>%
      group_by(src,port) %>%
      summarise(total_bytes=sum(bytes)) %>%
      arrange(desc(port)) %>%
      collect() -> df

### Порты с маскимальным кол-вом данных

    df %>%
      group_by(src, port) %>%
      summarise(total_data=sum(total_bytes)) %>%
      arrange(desc(total_data)) %>%
      head(10) %>%
      collect()

    ## # A tibble: 10 × 3
    ## # Groups:   src [3]
    ##    src           port total_data
    ##    <chr>        <int>      <int>
    ##  1 13.37.84.125    36 2070876332
    ##  2 13.37.84.125    95 2031985904
    ##  3 13.37.84.125    21 2027501066
    ##  4 13.37.84.125    78 2018366254
    ##  5 13.37.84.125    32 1989408807
    ##  6 12.55.77.96     31  233345180
    ##  7 13.48.72.30     26    2468348
    ##  8 13.48.72.30     61    2465805
    ##  9 13.48.72.30     77    2453566
    ## 10 13.48.72.30     79    2421971

### Количество хостов к портам

    df %>%
      group_by(port) %>%
      summarise(hosts=n()) %>%
      arrange(hosts) %>%
      head(10) %>%
      collect()

    ## # A tibble: 10 × 2
    ##     port hosts
    ##    <int> <int>
    ##  1    21     1
    ##  2    31     1
    ##  3    32     1
    ##  4    36     1
    ##  5    78     1
    ##  6    95     1
    ##  7    51    24
    ##  8    22  1000
    ##  9    23  1000
    ## 10    25  1000

### Из предыдущих пунктов следует вывод, что ip-адрес злоумышленника 12.55.77.96, а порт 31, т.к. из таблицы в 5 пункте видно, что 31 порт использовал только 1 хост и в тоже время из таблицы в 4 пункте видно, что больше всего данных было передано именно по этому порту

    df %>%
      filter(port == 31) %>%
      group_by(src) %>%
      summarise(total_data=sum(total_bytes)) %>%
      collect()

    ## # A tibble: 1 × 2
    ##   src         total_data
    ##   <chr>            <int>
    ## 1 12.55.77.96  233345180

### Ответ на задание - 12.55.77.96
