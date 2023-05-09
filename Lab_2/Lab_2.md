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
    mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)")&!str_detect(dst,"^((12|13|14)\\.)")), hour = hour(as.POSIXlt(timestamp/1000, origin = "1970-01-01"))) %>%
    filter(outside_traffic == TRUE, hour >=0 & hour <= 15) %>%
    group_by(src) %>%
    summarise(total_bytes = sum(bytes)) %>%
    arrange(desc(total_bytes))  %>%
    head(10)

    ## # A tibble: 10 × 2
    ##    src          total_bytes
    ##    <chr>              <dbl>
    ##  1 13.37.84.125  4355695968
    ##  2 13.48.72.30    699869643
    ##  3 14.51.75.107   664232091
    ##  4 14.51.30.86    662053441
    ##  5 12.59.25.34    656414986
    ##  6 13.39.46.94    645382172
    ##  7 12.56.32.111   644762456
    ##  8 14.57.50.29    644628807
    ##  9 12.58.68.102   634977381
    ## 10 12.37.36.110   613188438

# График распределения отправленных пакетов в каждый час

\#group\_by(hour) %&gt;% \#summarise(packets=n()) %&gt;% \#collect()
\#ggplot(., aes(x=hour,y=packets),) + geom\_histogram(stat=“identity”,
color=“black”,fill=“red”)
