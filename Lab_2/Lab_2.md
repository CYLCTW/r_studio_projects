### Подключение нужных пакетов

    library(arrow)

    ## 
    ## Присоединяю пакет: 'arrow'

    ## Следующий объект скрыт от 'package:utils':
    ## 
    ##     timestamp

    library(dplyr)

    ## 
   

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

    Sys.setlocale("LC_TIME", "Russian")

    ## [1] "Russian_Russia.1251"

    dataset %>%
    select(timestamp, src, dst, bytes) %>%
    mutate(outside_traffic = (!str_detect(src,"^((12|13|14)\\.)")&str_detect(dst,"^((12|13|14)\\.)")), hour = hour(as.POSIXlt(timestamp/1000, origin = "1970-01-01"))) %>%
    filter(outside_traffic == TRUE, hour < 8 | hour > 17) %>%
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
