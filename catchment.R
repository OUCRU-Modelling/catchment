library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(stringi)
library(patchwork)


dta <- read_excel("D:/OUCRU/HCDC/project phân tích sero quận huyện/measles/data/vaccine cover2.xlsx",
                  col_types = c("text", "text", "text",
                                "text", "date", "text", "text", "text",
                                "text", "date", "text", "numeric",
                                "numeric", "text", "numeric", "numeric",
                                "text", "text", "text", "text", "text",
                                "date", "text"))

datahcdc <- dta[-c(1:3),1:11] %>% na.omit()
dataoucru <- dta[,12:23]

dta <- dta[-c(1:3),] %>% data.frame() %>% clean_names()

dfpt <- datahcdc[,c(1,2,7,8,10)]
colnames(dfpt) <- c("stt","bv","px","qhchuan","col_date")

## plot function

plot_spatial <- function(data,map){

  data$Prevalence <- data$n/sum(data$n)*100

  output <- left_join(map, data.frame(data), by = join_by(varname_2 == qhchuan))
  output %>% ggplot() +
    geom_sf(aes(fill = Prevalence),show.legend = T)+
    scale_fill_continuous(low="yellow", high="red",
                          guide="colorbar",na.value="white",
                          name = "Percentage (%)")+
    theme_void()
}

map_path <- "D:/OUCRU/HCDC/project phân tích sero quận huyện/"

vn_qh <- st_read(dsn = file.path(map_path, "gadm41_VNM.gpkg"), layer = "ADM_ADM_2")


## chỉ lấy của TPHCM

library(janitor)  ## để có câu lệnh clean_names

vn_qh1 <- vn_qh %>%
  clean_names() %>%     ## cho thành chữ thường
  filter(
    str_detect(
      name_1,
      "Hồ Chí Minh"
    )
  )

vn_qh1 %>%
  ggplot() +
  ggplot2::geom_sf()


## gộp thủ đức
qhtp <- vn_qh1[-c(14,21),]

qhtp$geom[qhtp$varname_2 == "Thu Duc"] <- vn_qh1[c("21","24","14"),] %>%
  st_union()

qhtp <- qhtp %>% st_cast("MULTIPOLYGON")

qhtp$varname_2 <- stri_trans_general(qhtp$varname_2, "latin-ascii") %>%
  tolower() %>%
  str_remove("district") %>%
  trimws(which = "both")

## bản đồ theo bệnh viện

dfpt[,c("bv","px","qhchuan")] <- lapply(dfpt[,c("bv","px","qhchuan")],tolower)

dfpt$bv <- stri_trans_general(dfpt$bv,"latin-ascii")
dfpt$qhchuan <- dfpt$qhchuan %>% stri_trans_general("latin-ascii") %>%
  str_replace_all(
    c("tp thu duc"  = "thu duc"))


qhbv <- dfpt %>% group_by(bv) %>%
  count(qhchuan)

nd2 <- qhbv %>% filter(bv == "bv nhi dong 2")
nd2$per <- nd2$n/sum(nd2$n)*100

ndtp <- qhbv %>% filter(bv == "bv nhi dong tp")
ndtp$per <- ndtp$n/sum(ndtp$n)*100


## tọa độ bv nd2
tdq1 <- data.frame(long = 106.7023,
                   lat  = 10.7808) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


## tọa độ bv nd thành phố

tdndtp <- data.frame(long = 106.5741,
                     lat  = 10.7087) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

## tọa độ bv nd1
tdnd1 <- data.frame(long = 106.6702,
                    lat  = 10.7686) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# gộp những đợt lẫy của hcdc 36912
## nd2: 2 3 6 9 11 12
## ndtp: 3 5 9 12

dfpt$year <- year(dfpt$col_date)
dfpt$month <- month(dfpt$col_date)
dfpt$month <- dfpt$month %>%
  str_replace_all(
    c("^2"  = "3",
      "5"  = "6",
      "11"  = "12"))


dfhm <- dfpt %>% group_by(bv,month,year) %>%
  count(qhchuan)

dfhm$qhchuan <- dfhm$qhchuan %>%
  str_remove_all("^0")

## plot gộp các đợt thành 36912
## nd2, ndtp nam 2022
ch2_0922 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "9" & year == "2022") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('Sep 2022')

ch2_1222 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "12" & year == "2022") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('Dec 2022')

cch_0922 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "9" & year == "2022") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('Sep 2022')

cch_1222 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "12" & year == "2022") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('Dec 2022')

ch2_0922 + ch2_1222 + cch_0922 + cch_1222

## check lại data nhi dong 2
sum(ch2_0922$data$n,na.rm = T)
sum(ch2_1222$data$n,na.rm = T)
sum(cch_0922$data$n,na.rm = T)
sum(cch_1222$data$n,na.rm = T)

##
dfhm %>% filter(bv == "bv nhi dong 2" & year == "2022") %>% summarise( sum = sum(n))
dfhm %>% filter(bv == "bv nhi dong tp" & year == "2022") %>% summarise( sum = sum(n))

## nd2 nam 2023

ch2_0323 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "3" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('Mar 2023')

ch2_0623 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "6" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('June 2023')

ch2_0923 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "9" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('Sep 2023')

ch2_1223 <- dfhm %>% filter(bv == "bv nhi dong 2" & month == "12" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  ggtitle('Dec 2023')

## figure 1
ch2_0922 + ch2_1222 + ch2_0323 +
  ch2_0623 + ch2_0923 + ch2_1223 +
  plot_annotation(
    title = "Children's Hospital 2"
  )

## check lại data nhi dong 2
sum(ch2_0323$data$n,na.rm = T)
sum(ch2_0623$data$n,na.rm = T)
sum(ch2_0923$data$n,na.rm = T)
sum(ch2_1223$data$n,na.rm = T)


dfhm %>% filter(bv == "bv nhi dong 2" & year == "2023") %>% summarise( sum = sum(n))


## ndtp 2023
cch_0323 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "3" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('Mar 2023')

cch_0623 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "6" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('June 2023')

cch_0923 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "9" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('Sep 2023')

cch_1223 <- dfhm %>% filter(bv == "bv nhi dong tp" & month == "12" & year == "2023") %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  ggtitle('Dec 2023')

cch_0323 + cch_0623 + cch_0923 + cch_1223

## check data
sum(cch_0323$data$n,na.rm = T)
sum(cch_0623$data$n,na.rm = T)
sum(cch_0923$data$n,na.rm = T)
sum(cch_1223$data$n,na.rm = T)


dfhm %>% filter(bv == "bv nhi dong tp" & year == "2023") %>% summarise( sum = sum(n))

cch_0922 + cch_1222 + cch_0323 +
  cch_0623 + cch_0923 + cch_1223 +
  plot_annotation(
    title = "City Children's Hospital"
  )

## nd1

final <- read_csv("D:/OUCRU/HCDC/project phân tích sero quận huyện/cleaned.csv")
final$colmonth <- substr(final$id, 10, 11)


map5 <- final %>% group_by(colmonth) %>%
  count(qhchuan)

month6 <- plot_spatial(map5[map5$colmonth == "06",],qhtp) + ggtitle('June')
month7 <- plot_spatial(map5[map5$colmonth == "07",],qhtp) + ggtitle('July')
month8 <- plot_spatial(map5[map5$colmonth == "08",],qhtp) + ggtitle('August')
month9 <- plot_spatial(map5[map5$colmonth == "09",],qhtp) + ggtitle('September')
month10 <- plot_spatial(map5[map5$colmonth == "10",],qhtp) + ggtitle('October')


qhtp$nl_name_2 <- c("BC","BTân","BT","CG","CC","GV",
                    "HM","NB","PN","1","10","11","12"
                    ,"3","4","5","6","7","8","TB",
                    "TP","TĐ")

sample <- qhtp %>%
  ggplot() +
  ggplot2::geom_sf()+
  geom_sf_text(aes(label = nl_name_2),size=2)+
  theme_void()

month6 + month7 + month8 + month9 + month10 + sample +
  plot_annotation(
    title = "Children's Hospital 1"
  )


ch1 <- map5 %>% group_by(qhchuan) %>% summarise(n = sum(n))

## combine

plotchh <- dfhm %>% filter(bv == "bv nhi dong tp") %>% group_by(qhchuan) %>%
  summarise(n = sum(n)) %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdndtp,
          color = "blue", size = 1)+
  geom_sf_text(aes(label = nl_name_2),size=2)+
  ggtitle("City Children's Hospital")

plotch2 <- dfhm %>% filter(bv == "bv nhi dong 2") %>% group_by(qhchuan) %>%
  summarise(n = sum(n)) %>%
  plot_spatial(qhtp) +
  geom_sf(data = tdq1,
          color = "blue", size = 1)+
  geom_sf_text(aes(label = nl_name_2),size=2)+
  ggtitle("Children's Hospital 2")

plotch1 <- plot_spatial(ch1,qhtp) +
  geom_sf(data = tdnd1,
          color = "blue", size = 1)+
  geom_sf_text(aes(label = nl_name_2),size=2)+
  ggtitle("Children's Hospital 1")

plotchh + plotch1 + plotch2
