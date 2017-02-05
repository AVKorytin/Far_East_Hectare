setwd("~/R/MRN/Dyatkovo")
files <- dir()
df <- read.csv2(files[1], stringsAsFactors = F, encoding = "latin1")
str(df)
for (i in files[-1]) {
        df0 <- read.csv2(i, stringsAsFactors = F, encoding = "latin1")
        df <- rbind(df, df0)
}

date <- strsplit(df$Дата.сделки, "[.]")
year <- as.numeric(sapply(date, "[", 2))
month <- as.numeric(sapply(date, "[", 1))
time <- (year - min(year))*12 + month # year + month/12
h <- data.frame(price = df$Удельная.цена.сделки..предложения..руб..кв.м., time)
h$type <- df$Вид.объекта.недвижимости
h$mortgage <- df$Ограничения..обременения..вещных.прав
require(ggplot2)
h1 <- h[sample(1:nrow(h), 10000),]
ggplot(h1, aes(time, price, color = mortgage)) + geom_point() + stat_smooth() + xlim(1999, 2016) + scale_y_log10()

# Delete doubles
# Time-ajustment outliers filter
# XY detection

#Part 2. Far-East Gektar
setwd("~/R/DV_gektar/")
df <- read.csv2("~/R/DV_gektar/Chukotka.csv", stringsAsFactors = F, encoding = "latin1")
setwd("~/R/DV_gektar/Sakhalin/")
files <- dir()
for (i in files) df <- rbind(df, read.csv2(i, stringsAsFactors = F, encoding = "latin1"))
cadastre <- strsplit(df$Кадастровый.квартал, "[:]")
k0 <- sapply(cadastre, "[", 1)
k1 <- sapply(cadastre, "[", 2)
k2 <- sapply(cadastre, "[", 3)
h <- data.frame(id = df$Номер.объекта, k0, k1, k2, region = df$Субъект, district = df$Район,
                city = df$Город, place = df$Населенный.пункт, time, s = df$Площадь..кв.м.,
                p = df$Удельная.цена.сделки..предложения..руб..кв.м., v = df$Цена.сделки..предложения..руб.,
                share = df$Доля.в.праве.на.объект.недвижимости, objects_in_deal = df$Количество.объектов.в.сделке,
                restriction = df$Ограничения..обременения..вещных.прав, stringsAsFactors = F)
require(ggplot2)
table(h$region, h$k0)
table(nchar(h$district) > 1, nchar(h$city) > 1, useNA = "ifany")
length(levels(factor(h$place)))
h$use <- factor(df$Разрешенное.использование)
levels(h$use) <- substr(levels(h$use), 5, 54)
with(subset(h, h$use %in% levels(h$use)[c(1,2,4,5,7,10,13)] & h$s >= 400 & h$s <=10^5),
     qplot(p, geom = "density", fill = use, alpha = 1/3, log = "x"))
h$year <- year
h$k <- k
h1 <- subset(h, s > 99 & s < 10^6 & p > .1 & p < 10^4 & year >= 2007 & !duplicated(id), -c(k0, k1))

sp <- aggregate(s ~ region, h1, sum)
val <- aggregate(v ~ region, h1, sum)
wm <- data.frame(region = sp$region, wmean = val$v/sp$s)

require(tidyr)
require(plyr)
#require(dplyr)
spread(ddply(h1, .(year, region), function(x) data.frame(wp = weighted.mean(x$p, x$s))), year, wp)
spread(ddply(h1, .(year, region), function(x) data.frame(n = length(x$p))), year, n)
prices <- ddply(h1, .(k), function(x) data.frame(wp = weighted.mean(x$p, x$s), m = median(x$p),
                                                 q1 = quantile(x$p, .25), q3 = quantile(x$p, .75),
                                                 n = length(x$p), sp = sum(x$s)/10^4))
top <- arrange(subset(prices, n > 20), desc(m))
top$name <- mapply(function(x) names(with(subset(h, k == x), sort(table(c(city[city != ""], district[district != ""])), decreasing = T)))[1], top$k)
top
#chance <- mapply(function(x) {
 #       y <- with(subset(h, k == x), sort(table(c(city[city != ""], district[district != ""])), decreasing = T))
  #      return(paste(y[1], y[2])) }, top$k)
saveRDS(top, "~/R/top.RDS")
saveRDS(h1, "~/R/dv_sample.RDS")
saveRDS(df, "~/R/dv_gektar.RDS")
with(subset(h1, k == "25:19"), qplot(p, geom = "density", fill = use, alpha = 1/3, log = "x"))

paste0("http://pkk5.rosreestr.ru/#text=", gsub(":", "%3A", kk[!(kk %in% coords$k)]), "&type=2&app=search&opened=1")

require(RSelenium)
rsDriver()
startServer(args=c("-Dwebdriver.chrome.driver=C:/Users/Korytin_Andrey/Documents/R/chromedriver.exe"),
            log = FALSE, invisible = FALSE)
remDr <- remoteDriver(browserName = "chrome")
l <- length(k_)
coords3 <- data.frame(k = rep(NA, l), x = rep(NA, l), y = rep(NA, l), stringsAsFactors = F)
k <- levels(factor(h11$kk))[!(levels(factor(h11$kk)) %in% levels(factor(h1$k_)))]
k_ <- paste0("http://pkk5.rosreestr.ru/#text=", gsub(":","%3A", k), "&type=2&app=search&opened=1")
remDr$open(silent = TRUE)
for(i in 2:l) {
        url <- k_[i]
        remDr$navigate(url)
        Sys.sleep(3)
        a <- remDr$getCurrentUrl()
        coords3$k[i] <- gsub("%3A", ":", gsub("(.*text=)|(&type=.*)", "", a[[1]]))
        coords3$x[i] <- gsub("(.*x=)|(&y=.*)", "", a[[1]])
        coords3$y[i] <- gsub("(.*y=)|(&z=.*)", "", a[[1]])
        print(i)
        if (coords3$x[i]==coords3$x[i-1] | nchar(coords3$x[i]) > 22) {
                remDr$refresh()
                remDr$navigate(url)
                Sys.sleep(3)
                a <- remDr$getCurrentUrl()
                coords3$k[i] <- gsub("%3A", ":", gsub("(.*text=)|(&type=.*)", "", a[[1]]))
                coords3$x[i] <- gsub("(.*x=)|(&y=.*)", "", a[[1]])
                coords3$y[i] <- gsub("(.*y=)|(&z=.*)", "", a[[1]])
                print(i)
        }
}
coords0 <- rbind(coords[,1:3], coords2[,1:3], coords1)
require(geosphere)
coords0 <- cbind(coords0[,1:3], mercator(coords0[,2:3], inverse = T))
saveRDS(coords0, "coords0.Rds")

p <- aggregate(p ~ k, subset(h, k %in% coords0$k), median)
v <- aggregate(v ~ k, subset(h, k %in% coords0$k), length)
f <- merge(merge(coords0, p, by = "k"), v, by = "k")[-6657,]
saveRDS(f, "land_parcels_deals.Rds")
saveRDS(df, "initial_data.Rds")
saveRDS(h, "filtred_data.Rds")

require(leaflet)
ff <- sp::SpatialPointsDataFrame(f[,4:5], f[,c(1,6,7)])
#ff$p <- log(ff$p)
f09 <- function(x) ifelse(x>0, ifelse(x<9, x, 9), 0)
pal <- colorNumeric("YlOrRd", f09(ff$p))
m <- leaflet(ff) %>% addTiles() %>%
        addCircleMarkers(radius = ~sqrt(4*v), stroke = F, color = ~pal(f09(p)),
                         fillOpacity = ~.5, popup = ~paste(k, round(exp(p)), sep = " ~ "))
m %>% addLegend("bottomright", pal = pal, values = ~f09(p), title = "Median parcels' price, RUB/sq.m",
                labFormat = labelFormat(suffix=" RUB", transform = function(x) round(exp(f09(x))),
                                        big.mark = " "), opacity = 1)


