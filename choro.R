# --
# setup
# --
rm(list = ls()) 
setwd("~/Desktop/R/choropleth/")

# libraries
# --
# install.packages('devtools') (requiered ckanr version not yet available via cran)
# devtools::install_github("ropensci/ckanr")
# install.packages('RCurl')
# install.packages('rgdal')
# install.packages('leaflet')

require(ckanr)
require(RCurl)
require(rgdal)
require(leaflet)
require(jsonlite)

# import population datasets form http://data.stadt-zuerich.ch using ckanr
# --
ckanr_setup(url = "https://data.stadt-zuerich.ch")
bev_bes_url <- package_show('bev-bestand-jahr-quartier-seit1970', as='table')$resources$url
bev_nat_url <- package_show('bev-bestand-jahr-quartier-nationalitaet', as='table')$resources$url

bev_bes <- read.csv(text = getURL(bev_bes_url, .encoding = "UTF-8"))
bev_nat <- read.csv(text = getURL(bev_nat_url, .encoding = "UTF-8"))

# aggregate number of foreign population per year and district
bev_internat <- bev_nat[bev_nat$NationLang != "Schweiz",]
bev_internat <- aggregate(cbind(AnzBestWir)~StichtagDatJahr+QuarSort, data=bev_internat, sum, na.rm=TRUE)
names(bev_internat) <- c("StichtagDatJahr","QuarSort", "AnzInternat")

bev_internat_bes <- merge(bev_bes, bev_internat, by = c("StichtagDatJahr","QuarSort"))
bev_internat_bes$AnteilInternat <- bev_internat_bes$AnzInternat / bev_internat_bes$AnzBestWir

# import statistisches_quartier geojson using ckanr
# --
statQuart <- package_show('statistisches_quartier', as='table')$resources
statQuartJSON <- statQuart[statQuart$format == "JSON",]$url
geoJSON <- getURL(statQuartJSON, .encoding = "UTF-8") %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

writeLines(getURL(statQuartJSON, .encoding = "UTF-8"), "quartiere.json")
zurich <- readOGR(dsn = "quartiere.json", layer = "OGRGeoJSON")

# sort datasets by district
bev_internat_bes <- bev_internat_bes[order(bev_internat_bes$QuarSort),]
zurich <- zurich[order(zurich$QNr),]

# draw map
# --
# define color ranges
cn <- colorNumeric(c("#FFFFFF","#08427D"), domain = c(0,.5))
bins <- c(.0,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5)

palette <- colorBin(cn(bins), bins = bins)



mymap <- leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(minZoom=8, maxZoom=16)) %>%
  addLayersControl(
    baseGroups = c(1993:2015),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(
    position = 'topleft',
#     pal = cn,
#     bins = 2,
#     values = c(0,.5),
    colors = cn(bins), 
    labels = c('0%',"","","","","25%","","","","",'50%'),  ## legend labels (only min and max)
    opacity = 0.8,      ##transparency again
    title = "Ausländeranteil<br>pro Quartier")   ## title of the legend

for (i in 1993:2015) {
  mymap <- mymap %>% addPolygons(data = zurich,
                                  fillColor = ~cn(bev_internat_bes[bev_internat_bes$StichtagDatJahr == i,]$AnteilInternat), 
                                 fillOpacity = 0.8,
                                 color = "white", 
                                 weight = 2.0,
                                 popup = paste0("Quartier: ", bev_internat_bes[bev_internat_bes$StichtagDatJahr == i,]$QuarLang,
                                                "<br>Bestand: ", bev_internat_bes[bev_internat_bes$StichtagDatJahr == i,]$AnzBestWir,
                                                "<br>Zuzüge: ", bev_internat_bes[bev_internat_bes$StichtagDatJahr == i,]$AnzInternat, 
                                                "<br>Anteil: ", round(bev_internat_bes[bev_internat_bes$StichtagDatJahr == i,]$AnteilInternat*100)," %"),
                                 group=paste0(i, ""))
}

print(mymap)


library(htmlwidgets)
saveWidget(mymap, file = "index.html", selfcontained = F)
