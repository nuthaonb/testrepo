rm(list=ls()) #Clear all environment 
#packages <- c("dplyr","gplots","ggplot2","rmarkdown","tidyr","readxl",
#              "reshape","reshape2","openxlsx","ggthemes","psych","kableExtra",
#              "jtools","huxtable","broom","officer","flextable","ggstance",
#              "broom.mixed","sjPlot", "sjmisc", "sjlabelled","dygraphs","xts")
#install.packages(packages,repos = "http://cran.us.r-project.org")

#install.packages("xts")
#library("xts")

libr <- c("dplyr","gplots","ggplot2","rmarkdown","tidyr","readxl",
          "reshape","reshape2","openxlsx","ggthemes","psych","kableExtra",
          "jtools","huxtable","broom","officer","flextable","ggstance",
          "broom.mixed","sjPlot", "sjmisc", "sjlabelled","dygraphs","xts")

lapply(libr, require, character.only=T)

setwd("Z:/Business and Economic Intelligence/2. BSI/Research/2020 Research") #Check directory first

Sys.setlocale(locale = "English")

####################################
############ Import Data ###########
####################################
raw.plot.data <- read_excel("uncertainty indices/FDISP_ex-ante forecast dispersion_full.xlsx",
                            sheet = "Plot Data", skip = 1, col_names = TRUE, col_types = c("date","text",rep("numeric",37)))

#raw.plot.data$Date <- format(raw.plot.data$Date, format = "%b-%Y")

####################################
########### Visualization ##########
####################################

#Color tone
#color.tone <- c("G","Y","P")
#color.tone <- c("#CCEBD6", "#FFFCA9", "#FFE6E6")
  
#V1-Production: FDISP_PROD&FEDISP_PROD and MPI
prod.data <- raw.plot.data %>%
  select(Date, ends_with("PROD"), MPI)

prod.data <- xts( x=prod.data, order.by = prod.data$Date)

##Normal
dygraph(prod.data, main = "Production") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty indices") %>%
  dyAxis("y2", label = "MPI") %>%
  dySeries("MPI", axis = 'y2', color = "#CC3366", strokeWidth = 3) %>%
  dySeries("OD_PROD", strokeWidth = 1.75, color = "#6666FF") %>%
  dySeries("FE3M_PROD", strokeWidth = 1.75, color = "#FF9933") %>%
  dySeries("FE1M_PROD", strokeWidth = 1.75, color = "#00CC99") %>%
#  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyShading(from = "2008-08-01", to = "2009-01-01", color = "#CCEBD6" ) %>%
#  dyShading(from = "2009-12-01", to = "2010-01-01", color = "#CCEBD6" ) %>%
#  dyShading(from = "2010-03-01", to = "2010-04-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2010-04-01", to = "2010-05-01", color = "#CCEBD6" ) %>%
#  dyShading(from = "2010-12-01", to = "2011-01-01", color = "#FFFCA9" ) %>%
#  dyShading(from = "2011-03-01", to = "2011-04-01", color = "#FFFCA9" ) %>%
#  dyShading(from = "2011-04-01", to = "2011-06-01", color = "#FFE6E6" ) %>%
#  dyShading(from = "2011-06-01", to = "2011-07-01", color = "#FFFCA9" ) %>%
#  dyShading(from = "2011-09-01", to = "2011-11-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2012-11-01", to = "2012-12-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2013-01-01", to = "2013-02-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2013-10-01", to = "2013-11-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2014-04-01", to = "2014-06-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2015-02-01", to = "2015-05-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2015-07-01", to = "2015-08-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2015-09-01", to = "2015-10-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2015-10-01", to = "2015-11-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2016-03-01", to = "2016-11-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2016-11-01", to = "2017-01-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2017-03-01", to = "2017-04-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2017-08-01", to = "2017-09-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2018-03-01", to = "2018-04-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2017-08-01", to = "2017-09-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2018-11-01", to = "2018-12-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2019-07-01", to = "2019-09-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2020-02-01", to = "2020-05-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

#V2-Investment: FDISP_INV&FEDISP_INV and PII_SA
inv.data <- raw.plot.data %>%
  select(Date, ends_with("INV"), PII_SA)

inv.data <- xts( x=inv.data, order.by = inv.data$Date)

dygraph(inv.data, main = "Investment") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty indices") %>%
  dyAxis("y2", label = "PII_SA") %>%
#  dyAxis("y", label = "Uncertainty indices", valueRange = c(0, 1.2)) %>%
#  dyAxis("y2", label = "PII_SA", valueRange = c(60, 120)) %>%
  dySeries("PII_SA", axis = 'y2', color = "#CC3366", strokeWidth = 3) %>%
  dySeries("OD_INV", strokeWidth = 1.75, color = "#6666FF") %>%
  dySeries("FE3M_INV", strokeWidth = 1.75, color = "#FF9933") %>%
  dySeries("FE1M_INV", strokeWidth = 1.75, color = "#00CC99") %>%
  #  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyShading(from = "2011-09-01", to = "2011-11-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2014-07-01", to = "2014-08-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2016-11-01", to = "2017-01-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2018-03-01", to = "2018-04-01", color = "#FFE6E6" ) %>%
  dyShading(from = "2019-05-01", to = "2019-06-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2020-02-01", to = "2020-06-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

#V3-Overall BSI: FEDISP_BSI and BSI_cur&BSI_3M
ovrall.data <- raw.plot.data %>%
  select(Date, ends_with("BSI"), BSI, BSI_3M)

ovrall.data <- xts(x=ovrall.data, order.by = ovrall.data$Date)

dygraph(ovrall.data, main = "Overall BSI") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty indices") %>%
  dyAxis("y2", label = "BSI") %>%
  #  dyAxis("y", label = "Uncertainty indices", valueRange = c(0, 1.2)) %>%
  #  dyAxis("y2", label = "PII_SA", valueRange = c(60, 120)) %>%
  dySeries("BSI", axis = 'y2', color = "#CC3366", strokeWidth = 2.5) %>%
  dySeries("BSI_3M", axis = 'y2', color = "#FF9933", strokeWidth = 2.5) %>%
  dySeries("FE3M_BSI", strokeWidth = 1.75, color = "#6666FF") %>%
  dySeries("FE1M_BSI", strokeWidth = 1.75, color = "#00CC99") %>%
  #  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyShading(from = "2008-10-01", to = "2008-11-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2010-03-01", to = "2010-04-01", color = "#FFE6E6") %>%
  dyShading(from = "2011-09-01", to = "2011-10-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2015-08-01", to = "2015-10-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2019-03-01", to = "2019-04-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2020-03-01", to = "2020-04-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

#V3-Overall BSI: FEDISP_BSI and BSI_cur
ovrall1.data <- raw.plot.data %>%
  select(Date, ends_with("BSI"), BSI)

ovrall1.data <- xts(x=ovrall1.data, order.by = ovrall1.data$Date)

dygraph(ovrall1.data, main = "Overall BSI") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty indices") %>%
  dyAxis("y2", label = "BSI") %>%
  #  dyAxis("y", label = "Uncertainty indices", valueRange = c(0, 1.2)) %>%
  #  dyAxis("y2", label = "PII_SA", valueRange = c(60, 120)) %>%
  dySeries("BSI", axis = 'y2', color = "#CC3366", strokeWidth = 2.5) %>%
#  dySeries("BSI_3M", axis = 'y2', color = "#FF9933", strokeWidth = 2.5) %>%
  dySeries("FE3M_BSI", strokeWidth = 1.75, color = "#6666FF") %>%
  dySeries("FE1M_BSI", strokeWidth = 1.75, color = "#00CC99") %>%
  #  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1")) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyShading(from = "2008-10-01", to = "2008-11-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2010-03-01", to = "2010-04-01", color = "#FFE6E6") %>%
  dyShading(from = "2011-09-01", to = "2011-10-01", color = "#FFFCA9" ) %>%
  dyShading(from = "2015-08-01", to = "2015-10-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2019-03-01", to = "2019-04-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2020-03-01", to = "2020-04-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

#V4-Outlook Dispersion: OD
dispersion.data <- raw.plot.data %>%
  select(Date, starts_with("OD"))

dispersion.data <- xts(x=dispersion.data, order.by = dispersion.data$Date)

dygraph(dispersion.data, main = "Outlook Dispersion: OD") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty index") %>%
  dySeries("OD_PERF", color = "#CC3366", strokeWidth = 2.5) %>%
  dySeries("OD_PROD", color = "#FF9933", strokeWidth = 2.5) %>%
  dySeries("OD_EMPLOY", color = "#6666FF", strokeWidth = 2.5) %>%
  dySeries("OD_INV", color = "#00CC99", strokeWidth = 2.5) %>%
  dySeries("OD_COST", color = "#75CEE0", strokeWidth = 2.5) %>%
  dySeries("OD_ORDER", color = "#A0D87F", strokeWidth = 2.5) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyRangeSelector() %>%
  dyLegend(width = 850, show = "always", hideOnMouseOut = TRUE)

#V5-Forecast Error: FE all
fe.data <- raw.plot.data %>%
  select(Date, starts_with("FE"))

fe.data <- xts(x=fe.data, order.by = fe.data$Date)

dygraph(fe.data, main = "Forecast Error: FE") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty index") %>%
  dySeries("FE1M_BSI", color = "#CC3366", strokeWidth = 2.5, drawPoints = TRUE, pointSize = 3.5) %>%
  dySeries("FE3M_BSI", color = "#00CC99", strokeWidth = 2.5, drawPoints = TRUE, pointSize = 3.5) %>%
  dySeries("FE1M_PROD", color = "#6666FF", strokeWidth = 2.5) %>%
  dySeries("FE3M_PROD", color = "#FF9933", strokeWidth = 2.5) %>%
  dySeries("FE1M_INV", color = "#A0D87F", strokeWidth = 2.5) %>%
  dySeries("FE3M_INV", color = "#75CEE0", strokeWidth = 2.5) %>%
#  dySeries("FEDISP_EMPLOY", color = "#6666FF", strokeWidth = 2.5) %>%
#  dySeries("FEDISP_INV", color = "#FF9933", strokeWidth = 2.5) %>%
#  dySeries("FEDISP_COST", color = "#75CEE0", strokeWidth = 2.5) %>%
#  dySeries("FEDISP_ORDER", color = "#A0D87F", strokeWidth = 2.5) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyRangeSelector() %>%
  dyLegend(width = 850, show = "always", hideOnMouseOut = TRUE)

#V6-Forecast Error: FE1M
fe1m.data <- raw.plot.data %>%
  select(Date, starts_with("FE1M"))

fe1m.data <- xts(x=fe1m.data, order.by = fe1m.data$Date)

dygraph(fe1m.data, main = "Forecast Error 1 month: FE1M") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty index") %>%
  dySeries("FE1M_BSI", color = "#CC3366", strokeWidth = 2.5, drawPoints = TRUE, pointSize = 3.5) %>%
#  dySeries("FE3M_BSI", color = "#00CC99", strokeWidth = 2.5) %>%
  dySeries("FE1M_PROD", color = "#6666FF", strokeWidth = 2.5) %>%
#  dySeries("FE3M_PROD", color = "#FF9933", strokeWidth = 2.5) %>%
  dySeries("FE1M_INV", color = "#A0D87F", strokeWidth = 2.5) %>%
#  dySeries("FE3M_INV", color = "#75CEE0", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_EMPLOY", color = "#6666FF", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_INV", color = "#FF9933", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_COST", color = "#75CEE0", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_ORDER", color = "#A0D87F", strokeWidth = 2.5) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyRangeSelector() %>%
  dyLegend(width = 850, show = "always", hideOnMouseOut = TRUE)

#V6-Forecast Error: FE3M
fe3m.data <- raw.plot.data %>%
  select(Date, starts_with("FE3M"))

fe3m.data <- xts(x=fe3m.data, order.by = fe3m.data$Date)

dygraph(fe3m.data, main = "Forecast Error 3 month: FE3M") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Uncertainty index") %>%
#  dySeries("FE1M_BSI", color = "#CC3366", strokeWidth = 2.5) %>%
  dySeries("FE3M_BSI", color = "#00CC99", strokeWidth = 2.5, drawPoints = TRUE, pointSize = 3.5) %>%
#  dySeries("FE1M_PROD", color = "#6666FF", strokeWidth = 2.5) %>%
  dySeries("FE3M_PROD", color = "#FF9933", strokeWidth = 2.5) %>%
#  dySeries("FE1M_INV", color = "#A0D87F", strokeWidth = 2.5) %>%
  dySeries("FE3M_INV", color = "#75CEE0", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_EMPLOY", color = "#6666FF", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_INV", color = "#FF9933", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_COST", color = "#75CEE0", strokeWidth = 2.5) %>%
  #  dySeries("FEDISP_ORDER", color = "#A0D87F", strokeWidth = 2.5) %>%
  dyEvent("2008-10-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#0033FF")%>%
  dyRangeSelector() %>%
  dyLegend(width = 850, show = "always", hideOnMouseOut = TRUE)

#V7-Final plot: selected uncertainty indices with Economic indicators
##7.1: with Economic indicators
final.plot1.data <- raw.plot.data %>%
  select(Date, OD_PROD, FE3M_PERF, MPI_SA, PII_SA, CEI, BSI_SA)

final.plot1.data <- xts(x= final.plot1.data, order.by = final.plot1.data$Date)
#A0D87F
##Uncertainty indices with Economic indicators
dygraph(final.plot1.data, main = "pic 1: Uncertainty indices plot with Economic indicators") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "Economic indicators") %>%
  dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  dySeries("PII_SA",  color = "#FF9933", strokeWidth = 3) %>%
  dySeries("CEI", color = "#6666FF", strokeWidth = 3) %>%
  dySeries("BSI_SA",  color = "#2275F5", strokeWidth = 3) %>%
  dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#88B858") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#8803FE")%>%
  dyShading(from = "2008-04-01", to = "2008-12-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2011-05-01", to = "2011-11-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2014-01-01", to = "2014-02-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2019-02-01", to = "2020-07-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2015-09-01", to = "2015-10-01", color = "#FFFCA9" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##7.2: with GDP
final.plot2.data <- raw.plot.data %>%
  select(Date, OD_PROD, FE3M_PERF, GDP_sa_qoq_avg)

final.plot2.data <- xts(x= final.plot2.data, order.by = final.plot2.data$Date)
#A0D87F
##Uncertainty indices with Economic indicators
dygraph(final.plot2.data, main = "pic 2: Uncertainty indices plot with GDP") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "GDP", valueRange = c(-12,12)) %>%
  dySeries("GDP_sa_qoq_avg", color = "#CC3366", strokeWidth = 3) %>%
  #dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  #dySeries("PII_SA",  color = "#FF9933", strokeWidth = 3) %>%
  #dySeries("CEI", color = "#6666FF", strokeWidth = 3) %>%
  #dySeries("BSI_SA",  color = "#2275F5", strokeWidth = 3) %>%
  dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#88B858") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19    ", labelLoc = "top", color = "#8803FE")%>%
  dyShading(from = "2008-04-01", to = "2009-03-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2011-02-01", to = "2011-12-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2017-08-01", to = "2017-12-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2019-02-01", to = "2020-07-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)


#V8-1st round presentation's plot
##8.1: All with MPI, SPI
pres.plot1.data <- raw.plot.data %>%
  select(Date, SPI_SA, MPI_SA, OD_BSI, OD_PERF, OD_PROD, OD_ORDER, FE3M_PERF)

pres.plot1.data <- xts(x= pres.plot1.data, order.by = pres.plot1.data$Date)

dygraph(pres.plot1.data, main = "Uncertainty indices plot with Economic indicators (ALL)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "Economic indicators") %>%
  dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  dySeries("OD_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#8803FE")%>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##8.2: All with SPI
pres.plot2.data <- raw.plot.data %>%
  select(Date, OD_BSI, OD_PERF, SPI_SA)

pres.plot2.data <- xts(x= pres.plot2.data, order.by = pres.plot2.data$Date)

dygraph(pres.plot2.data, main = "Uncertainty indices plot with SPI") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "Economic indicators") %>%
  #dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  #dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  #dySeries("OD_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  #dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#8803FE")%>%
  dyShading(from = "2019-05-01", to = "2020-06-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##8.3: All with MPI
pres.plot3.data <- raw.plot.data %>%
  select(Date, OD_PROD, FE3M_PERF, MPI_SA)

pres.plot3.data <- xts(x= pres.plot3.data, order.by = pres.plot3.data$Date)

dygraph(pres.plot3.data, main = "Uncertainty indices plot with MPI") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "Economic indicators") %>%
  dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  #dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  #dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  #dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  #dySeries("FE3M_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#8803FE")%>%
  dyShading(from = "2008-07-01", to = "2009-01-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2010-02-01", to = "2010-05-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2011-05-01", to = "2011-11-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2013-09-01", to = "2014-02-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2015-07-01", to = "2015-11-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2019-05-01", to = "2020-05-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##8.4: GDP QoQ
pres.plot4.data <- raw.plot.data %>%
  select(Date, FE3M_PERF, GDP_sa_qoq_avg)

pres.plot4.data <- xts(x= pres.plot4.data, order.by = pres.plot4.data$Date)

dygraph(pres.plot4.data, main = "Uncertainty indices plot with GDP (QoQ)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "GDP", valueRange = c(-12,12)) %>%
  dySeries("GDP_sa_qoq_avg", color = "#CC3366", strokeWidth = 3) %>%
  #dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  #dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  #dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  #dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  #dySeries("OD_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19    ", labelLoc = "top", color = "#8803FE")%>%
  dyShading(from = "2008-08-01", to = "2009-03-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2010-03-01", to = "2010-06-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2011-03-01", to = "2012-01-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2013-09-01", to = "2014-02-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2018-03-01", to = "2018-08-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2018-12-01", to = "2020-03-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##8.5: GDP YoY
pres.plot5.data <- raw.plot.data %>%
  select(Date, OD_ORDER, GDP_yoy_avg)

pres.plot5.data <- xts(x= pres.plot5.data, order.by = pres.plot5.data$Date)

dygraph(pres.plot5.data, main = "Uncertainty indices plot with GDP (YoY)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "GDP", valueRange = c(-20,20)) %>%
  dySeries("GDP_yoy_avg", color = "#CC3366", strokeWidth = 3) %>%
  #dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  #dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  #dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  #dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  dySeries("OD_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  #dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War   ", labelLoc = "top", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19    ", labelLoc = "top", color = "#8803FE")%>%
  dyShading(from = "2008-06-01", to = "2009-03-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2010-03-01", to = "2010-06-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2011-07-01", to = "2011-12-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2013-09-01", to = "2014-03-01", color = "#CCEBD6" ) %>%
  dyShading(from = "2018-07-01", to = "2020-03-01", color = "#CCEBD6" ) %>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)

##8.6: All with MPI, SPI
pres.plot6.data <- raw.plot.data %>%
  select(Date, SPI_SA, MPI_SA, OD_BSI, OD_PERF, OD_ORDER, FE3M_PERF, OD_PROD)

pres.plot6.data <- xts(x= pres.plot6.data, order.by = pres.plot6.data$Date)

dygraph(pres.plot6.data, main = "pic 1.4: Uncertainty indices plot with Economic indicators (ALL)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y2", label = "Uncertainty indices") %>%
  dyAxis("y", label = "Economic indicators") %>%
  dySeries("MPI_SA", color = "#CC3366", strokeWidth = 3) %>%
  dySeries("SPI_SA",  color = "#FF9933", strokeWidth = 3) %>%
  dySeries("OD_BSI", axis= 'y2', strokeWidth = 3, color = "#2275F5") %>%
  dySeries("OD_PERF", axis= 'y2', strokeWidth = 3,  color = "#CB7FEF") %>%
  dySeries("OD_PROD", axis= 'y2', strokeWidth = 3, color = "#00CC99") %>%
  dySeries("OD_ORDER", axis= 'y2', strokeWidth = 3, color = "#75C5F0") %>%
  dySeries("FE3M_PERF", axis= 'y2', strokeWidth = 3, color = "#768952") %>%
  dyEvent("2008-06-01", "การชุมนุมเสื้อเหลือง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2008-09-01", "GFC", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2010-04-01", "การชุมนุมเสื้อแดง", labelLoc = "bottom", color = "#0033FF") %>%
  dyEvent("2010-05-01", "EU Debt Crisis", labelLoc = "top", color = "#FF3300") %>%
  dyEvent("2011-08-01", "มหาอุทกภัย 2554", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2013-10-01", "การชุมนุม กปปส.", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2014-01-01", "Ebola outbreak", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2014-06-01", "รัฐประหาร", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2015-09-01", "ระเบิดแยกราชประสงค์", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2016-07-01", "Brexit", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2016-11-01", "การสวรรคตของ ร.9", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2018-03-01", "Trade War", labelLoc = "bottom", color = "#FF3300")%>%
  dyEvent("2019-04-01", "เลือกตั้ง", labelLoc = "bottom", color = "#0033FF")%>%
  dyEvent("2020-02-01", "COVID-19", labelLoc = "bottom", color = "#8803FE")%>%
  dyRangeSelector() %>%
  dyLegend(width = 650, show = "always", hideOnMouseOut = TRUE)
