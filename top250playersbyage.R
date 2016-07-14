spielervalue <- function(n) {
  for (i in 1:10) {
    spiels <- str_c("http://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop/plus/ausrichtung/alle/spielerposition_id/alle/altersklasse/", n, "/land_id/0/yt0/Show/0//page/", i) %>%
      read_html(encoding = "UTF-8")
    dfspieli <- data.frame(name = spiels %>%
                             html_nodes(".spielprofil_tooltip") %>%
                             html_text(),
                           position = spiels %>%
                             html_nodes(xpath = "//*[@class='inline-table']//tr[2]/td") %>%
                             html_text(),
                           club = spiels %>%
                             html_nodes(xpath = "//*[@id='yw1']/table/tbody//td[5]/a/img") %>%
                             html_attr("alt"),
                           age = spiels %>%
                             html_nodes(xpath = "//*[@id='yw1']/table/tbody//td[3]") %>%
                             html_text() %>%
                             parse_number(),
                           valuemil = spiels %>%
                             html_nodes(xpath = "//*[@id='yw1']/table/tbody//td[6]/b") %>%
                             html_text() %>%
                             str_replace(",([0-9]{2}) Mill.*$", ".\\1"))
    if (i == 1) {
      dfspiel <- dfspieli
    } else {
      dfspiel <- rbind(dfspiel, dfspieli)
    }
  }
  return(dfspiel)
}
top_250_23to30 <- spielervalue("23-30")
top_250_u23 <- spielervalue("u23")
top_250_o30 <- spielervalue("o30")

