#..................  Graphics script ....................

GET_WB_INCOME <- function(syear=1990, fyear=NULL){ 
  # Set the upper limit for the year
  if(is.null(fyear)) fyear <- AsYear(Sys.Date())
  
  # WB INCOME GROUP DATASET
  URL <- "https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0090754"
  # Where to store it to
  outloc <- "./Data/RawData/WB/INCOME_GROUPS.xlsx"
  # Download it and save it out
  download.file(URL, outloc, mode = "wb")
  
  WB <- read_excel("./Data/RawData/WB/INCOME_GROUPS.xlsx", sheet = "Country Analytical History")[,-2]
  colnames(WB) <- c("ISO3.CODE", WB[5,-1])
  
  WB_INCOME <- WB %>%
    filter(
      !is.na(ISO3.CODE)
    ) %>%
    pivot_longer(
      cols = -ISO3.CODE,
      names_to = "Year",
      values_to = "INCOME_GROUP"
    ) %>%
    mutate(
      INCOME_GROUP = case_when(
        INCOME_GROUP == "L" ~ "Low",
        INCOME_GROUP == "LM" ~ "Lower Middle",
        INCOME_GROUP == "UM" ~ "Upper Middle",
        INCOME_GROUP == "H" ~ "High",
        TRUE ~ INCOME_GROUP
      )
    ) %>%
    filter(
      Year >= syear,
      Year <= fyear
    )
  
  return(WB_INCOME)
}

Get_AgriGDP <- function(syear=1990, fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear <- AsYear(Sys.Date())
  
  # WB INCOME GROUP DATASET
  URL <- "https://bulks-faostat.fao.org/production/Value_of_Production_E_All_Data.zip"
  # Where to store it to
  outloc <- "./Data/RawData/FAOSTAT_AgriGDP.zip"
  # Download it and save it out
  download.file(URL, outloc, mode = "wb")
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
  
  #US CPI 
  CPI_CONV <- GetCPI_USA(syear,fyear)
  
  #COUNTRIES AND ITEMS
  FAOSTAT <- GetFAOSTAT_Prod(syear=syear,fyear=fyear)$yield_data
  
  # Load the data
  AGRI_GDP <- read.csv("./Data/RawData/FAOSTAT_AgriGDP/Value_of_Production_E_All_Data.csv")%>%
  select(
    -matches(
      "F$|N$"
    )
  )%>%
  pivot_longer(
    cols = starts_with("Y"), 
    names_to = "Year",   
    values_to = "Value"  
  )%>%
  mutate(
    Year = as.numeric(str_remove(Year, "^Y")),
    M49 = as.numeric(gsub("'", "", Area.Code..M49.)),
    ISO3.CODE =  countrycode(
      M49, 
      origin = "un",
      destination = "iso3c"
    )
  )%>%
    filter(
      Element == "Gross Production Value (current thousand US$)",
      ISO3.CODE %in% FAOSTAT$ISO3.CODE,
      Item %in% FAOSTAT$Item #only GDP from Items considered in the analysis
    )%>%
    left_join(
      CPI_CONV,
      by = "Year"
    )%>%
    transmute(
      ISO3.CODE,
      Year,
      Item,
      USD_GDP = (Value/CPI_2017)*1000
    )
  
  return(AGRI_GDP)
}

GET_GRAPHICS <- function(FINAL_LOSS, syear = 1990, fyear = NULL){
  
  if(is.null(fyear)) fyear <- AsYear(Sys.Date())
  
  GDP <- Get_AgriGDP(syear,fyear)
  
  FIGURE1 <- FINAL_LOSS %>%
    group_by(
      Year
    ) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE),
      Lower = sum(LowerBound, na.rm = TRUE),
      Upper = sum(UpperBound, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = Year, y = Loss)) +
    
    geom_ribbon(
      aes(ymin = Lower, ymax = Upper, x = Year), 
      fill = "#5b9dcf",   
      alpha = 0.3     
    )+
    
    geom_line(size = 1,colour = "#5b9dcf") +
    
    geom_point(colour = "black", size = 1)  + 
    
    geom_line(aes(y = Lower), linetype = "dotted", size = 0.3, alpha = 0.3) +  
    geom_line(aes(y = Upper), linetype = "dotted", size = 0.3, alpha = 0.3) +  
    
    labs(
      x = "Year",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters (1991-2022)"
    ) +
    
    scale_y_continuous(
      labels = function(x) paste0(x / 1e9, " bill."),
      limits = c(-30e9, 280e9)
    )+
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7,size=1)+
    
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black")
    )
  
  ggsave(
    "Plots/FIGURE1.png",
    FIGURE1,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  FIGURE2 <- FINAL_LOSS%>%
    group_by(unsd_macro_reg) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE),
      Lower = sum(LowerBound, na.rm = TRUE),
      Upper = sum(UpperBound, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = reorder(unsd_macro_reg, -Loss), y = Loss)) +
    
    geom_bar(stat = "identity", fill = "#5b9dcf") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7, size = 1)+
    
    labs(
      x = "Region",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters (1991-2022) by region"
    ) +
    
    scale_y_continuous(breaks = y_breaks2, labels = y_labels2, limits = c(0, 2e12)) +
    
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 8, angle = 45,vjust = 0.5) 
    )
  
  ggsave(
    "Plots/FIGURE2.png",
    FIGURE2,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  y_breaks3 <- c(-10,-5,0,5,10,15,20,25)
  y_labels3 <- c("-10%", "-5%", "0%", "5%", "10%","15%","20%","25%")
  
  FIGURE3 <- GDP %>%
    filter(
      Area %in% unique(LOSSES_GENERAL$unsd_macro_reg),
      Year >= syear
    ) %>%
    group_by(Area) %>%
    summarise(
      TOT_GDP = sum(Prod_Value, na.rm = TRUE)
    ) %>%
    left_join(
      FINAL_LOSS %>%
        group_by(unsd_macro_reg) %>%  
        summarise(
          Loss = sum(value_lost, na.rm = TRUE),
          Lower = sum(LowerBound, na.rm = TRUE),  
          Upper = sum(UpperBound, na.rm = TRUE)
        ),
      by = c("Area" = "unsd_macro_reg")
    ) %>%
    mutate(
      Perc_loss = (Loss / TOT_GDP) * 100,
      Perc_lower = (Lower / TOT_GDP) * 100, 
      Perc_upper = (Upper / TOT_GDP) * 100   
    ) %>%
    ggplot(aes(x = reorder(Area, -Perc_loss), y = Perc_loss)) +
    geom_bar(stat = "identity", fill = "#5b9dcf") +
    geom_errorbar(aes(ymin = Perc_lower, ymax = Perc_upper), width = 0.2) +
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7, size = 1)+
    labs(
      x = "Region",
      y = "Percentage loss of GDP (%)",
      title = "Percentage of GDP lost due to disasters (1991-2023) by region"
    ) +
    scale_y_continuous(breaks = y_breaks3, labels = y_labels3) +
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 8, angle = 45,vjust = 0.5) 
    )
  
  ggsave(
    "Plots/FIGURE3.png",
    FIGURE3,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  
  regions <- c("Africa", "Americas", "Europe", "Asia", "Oceania")
  
  for (region in regions) {
    
    data_region <- FINAL_LOSS %>%
      group_by(Year, unsd_macro_reg) %>%
      summarise(
        Loss = sum(value_lost, na.rm = TRUE),
        Lower = sum(LowerBound, na.rm = TRUE),
        Upper = sum(UpperBound, na.rm = TRUE)
      ) %>%
      filter(unsd_macro_reg == region)
    
    y_min <- min(data_region$Lower, na.rm = TRUE)
    y_max <- max(data_region$Upper, na.rm = TRUE)
    
    plot <- ggplot(data_region, aes(x = Year, y = Loss, colour = unsd_macro_reg)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#5b9dcf", colour = NA) +
      geom_line(size = 1, alpha = 0.6, colour = "#5b9dcf") +
      geom_point(colour = "black", size = 1) +
      geom_hline(yintercept = 0, colour = "red", size = 1, alpha = 0.7) +
      labs(
        x = "Year",
        y = "Monetary losses (2017 PPP USD)",
        title = paste("Monetary losses on agriculture due to disasters in", region, "(1991-2022)"),
        colour = "Region",
        fill = "Region"
      ) +
      scale_y_continuous(
        limits = c(min(y_min,0), y_max),
        labels = function(x) paste0(x / 1e9, " bill.")
      ) +
      theme(
        plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
        axis.text = element_text(family = "Arial", size = 10),
        axis.title = element_text(family = "Arial", size = 11),
        axis.line = element_line(color = "black"),
        legend.position = "none"
      ) 
    
    ggsave(
      filename = paste0("Plots/FIGURE4_", toupper(region), ".png"),
      plot = plot,
      scale = 1,
      width = 20, height = 20, units = "cm"
    )
  }
  
  for (region in regions) {
    
    data_region <- FINAL_LOSS %>%
      group_by(Year, unsd_macro_reg) %>%
      summarise(
        Loss = sum(value_lost, na.rm = TRUE),
        Lower = sum(LowerBound, na.rm = TRUE),
        Upper = sum(UpperBound, na.rm = TRUE)
      ) %>%
      filter(unsd_macro_reg == region) %>%
      group_by(unsd_macro_reg) %>% 
      mutate(
        moving_avg3 = rollmean(Loss, k = 3, fill = NA, align = 'right'),
        moving_avg5 = rollmean(Loss, k = 5, fill = NA, align = 'right'),
        Lower_avg = rollmean(Lower, k = 5, fill = NA, align = 'right'),
        Upper_avg = rollmean(Upper, k = 5, fill = NA, align = 'right')
      ) %>%
      filter(Year >= syear+4)
    
    y_min <- min(data_region$Lower_avg, na.rm = TRUE)
    y_max <- max(data_region$Upper_avg, na.rm = TRUE)
    
    plot <- ggplot(data_region, aes(x = Year, y = moving_avg5, colour = unsd_macro_reg)) +
      geom_ribbon(aes(ymin = Lower_avg, ymax = Upper_avg), alpha = 0.2, fill = "#5b9dcf", colour = NA) +
      geom_line(size = 1, colour = "#5b9dcf") +
      geom_point(colour = "black", size = 1) +
      geom_hline(yintercept = 0, color = "red", size = 1, alpha = 0.7) +  
      labs(
        x = "Year",
        y = "Monetary losses (2017 PPP USD)",
        title = paste("Monetary losses on agriculture due to disasters in", region, "(",syear+4,"-",fyear,")"),
        colour = "Region",
        subtitle = "5 years moving average with confidence intervals"
      ) +
      scale_y_continuous(
        limits = c(min(y_min,0), y_max),
        labels = function(x) paste0(x / 1e9, " bill.")  
      ) +
      theme(
        plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
        plot.subtitle = element_text(family = "Arial", face = "bold"),
        legend.position = "bottom",  
        axis.text = element_text(family = "Arial", size = 10), 
        axis.title = element_text(family = "Arial", size = 11),
        axis.line = element_line(color = "black")
      ) + theme(legend.position = "none") 
    
    ggsave(
      filename = paste0("Plots/FIGURE5_", toupper(region), ".png"),
      plot = plot,
      scale = 1,
      width = 20, height = 20, units = "cm"
    )
  }
  
  for (region in regions) {
    
    data_region <- FINAL_LOSS %>%
      group_by(Year, unsd_macro_reg) %>%
      summarise(
        Loss = sum(value_lost, na.rm = TRUE),
        Lower = sum(LowerBound, na.rm = TRUE),
        Upper = sum(UpperBound, na.rm = TRUE)
      ) %>%
      filter(unsd_macro_reg == region) %>%
      group_by(unsd_macro_reg) %>% 
      mutate(
        moving_avg3 = rollmean(Loss, k = 3, fill = NA, align = 'right'),
        Lower_avg = rollmean(Lower, k = 3, fill = NA, align = 'right'),
        Upper_avg = rollmean(Upper, k = 3, fill = NA, align = 'right')
      ) %>%
      filter(Year >= syear + 2)
    
    y_min <- min(data_region$Lower_avg, na.rm = TRUE)
    y_max <- max(data_region$Upper_avg, na.rm = TRUE)
    
    plot <- ggplot(data_region, aes(x = Year, y = moving_avg3, colour = unsd_macro_reg)) +
      geom_ribbon(aes(ymin = Lower_avg, ymax = Upper_avg), alpha = 0.2, fill = "#5b9dcf", colour = NA) +
      geom_line(size = 1, colour = "#5b9dcf") +
      geom_point(colour = "black", size = 1) +
      geom_hline(yintercept = 0, color = "red", size = 1, alpha = 0.7) +  
      labs(
        x = "Year",
        y = "Monetary losses (2017 PPP USD)",
        title = paste("Monetary losses on agriculture due to disasters in", region, "(",syear+2,"-",fyear,")"),
        colour = "Region",
        subtitle = "3 years moving average with confidence intervals"
      ) +
      scale_y_continuous(
        limits = c(min(0,y_min), y_max),
        labels = function(x) paste0(x / 1e9, " bill.")  
      ) +
      theme(
        plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
        plot.subtitle = element_text(family = "Arial", face = "bold"),
        legend.position = "bottom",  
        axis.text = element_text(family = "Arial", size = 10), 
        axis.title = element_text(family = "Arial", size = 11),
        axis.line = element_line(color = "black")
      ) + theme(legend.position = "none") 
    
    ggsave(
      filename = paste0("Plots/FIGURE6_", toupper(region), ".png"),
      plot = plot,
      scale = 1,
      width = 20, height = 20, units = "cm"
    )
  }
  
  for (region in regions) {
    
    GDP_REGIONS2 <- GDP %>%
      filter(
        Area %in% unique(FINAL_LOSS$unsd_macro_reg),
        Year >= 1991
      ) %>%
      group_by(Area, Year) %>%
      summarise(TOT_GDP = sum(Prod_Value, na.rm = TRUE)) %>%
      left_join(
        FINAL_LOSS %>%
          group_by(unsd_macro_reg, Year) %>%
          summarise(
            Loss = sum(value_lost, na.rm = TRUE),
            Lower = sum(LowerBound, na.rm = TRUE),
            Upper = sum(UpperBound, na.rm = TRUE)
          ),
        by = c("Area" = "unsd_macro_reg", "Year")
      ) %>%
      mutate(
        Perc_loss = (Loss / TOT_GDP) * 100,
        Lower_perc = (Lower / TOT_GDP) * 100,
        Upper_perc = (Upper / TOT_GDP) * 100
      ) %>%
      filter(Area == region)
    
    y_min <- min(GDP_REGIONS2$Lower_perc, na.rm = TRUE)
    y_max <- max(GDP_REGIONS2$Upper_perc, na.rm = TRUE)
    
    plot <- GDP_REGIONS2 %>%
      ggplot(aes(x = Year, y = Perc_loss, colour = Area)) +
      geom_ribbon(aes(ymin = Lower_perc, ymax = Upper_perc), alpha = 0.2, fill = "#5b9dcf", colour = NA) +
      geom_line(size = 1, alpha = 0.7,colour = "#5b9dcf") +
      geom_point(colour = "black", size = 1) +
      geom_hline(yintercept = 0, color = "red", size = 1, alpha = 0.7) +  
      labs(
        x = "Year",
        y = "% of losses",
        title = paste("Losses as a share of agricultural GDP in", region, "(1991-2022)"),
        colour = "Region"
      ) +
      scale_y_continuous(
        limits = c(min(y_min,0), y_max),
        labels = function(x) paste0(x, " %")
      ) +
      theme(
        plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
        plot.subtitle = element_text(family = "Arial", face = "bold"),
        legend.position = "bottom",  
        axis.text = element_text(family = "Arial", size = 10), 
        axis.title = element_text(family = "Arial", size = 11),
        axis.line = element_line(color = "black")
      ) + theme(legend.position = "none") 
    
    ggsave(
      filename = paste0("Plots/FIGURE7_", toupper(region), ".png"),
      plot = plot,
      scale = 1,
      width = 20, height = 20, units = "cm"
    )
  }
  
  for (region in regions) {
    
    GDP_REGIONS3 <- GDP %>%
      filter(
        Area %in% unique(FINAL_LOSS$unsd_sub_reg),
        Year >= 1991
      ) %>%
      group_by(Area, Year) %>%
      summarise(TOT_GDP = sum(Prod_Value, na.rm = TRUE)) %>%
      left_join(
        FINAL_LOSS %>%
          group_by(unsd_sub_reg, Year) %>%
          summarise(
            Loss = sum(value_lost, na.rm = TRUE),
            Lower = sum(LowerBound, na.rm = TRUE),
            Upper = sum(UpperBound, na.rm = TRUE)
          ),
        by = c("Area" = "unsd_sub_reg", "Year")
      ) %>%
      mutate(
        Perc_loss = (Loss / TOT_GDP) * 100,
        Lower_perc = (Lower / TOT_GDP) * 100,
        Upper_perc = (Upper / TOT_GDP) * 100
      ) %>%
      left_join(
        country_region %>%
          group_by(unsd_sub_reg, unsd_macro_reg) %>%
          slice(1),
        by = c("Area" = "unsd_sub_reg")
      ) %>%
      filter(unsd_macro_reg == region)
    
    y_min <- min(GDP_REGIONS3$Lower_perc, na.rm = TRUE)
    y_max <- max(GDP_REGIONS3$Upper_perc, na.rm = TRUE)
    
    plot <- GDP_REGIONS3 %>%
      ggplot(aes(x = Year, y = Perc_loss, colour = Area, group = Area)) + 
      geom_ribbon(aes(ymin = Lower_perc, ymax = Upper_perc, fill = Area), alpha = 0.2, colour = NA) + 
      geom_line(size = 1, alpha = 0.7) +
      geom_point(colour = "black", size = 1) +
      geom_hline(yintercept = 0, color = "red", size = 1, alpha = 0.7) +  
      labs(
        x = "Year",
        y = "% of losses",
        title = paste("Losses as a share of agricultural GDP in", region, "by subregion and year (1991-2022)"),
        colour = "Subregion",
        fill = "Subregion"
      ) +
      scale_y_continuous(
        limits = c(y_min, y_max),
        labels = function(x) paste0(x, " %")
      ) +
      coord_cartesian(ylim = c(0, 50))+
      theme(
        plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
        plot.subtitle = element_text(family = "Arial", face = "bold"),
        legend.position = "bottom",  
        axis.text = element_text(family = "Arial", size = 10), 
        axis.title = element_text(family = "Arial", size = 11),
        axis.line = element_line(color = "black")
      )
    
    ggsave(
      filename = paste0("Plots/FIGURE8_", toupper(region), ".png"),
      plot = plot,
      scale = 1,
      width = 22, height = 20, units = "cm"
    )
  }
  
  FIGURE9 <- FINAL_LOSS %>%
    group_by(Year, INCOME_GROUP) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE)
    ) %>%
    left_join(
      FINAL_LOSS %>%
        group_by(Year) %>%
        summarise(
          Lower = sum(LowerBound, na.rm = TRUE),
          Upper = sum(UpperBound, na.rm = TRUE)
        ),
      by = c("Year")
    ) %>%
    filter(!is.na(INCOME_GROUP)) %>%
    ggplot(aes(x = Year, y = Loss, fill = INCOME_GROUP)) +
    geom_hline(yintercept = 0, colour = "red")+
    geom_bar(stat = "identity", position = "stack") +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) + 
    labs(
      x = "Year",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters by income group and year (1991-2022)",
      fill = "Income group"
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x / 1e9, " bill.")
    )  +
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      legend.position = "bottom",
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black")
    ) 
  
  ggsave(
    filename = "Plots/FIGURE9.png",
    plot = FIGURE9,
    scale = 1,
    width = 22, height = 20, units = "cm"
  )
  
  FIGURE10 <- FINAL_LOSS %>%
    group_by(INCOME_GROUP) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE),
      Lower = sum(LowerBound, na.rm = TRUE),  
      Upper = sum(UpperBound, na.rm = TRUE)
    ) %>%
    filter(!is.na(INCOME_GROUP)) %>%
    ggplot(aes(x = reorder(INCOME_GROUP, -Loss), y = Loss)) +
    geom_bar(
      stat = "identity",
      position = "stack",
      fill = "#5b9dcf"
    ) +
    geom_hline(yintercept = 0, color = "red")+
    geom_errorbar(
      aes(ymin = Lower, ymax = Upper),
      width = 0.2
    ) + 
    labs(
      x = "Income group",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters by income group (1991-2022)"
    ) +
    scale_y_continuous(
      limits = c(0, 1.5e12),
      labels = function(x) paste0(x / 1e12, " trill.")  
    ) +
    theme(
      plot.title = element_text(
        family = "Arial",
        size = 15,
        face = "bold",
        color = "#5b9dcf"
      ),
      legend.position = "bottom",
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black")
    )
  
  ggsave(
    filename = "Plots/FIGURE10.png",
    plot = FIGURE10,
    scale = 1,
    width = 22, height = 20, units = "cm"
  )
  
  GDP_INCOME <- GDP%>%
    filter(
      Year >= 1991
    )%>%
    mutate(
      UN_CODE = as.numeric(str_remove(UN_CODE,"'"))
    )%>%
    left_join(
      FAOCOUNTRIES%>% #This should better be changed using countrycode() function.
        transmute(
          ISO3_CODE,
          UN_CODE
        ),
      by = "UN_CODE"
    )%>%
    left_join(
      Income_groups_2022,
      by = c("ISO3_CODE","Year")
    )%>%
    mutate(
      INCOME_GROUP = case_when(
        ISO3_CODE == "COK" | ISO3_CODE == "GLP" | ISO3_CODE == "MTQ" | ISO3_CODE == "NIU" ~ "SIDS",
        ISO3_CODE == "GUF" | ISO3_CODE == "REU" | ISO3_CODE == "TKL" ~ "High", #French guyana is assigned France group by WB classification
        #Reunion is also french
        #TOKELAU I considered it part of New Zealand
        ISO3_CODE == "SCG" ~ "Lower Middle", #Serbia and Montenegro was considered low middle
        TRUE ~ INCOME_GROUP                            
      )
    )%>%replace_na(
      list(
        INCOME_GROUP = "NC"
      )
    )
  
  LOSSES_BY_INCOME_GDP <- GDP_INCOME %>%
    group_by(INCOME_GROUP, Year) %>%
    summarise(
      GDP = sum(Prod_Value, na.rm = TRUE)
    ) %>%
    left_join(
      FINAL_LOSS %>%
        group_by(INCOME_GROUP, Year) %>%  
        summarise(
          Loss = sum(value_lost, na.rm = TRUE),
          Lower = sum(LowerBound, na.rm = TRUE),
          Upper = sum(UpperBound, na.rm = TRUE)
        ),
      by = c("INCOME_GROUP", "Year")
    ) %>%
    mutate(
      Perc_loss = 100 * (Loss / GDP),
      Perc_lower = 100 * (Lower / GDP),  
      Perc_upper = 100 * (Upper / GDP)  
    )
  
  
  FIGURE11 <- LOSSES_BY_INCOME_GDP %>%
    ggplot(aes(x = Year, y = Perc_loss, colour = reorder(INCOME_GROUP,-Perc_loss))) +
    geom_line(size = 1, alpha = 0.7) +  
    geom_point(colour = "black", size = 1) +  
    geom_ribbon(aes(ymin = Perc_lower, ymax = Perc_upper, fill = reorder(INCOME_GROUP,-Perc_loss)), alpha = 0.2) + 
    labs(
      x = "Year",
      y = "Percentage loss",
      title = "Agricultural GDP losses by income group (1991-2022)",
      colour = "Income group"
    ) +
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      legend.position = "bottom",
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black")
    ) +
    guides(
      fill =  guide_legend(title = "Income group") , 
      color = "none"
    )+
    coord_cartesian(ylim = c(-5, 50))+
    geom_hline(yintercept = 0, color = "red")
  
  ggsave(
    "Plots/FIGURE11.png",
    FIGURE11,
    scale = 1,
    width = 22, height = 20, units = "cm"
  ) 
  
  gdp_data <- GDP %>%
    filter(
      Year >= 1991,
      Area %in% unique(LOSSES_GENERAL$unsd_sub_reg)
    ) %>%
    group_by(Area) %>%
    summarise(GDP = sum(Prod_Value, na.rm = TRUE))
  
  losses_data <- FINAL_LOSS %>%
    group_by(unsd_sub_reg) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE),
      Lower = sum(LowerBound, na.rm = TRUE),
      Upper = sum(UpperBound, na.rm = TRUE)
    )
  
  fig_data <- gdp_data %>%
    left_join(losses_data, by = c("Area" = "unsd_sub_reg")) %>%
    mutate(
      Perc_loss = 100 * (Loss / GDP),
      Perc_Low = 100*(Lower/GDP),
      Perc_Up = 100*(Upper/GDP)
    )%>%
    left_join(
      FINAL_LOSS%>%
        group_by(
          unsd_macro_reg,
          unsd_sub_reg
        )%>%slice(1)%>%transmute(unsd_macro_reg,unsd_sub_reg),
      by = c("Area" = "unsd_sub_reg")
    )
  
  FIGURE12 <- fig_data %>%
    ggplot(aes(x = reorder(Area, -Perc_loss), y = Perc_loss, fill = unsd_macro_reg)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Perc_Low, ymax = Perc_Up), width = 0.2) +
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7, size = 1) +
    
    labs(
      x = "Subregion",
      y = "% of losses",
      title = "Losses as a share of agricultural GDP by subregion (1991-2022) with confidence bounds",
      fill = "BOUND"
    ) +
    
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_cartesian(ylim = c(-1, 20))
  
  ggsave(
    "Plots/FIGURE12.png",
    FIGURE12,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  FIGURE13 <- FINAL_LOSS %>%
    group_by(unsd_macro_reg, Year) %>%
    summarise(Loss = sum(value_lost, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = Loss, fill = unsd_macro_reg)) +
    
    geom_bar(position = "stack", stat = "identity") +
    
    geom_errorbar(data = totals, aes(x = Year, ymin = Lower, ymax = Upper), 
                  inherit.aes = FALSE, width = 0.2) +
    
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7, size = 1) +
    
    labs(
      x = "Year",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters (1991-2022) by region",
      fill = "Region"
    )  +
    coord_cartesian(ylim = c(0, 3e+11))  +
    scale_y_continuous(
      labels = function(x) paste0(x / 1e9, " bill.")
    ) +
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5) 
    ) 
  
  
  ggsave(
    "Plots/FIGURE13.png",
    FIGURE13,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  
  data_region <- FINAL_LOSS %>%
    group_by(Year, unsd_macro_reg) %>%
    summarise(
      Loss = sum(value_lost, na.rm = TRUE),
      Lower = sum(LowerBound, na.rm = TRUE),
      Upper = sum(UpperBound, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(unsd_macro_reg) %>%
    mutate(
      moving_avg5 = rollmean(Loss, k = 5, fill = NA, align = 'right'),
      Lower_avg = rollmean(Lower, k = 5, fill = NA, align = 'right'),
      Upper_avg = rollmean(Upper, k = 5, fill = NA, align = 'right')
    ) %>%
    filter(Year >= fyear + 4)
  
  y_min <- min(data_region$Lower_avg, na.rm = TRUE)
  y_max <- max(data_region$Upper_avg, na.rm = TRUE)
  
  FIGURE14 <- ggplot(data_region, aes(x = Year, y = moving_avg5, colour = unsd_macro_reg)) +
    geom_ribbon(aes(ymin = Lower_avg, ymax = Upper_avg, fill = unsd_macro_reg), alpha = 0.2) +
    geom_line(size = 1) +
    geom_point(colour = "black", size = 1) +
    geom_hline(yintercept = 0, color = "red", size = 1, alpha = 0.7) +
    labs(
      x = "Year",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters (1995-2022)",
      subtitle = "5 years moving average with confidence intervals"
    )  +
    coord_cartesian(ylim = c(0, 80e+09))+
    scale_y_continuous(
      labels = function(x) paste0(x / 1e9, " bill.")
    ) +
    
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      plot.subtitle = element_text(family = "Arial", face = "bold"),
      legend.position = "bottom",  
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)
    ) 
  
  ggsave(
    "Plots/FIGURE14.png",
    FIGURE14,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
  
  
  REG <- FINAL_LOSS%>%
    ungroup()%>%
    transmute(unsd_macro_reg,unsd_sub_reg)%>%
    group_by(unsd_sub_reg,unsd_macro_reg)%>%
    slice(1)
  
  FIGURE15 <- FINAL_LOSS %>%
    filter(BOUND == "VL95")%>%
    group_by(unsd_sub_reg) %>%
    summarise(Loss = sum(value_lost, na.rm = TRUE),
              Lower = sum(LowerBound, na.rm = TRUE),
              Upper = sum(UpperBound, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(REG,by = "unsd_sub_reg")%>%
    ggplot(aes(x = unsd_sub_reg, y = Loss, fill = unsd_macro_reg)) +
    
    geom_bar(stat = "identity") +
    
    geom_errorbar(aes(x = reorder(unsd_sub_reg,-Loss), ymin = Lower, ymax = Upper), width = 0.2) +
    
    geom_hline(yintercept = 0, colour = "red", alpha = 0.7, size = 1) +
    
    labs(
      x = "Year",
      y = "Monetary losses (2017 PPP USD)",
      title = "Monetary losses on agriculture due to disasters (1991-2022) by subregion",
      fill = "Region"
    ) +
    
    coord_cartesian(ylim = c(0, 856285174491)) +
    
    theme(
      plot.title = element_text(family = "Arial", size = 15, face = "bold", color = "#5b9dcf"),
      axis.text = element_text(family = "Arial", size = 10),
      axis.title = element_text(family = "Arial", size = 11),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5) 
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x / 1e9, " bill.")
    ) 
  
  ggsave(
    "Plots/FIGURE15.png",
    FIGURE15,
    scale = 1,
    width = 20, height = 20, units = "cm"
  )
}
