# Load source files

   rm(list=ls(all=TRUE))
   packages <- c("lme4", "lmerTest", "ggplot2","dplyr","readxl","MuMIn","GGally","tidyr")
   lapply(packages, library, character.only = TRUE)
   source("scripts/00_setup.R")  # Run from the repository root (.Rproj)

   Top5 <- read_excel("data/raw/Appendix_2a_Top5_18Sept2013.xls", sheet = 1,skip = 11)  # Remove the description lines
   Top5 <- Top5[-1,] #The first line, after reading, was degree

   A_layer <- read_excel("data/raw/Appendix_3a_Ahorizon_18Sept2013.xls", sheet = 1,skip = 11)  # Remove the description lines
   A_layer <- A_layer[-1,] #The first line, after reading, was degree

   C_layer <- read_excel("data/raw/Appendix_4a_Chorizon_18Sept2013.xls", sheet = 1,skip = 11)  # Remove the description lines
   C_layer <- C_layer[-1,] #The first line, after reading, was degree

# Retain cropland sites

   Top5 <- Top5 %>%
        filter(LandCover1 == "Planted/Cultivated")

   A_layer <- A_layer %>%
        filter(LandCover1 == "Planted/Cultivated")

   C_layer <- C_layer %>%
        filter(LandCover1 == "Planted/Cultivated")

   Top5 <- Top5 %>% 
        filter(!LandCover2 %in% c("Urban/Recreational Grasses","Pasture/Hay"))
   A_layer <- A_layer %>% 
        filter(!LandCover2 %in% c("Urban/Recreational Grasses","Pasture/Hay"))
   C_layer <- C_layer %>% 
        filter(!LandCover2 %in% c("Urban/Recreational Grasses","Pasture/Hay"))

# Check horizon alignment before combining by row order.

   stopifnot(
     identical(Top5$SiteID, A_layer$SiteID),
     identical(Top5$SiteID, C_layer$SiteID)
   )

   merged1 <- cbind(Top5, A_layer[, c(9:82)])
   Input_Data <- cbind(merged1, C_layer[, c(9:82)])
 
# GIS attributes (HUC8, pH, LRR, soil order) were extracted in ArcGIS Pro.
   
   TP_GIS <- read_excel("data/gis/TP_GIS_All.xls")

   TP_GIS_selected <- TP_GIS[,c("SiteID","RASTERVALU", "HUC8", "LRRSYM", "TaxOrder")] #RASTERVALU is pH
   
   Input_Data <- Input_Data %>%
     left_join(TP_GIS_selected, by = "SiteID")
   
   Input_Data <- Input_Data %>%
     rename(pH = RASTERVALU,
            LRR = LRRSYM,
            SO = TaxOrder,
            HUC_8 = HUC8)
   
   Input_Data$HUC_8 <- ifelse(substr(Input_Data$HUC_8, 1, 1) == "0",
                             substr(Input_Data$HUC_8, 2, nchar(Input_Data$HUC_8)),
                             Input_Data$HUC_8)   # This is to match that of NuGIS
   
   rm(list = setdiff(ls(), c("Input_Data")))  # Clean workspace

# Aggregate NuGIS inventories for 1-, 10-, and 20-year periods.
   
   NuGIS_HUC8 <- read.csv("data/raw/NuGIS_HUC8_Data.csv")

   NuGIS_long <- NuGIS_HUC8[NuGIS_HUC8$Year <= 2006, ]
   NuGIS_medium <- NuGIS_HUC8[NuGIS_HUC8$Year <= 2006 & NuGIS_HUC8$Year >= 1997 , ]
   NuGIS_short <- NuGIS_HUC8[NuGIS_HUC8$Year == 2006, ]

    NuGIS_long_agr2huc8 <- NuGIS_long %>%
      group_by(HUC_8) %>%
      summarise(
        Psurplus_l = sum(P2O5_PPCA, na.rm = TRUE),
        Fertilizer_l = sum(FarmPPPCA, na.rm = TRUE),
        Manure_l = sum(LbPRecAc, na.rm = TRUE)
      )

    NuGIS_medium_agr2huc8 <- NuGIS_medium %>%
      group_by(HUC_8) %>%
      summarise(
        Psurplus_m = sum(P2O5_PPCA, na.rm = TRUE),
        Fertilizer_m = sum(FarmPPPCA, na.rm = TRUE),
        Manure_m = sum(LbPRecAc, na.rm = TRUE)
      )

    NuGIS_short_agr2huc8 <- NuGIS_short %>%
      group_by(HUC_8) %>%
      summarise(
        Psurplus_s = sum(P2O5_PPCA, na.rm = TRUE),
        Fertilizer_s = sum(FarmPPPCA, na.rm = TRUE),
        Manure_s = sum(LbPRecAc, na.rm = TRUE)
      )

    data_list <- list(NuGIS_long_agr2huc8, NuGIS_medium_agr2huc8, NuGIS_short_agr2huc8)
    
    Input_Data <- Reduce(function(x, y) merge(x, y, by = "HUC_8", all.x = TRUE), 
                         c(list(Input_Data), data_list))
    
    Input_Data <- Input_Data [!is.na(Input_Data $Psurplus_l), ]
    
    rm(list = setdiff(ls(), c("Input_Data")))

# N.S. = no sample; N.D. = not detected; INS = insufficient sample.

    Input_Data <- Input_Data %>%
      filter(SO != " ")
    
    Input_Data <- Input_Data %>%
      filter(C_P != "N.S.")
    
    Input_Data <- Input_Data %>%
      filter(Top5_P != "N.S.")
    
    Input_Data <- Input_Data %>%
      filter(A_C_Tot != "INS")
    
    Input_Data <- Input_Data %>%
      filter(A_C_Inorg != "N.S.")
    
# Replace censored P values (<50) with 25 mg/kg.
    
    Input_Data <- Input_Data %>%
      mutate(C_P = ifelse(C_P == "<50", "25", C_P))
    
    Input_Data <- Input_Data %>%
      mutate(Top5_P = ifelse(Top5_P == "<50", "25", Top5_P))
    
# Handle below-detection concentration values.
    Input_Data$Top5_Ca[Input_Data$Top5_Ca == "<0.01"] <- 0
    
    Input_Data$A_C_Inorg[Input_Data$A_C_Inorg == "N.D."] <- "0.0"
    
    column_names <- colnames(Input_Data)
    
    columns_to_keep <- grep("SiteID|StateID|(_P$|_Al$|_Fe$|_Ca$|_C_Tot$|_C_Inorg$|_C_Org$)", 
                            column_names, value = TRUE)
    
    columns_to_keep <- unique(c(columns_to_keep, tail(column_names, 12)))
    
    Input_Data <- Input_Data[, columns_to_keep]
    
    rm(list = setdiff(ls(), c("Input_Data")))
    
    Input_Data <- Input_Data %>%
      select(-Top5_C_Tot, -Top5_C_Inorg,-Top5_C_Org,
             -C_C_Tot, -C_C_Inorg,-C_C_Org,
             -A_Fe,-A_Ca,-A_Al, -A_P,
             -C_Fe,-C_Ca,-C_Al
             )
    
    exclude_cols <- c("SiteID", "StateID", "LRR", "SO")
    
    Input_Data <- Input_Data %>%
      mutate(across(-all_of(exclude_cols), as.numeric))

    str(Input_Data)  # View the structure of the dataset to confirm
    
    saveRDS(Input_Data, "data/processed/Input_Data.rds")

    