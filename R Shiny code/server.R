function(input, output, session){

# Affichage général -------------------------------------------------------
  # graphe représentant le NPS et le CA superposés par lot
  output$plot_NPS_CA_superpose_T_facet <- renderPlot({
    lot_labels <- c("Enquête 1", "Enquête 2", "Enquête 3")
    
    data <- data_nps_ca_count %>% group_by(DATEenvoi, Lot) %>% summarise(count_total = sum(Detracteur_count) + sum(Neutre_count) + sum(Promoteur_count), Detracteur_count = sum(Detracteur_count)*100/count_total, Promoteur_count = sum(Promoteur_count)*100/count_total, NPS = Promoteur_count - Detracteur_count, CA = sum(CA)/count_total)
    
    ggplot(data) +
      geom_line(aes(x=DATEenvoi, y=NPS, colour = "#00BFC4")) +
      geom_line(aes(x=DATEenvoi, y=CA/5, colour = "#F8766D")) +  # changement d'échelle du CA pour avoir des graphes avec des échelles cohérentes
      scale_y_continuous(name = "Net Promoter Score",
                         sec.axis = sec_axis(~.*5, name = "Chiffre d'affaires par client (en € par mois)")) +
      theme(axis.title.y.left = element_text(color = "#F8766D"), axis.text.y.left = element_text(color = "#F8766D")) +
      theme(axis.title.y.right = element_text(color = "#00BFC4"), axis.text.y.right = element_text(color = "#00BFC4")) +
      theme(legend.position='none') +
      facet_wrap(~ factor(Lot, labels = lot_labels)) +
      labs(x = "Date")
  })
  
  
  # graphe en barres du NPS pour chaque lot, détaillé par produit
  output$plot_hist_NPS <- renderPlot({
    typquest_labels <- c("Questionnaire 1", "Questionnaire 2", "Questionnaire 3", "Questionnaire 4", "Questionnaire 5")
    
    date_range_1 <- paste(substr(as.character(input$date_range_1),1,8),"01", sep="")
    date_range_1 <- as.character(date_range_1)
    date_range_1 <- as.Date.character(date_range_1)
    date_1 <- date_range_1[1]
    date_2 <- date_range_1[2]
    
    if (length(input$lot_input_1) != 0){
      data <- data_nps_ca_count
      data$Typquest <- factor(data$Typquest, labels = typquest_labels)
      data <- data[data$DATEenvoi >= date_1 & data$DATEenvoi <= date_2,] 
      data <- data[data$Lot %in% input$lot_input_1,] %>% group_by(Typquest) %>% summarise(count_total = sum(Detracteur_count) + sum(Neutre_count) + sum(Promoteur_count),NPS = (sum(Promoteur_count) - sum(Detracteur_count))*100/count_total, CA = sum(CA)/count_total)
      
      ggplot(data, aes(x=factor(Typquest), y=NPS)) +
        geom_bar(aes(fill=factor(NPS)), stat="identity") +
        scale_fill_manual(values = case_when(sort(data$NPS) >= 0 ~ "#00D223", TRUE ~ "#EA0000")) +
        geom_text(aes(label=paste(round(CA,0), " €/client",sep="")), vjust=case_when(data$NPS > 3 ~ 1.3, TRUE ~ -1), color=case_when(abs(data$NPS) > 3 ~ "white", TRUE ~ "black"), size=5) +
        theme(legend.position = "none") +
        labs(subtitle = "NPS = % clients promoteurs - % clients détracteurs",
             x = "Enquêtes clients",
             y = "Score NPS") 
    }
  })
  
  
  # graphe représentant le R2 ajusté pour les modèles de régression linéaire de chaque lot
  output$plot_hist_R2 <- renderPlot({
    lot_labels <- c("Enquête 1", "Enquête 2", "Enquête 3")
    
    ggplot(data_r2, aes(x=factor(Lot), y=R2)) +
      geom_point(aes(color = R2), stat="identity", size = 7) +
      scale_x_discrete(labels = lot_labels) +
      scale_color_gradient2(low = "#FF4000", mid = "yellow", high = "#04B404", midpoint = mean(data_r2$R2), limits=c(min(data_r2$R2), 0.75)) +
      geom_hline(yintercept = mean(data_r2$R2), colour = "red", size = 1, linetype="dashed") +
      geom_text(aes(x= factor(Lot)[1], y = mean(R2) - 0.05), label = paste("Moyenne = ", round(mean(data_r2$R2),2), sep=""), size = 5, color = "red") +
      expand_limits(y=c(0,1)) +
      theme(legend.position = "none") +
      labs(subtitle = "Coefficient de corrélation R2 entre le NPS et le CA",
           x = "Enquêtes clients",
           y = "R2 ajusté")
  })
  
  
  # graphe en barres représentant le pourcentage de clients détracteurs, neutres et promoteurs pour chaque lot, et détaillé par produit
  # sur les barres, il est possible d'afficher le montant moyen dépensé par mois par les clients, le panier moyen des clients, ou leur fréquence moyenne d'achat par mois
  output$plot_camembert_hist_DNP <- renderPlot({
    date_range_2 <- paste(substr(as.character(input$date_range_2),1,8),"01", sep="")
    date_range_2 <- as.character(date_range_2)
    date_range_2 <- as.Date.character(date_range_2)
    date_1 <- date_range_2[1]
    date_2 <- date_range_2[2]
    
    typquest_labels <- c("Questionnaire 1", "Questionnaire 2", "Questionnaire 3", "NQuestionnaire 4", "Questionnaire 5")

    
    if(length(input$lot_input_2) != 0){
      data <- data_nps_ca_dnp
      data$Typquest <- factor(data$Typquest, labels = typquest_labels)
      data <- data[data$DATEenvoi >= date_1 & data$DATEenvoi <= date_2,]
      data <- data[data$Lot %in% input$lot_input_2,] %>% group_by(Typquest, Client_group) %>% summarise(count = sum(count), CA_client = mean(CA_client), panier = mean(panier), frequence = mean(frequence), percentage = 0, percentage_cum = 0)
      data <- drop_na(data)
      
      for (i in unique(data$Typquest)){
        data[data$Typquest == i & data$Client_group == "Promoteur",]$percentage <- data[data$Typquest == i & data$Client_group == "Promoteur",]$count / sum(data[data$Typquest == i,]$count)
        data[data$Typquest == i & data$Client_group == "Neutre",]$percentage <- data[data$Typquest == i & data$Client_group == "Neutre",]$count / sum(data[data$Typquest == i,]$count)
        data[data$Typquest == i & data$Client_group == "Detracteur",]$percentage <- data[data$Typquest == i & data$Client_group == "Detracteur",]$count / sum(data[data$Typquest == i,]$count)
        data[data$Typquest == i & data$Client_group == "Promoteur",]$percentage_cum <- data[data$Typquest == i & data$Client_group == "Promoteur",]$percentage
        data[data$Typquest == i & data$Client_group == "Neutre",]$percentage_cum <- data[data$Typquest == i & data$Client_group == "Promoteur",]$percentage + data[data$Typquest == i & data$Client_group == "Neutre",]$percentage
        data[data$Typquest == i & data$Client_group == "Detracteur",]$percentage_cum <- data[data$Typquest == i & data$Client_group == "Promoteur",]$percentage + data[data$Typquest == i & data$Client_group == "Neutre",]$percentage + data[data$Typquest == i & data$Client_group == "Detracteur",]$percentage
      }
      
      ggplot(data, aes(factor(Typquest), weight = count)) +
        geom_bar(aes(fill = Client_group), position="fill") +
        #scale_y_continuous(labels=percent) +
        scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0 %", "25 %", "50 %", "75%", "100 %")) +
        scale_fill_manual(values = c("#F13E3E","#FFBF00","#00BA38")) +
        geom_text(aes(label = case_when(input$indicateur_dnp == "CA" ~ paste(as.character(round(CA_client,0)), " €/client", sep=""), 
                                        input$indicateur_dnp == "panier" ~ paste(as.character(round(panier,0)), " €/client", sep=""),
                                        TRUE ~ paste(as.character(round(frequence,2)), " tickets/mois", sep="")), y = percentage_cum), vjust=15*data$percentage, color="#333333", size=5) +
        labs(subtitle = "Valeurs en pourcentage",
             fill = "Type de client",
             x = "Groupe de clients",
             y = "")
    }
  })
  
  
  # histogramme représentant le chiffre d'affaire moyen par mois des clients ayant mis chaque note de satisfaction
  # sur les barres, on trouve également le nombre de clients ayant mis chaque note de satisfaction
  output$plot_hist_satisfaction <- renderPlot({
    typquest_labels <- c("Questionnaire 1", "Questionnaire 2", "Questionnaire 3", "Questionnaire 4", "Questionnaire 5")
    
    if(length(input$lot_input_3) != 0){
      date_range_3 <- paste(substr(as.character(input$date_range_3),1,8),"01", sep="")
      date_range_3 <- as.character(date_range_3)
      date_range_3 <- as.Date.character(date_range_3)
      date_1 <- date_range_3[1]
      date_2 <- date_range_3[2]
      
      data <- data_nps_ca
      data$Typquest <- factor(data$Typquest, labels = typquest_labels)
      data <- data[data$DATEenvoi >= date_1 & data$DATEenvoi <= date_2,] 
      data <- data[data$Lot %in% input$lot_input_3,] %>% group_by(Satisfaction) %>% summarise(count_total = sum(Detracteur) + sum(Neutre) + sum(Promoteur), CA = sum(CA)/count_total)
      
      ggplot(data, aes(x = factor(Satisfaction), y = CA, fill = factor(Satisfaction))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values=c("#EA0000","#FFBF00","#00D223","#00D223")) +
        expand_limits(y=c(0,max(data$CA) + 30)) +
        theme(legend.position = "none") +
        geom_text(aes(label= paste(as.character(format(count_total, big.mark = " ")), " clients", sep="")), vjust = -0.3, color="black", size=4) +
        labs(subtitle = "Êtes-vous satisfait(e) des produits et / ou services proposés par votre magasin ?  \n\nNotes sur une échelle de 1 (pas du tout satisfait) à 4 (très satisfait)",
             x = "Notes des clients",
             y = "Chiffre d'affaire par client (en € par mois)") 
    }
  })
  
  
  # histogramme représentant le chiffre d'affaire moyen par mois des clients ayant mis chaque note de recommandation
  # sur les barres, on trouve également le nombre de clients ayant mis chaque note de recommandation
  output$plot_hist_recommandation <- renderPlot({
    typquest_labels <- c("Questionnaire 1", "Questionnaire 2", "Questionnaire 3", "Questionnaire 4", "Questionnaire 5")
    
    if (length(input$lot_input_4) != 0){
      date_range_4 <- paste(substr(as.character(input$date_range_4),1,8),"01", sep="")
      date_range_4 <- as.character(date_range_4)
      date_range_4 <- as.Date.character(date_range_4)
      date_1 <- date_range_4[1]
      date_2 <- date_range_4[2]
      
      data <- data_nps_ca
      data$Typquest <- factor(data$Typquest, labels = typquest_labels)
      data <- data[data$DATEenvoi >= date_1 & data$DATEenvoi <= date_2,] 
      data <- data[data$Lot %in% input$lot_input_4,] %>% group_by(Recommandation) %>% summarise(count_total = sum(Detracteur) + sum(Neutre) + sum(Promoteur), CA = sum(CA)/count_total)
      
      ggplot(data, aes(x = factor(Recommandation), y = CA, fill = Recommandation)) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(colours=c("#EA0000","#FF8000","#00D223","#00D223"), values=c(0,0.25,0.5,1)) +
        expand_limits(y=c(0,max(data$CA) + 30)) +
        theme(legend.position = "none") +
        geom_text(aes(label= paste(as.character(format(count_total, big.mark = " ")), " clients", sep="")), vjust = -0.3, color="black", size=4) +
        labs(subtitle = "Recommanderiez-vous ce magasin à un proche ? \n\nNotes de 0 (Pas du tout) à 10 (Tout à fait)",
             x = "Notes des clients",
             y = "Chiffre d'affaire par client (en € par mois)") 
    }
  })
  
  
  

# Affichage par lot -------------------------------------------------------
  output$h2_title <- renderUI({
    lot <- input$lot_input
    lot_list <- c("Enquête 1", "Enquête 2", "Enquête 3")

    if (length(input$lot_input) != 0){
      title <- lot_list[as.numeric(lot[1])]
      if (length(lot) > 1){
        for (i in c(2:length(lot))){
          title <- paste(title, ", ", lot_list[as.numeric(lot[i])], sep = "")
        }
      }
    } else {
      title <- "Veuillez sélectionner au moins un groupe de clients"
    }
    h2(title)
  })


  # sliderInput pour sélectionner la période d'étude
  output$select_date <- renderUI({
    if (length(input$date) != 0) {
      selected <- input$date
    } else {
      selected <- as.Date(c("2019-11-01","2019-11-01"))
    }

    sliderInput(inputId = "date",
                label = "Choisissez une période de temps :",
                min = as.Date("2017-12-01"), max = as.Date("2019-11-01"),
                value = selected,
                timeFormat = "%B %Y", step = 30, width = "600px")
  })


  observe({
    
    ## date range
    date_range <- paste(substr(as.character(input$date),1,8),"01", sep="")
    date_range <- as.character(date_range)
    date_range <- as.Date.character(date_range, format = "%Y-%m-%d")
    date_1 <- date_range[1]
    date_2 <- date_range[2]
    
    if (length(input$lot_input) != 0 & length(input$date) != 0){
      
      ## valeurs des valuebox
      promoteur_valuebox <- sum(data_nps_ca_count[data_nps_ca_count$Lot %in% input$lot_input & data_nps_ca_count$DATEenvoi >= date_1 & data_nps_ca_count$DATEenvoi <= date_2,"Promoteur_count"])
      neutre_valuebox <- sum(data_nps_ca_count[data_nps_ca_count$Lot %in% input$lot_input & data_nps_ca_count$DATEenvoi >= date_1 & data_nps_ca_count$DATEenvoi <= date_2,"Neutre_count"])
      detracteur_valuebox <- sum(data_nps_ca_count[data_nps_ca_count$Lot %in% input$lot_input & data_nps_ca_count$DATEenvoi >= date_1 & data_nps_ca_count$DATEenvoi <= date_2,"Detracteur_count"])
      count_total <- promoteur_valuebox + neutre_valuebox + detracteur_valuebox

      promoteur_valuebox <- round(promoteur_valuebox * 100 / count_total,0)
      detracteur_valuebox <- round(detracteur_valuebox * 100 / count_total,0)
      neutre_valuebox <- 100 - promoteur_valuebox - detracteur_valuebox
      NPS_valuebox <- promoteur_valuebox - detracteur_valuebox

      CA_valuebox <- sum(data_nps_ca_count[data_nps_ca_count$Lot %in% input$lot_input & data_nps_ca_count$DATEenvoi >= date_1 & data_nps_ca_count$DATEenvoi <= date_2,"CA"])
      CA_valuebox <- round(CA_valuebox/1e3,1)

      ## modèle de regression linéaire
      data <- data_nps_ca_semaine_percentage[data_nps_ca_semaine_percentage$Lot %in% input$lot_input,!names(data_nps_ca_semaine_percentage) %in% c("Detracteur_count","Neutre_count","Promoteur_count","CA_detracteur","CA_neutre","CA_promoteur")]

      # on calcule le NPS global sur les différents lots et produits choisis
      for (i in input$lot_input){
        for (j in unique(typquest_count[typquest_count$Lot == i,]$Typquest)){
          data[data$Lot == i & data$Typquest == j,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] <- data[data$Lot == i & data$Typquest == j,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] * typquest_count[typquest_count$Lot == i & typquest_count$Typquest == j,]$n
        }
      }
      data[,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] <- data[,!names(data) %in% c("DATEenvoi","Lot","Typquest","CA")] / sum(typquest_count[typquest_count$Lot == i,]$n)

      # mise en forme des données pour appliquer le modèle de régression linéaire
      data <- data %>% group_by(DATEenvoi) %>% summarise(CA_client = sum(CA_client, na.rm = TRUE), NPS_0 = sum(NPS_0, na.rm = TRUE), NPS_1 = sum(NPS_1, na.rm = TRUE), NPS_2 = sum(NPS_2, na.rm = TRUE), NPS_3 = sum(NPS_3, na.rm = TRUE), NPS_4 = sum(NPS_4, na.rm = TRUE), NPS_5 = sum(NPS_5, na.rm = TRUE), NPS_6 = sum(NPS_6, na.rm = TRUE), NPS_7 = sum(NPS_7, na.rm = TRUE), NPS_8 = sum(NPS_8, na.rm = TRUE),
                                                         NPS_9 = sum(NPS_9, na.rm = TRUE), NPS_10 = sum(NPS_10, na.rm = TRUE), NPS_11 = sum(NPS_11, na.rm = TRUE), NPS_12 = sum(NPS_12, na.rm = TRUE), NPS_13 = sum(NPS_13, na.rm = TRUE), NPS_14 = sum(NPS_14, na.rm = TRUE), NPS_15 = sum(NPS_15, na.rm = TRUE), NPS_16 = sum(NPS_16, na.rm = TRUE), NPS_17 = sum(NPS_17, na.rm = TRUE),
                                                         NPS_18 = sum(NPS_18, na.rm = TRUE), NPS_19 = sum(NPS_19, na.rm = TRUE), NPS_20 = sum(NPS_20, na.rm = TRUE), NPS_21 = sum(NPS_21, na.rm = TRUE), NPS_22 = sum(NPS_22, na.rm = TRUE), NPS_23 = sum(NPS_23, na.rm = TRUE), NPS_24 = sum(NPS_24, na.rm = TRUE), NPS_25 = sum(NPS_25, na.rm = TRUE), NPS_26 = sum(NPS_26, na.rm = TRUE))
      data <- data[27:nrow(data),]
      data <- data[,!names(data) %in% c("DATEenvoi")]

      # modèle de régression linéaire, avec le r2 ajusté et les coefficients
      model_lm <- lm(CA_client ~ ., data = data)

      r2_adj <- summary(model_lm)$adj.r.squared

      coeff <- model_lm$coefficients
      names_coeff <- c("Ordonnée à l'origine", "NPS", "NPS 1 semaine avant")
      for (i in c(4:length(coeff))){
        names_coeff <- c(names_coeff, paste("NPS ", (i-2), " semaines avant", sep=""))
      }
      names(coeff) <- names_coeff
      df <- data.frame(coeff)
      df$coeff <- round(df$coeff,3)
      names(df) <- c("Coefficients multiplicatifs associés aux variables")

      delta_CA <- sum(coeff[2:length(coeff)])

      # construction des variables de la régression linéaire à partir des résultats du modèle
      x_model <- coeff[1] + data$NPS_0 * coeff[2] + data$NPS_1 * coeff[3] + data$NPS_2 * coeff[4] + data$NPS_3 * coeff[5] + data$NPS_4 * coeff[6] + data$NPS_5 * coeff[7] +
        data$NPS_6 * coeff[8] + data$NPS_7 * coeff[9] + data$NPS_8 * coeff[10] + data$NPS_9 * coeff[11] + data$NPS_10 * coeff[12] + data$NPS_11 * coeff[13] +
        data$NPS_12 * coeff[14] + data$NPS_13 * coeff[15] + data$NPS_14 * coeff[16] + data$NPS_15 * coeff[17] + data$NPS_16 * coeff[18] + data$NPS_17 * coeff[19] +
        data$NPS_18 * coeff[20] + data$NPS_19 * coeff[21] + data$NPS_20 * coeff[22] + data$NPS_21 * coeff[23] + data$NPS_22 * coeff[24] + data$NPS_23 * coeff[25] +
        data$NPS_24 * coeff[26] + data$NPS_25 * coeff[27] + data$NPS_26 * coeff[28]
      y_model <- data$CA_client

      
      ## labels
      typquest_labels <- c("Questionnaire 1", "Questionnaire 2", "Questionnaire 3", "Questionnaire 4", "Questionnaire 5")
      lot_labels <- c("Enquête 1", "Enquête 2", "Enquête 3")
      

      ## mise en forme des données pour les graphes de satisfaction et de recommandation
      data_sr <- data_nps_ca
      data_sr$Typquest <- factor(data_sr$Typquest, labels = typquest_labels)
      data_sr <- data_sr[data_sr$DATEenvoi >= date_1 & data_sr$DATEenvoi <= date_2,] 
      data_s <- data_sr[data_sr$Lot %in% input$lot_input,] %>% group_by(Satisfaction) %>% summarise(count_total = sum(Detracteur) + sum(Neutre) + sum(Promoteur), CA = sum(CA)/count_total)
      data_r <- data_sr[data_sr$Lot %in% input$lot_input,] %>% group_by(Recommandation) %>% summarise(count_total = sum(Detracteur) + sum(Neutre) + sum(Promoteur), CA = sum(CA)/count_total)
      


      ## outputs
      output$nb_client <- renderUI({
        valueBox(format(count_total, big.mark = " "), "Nombre de clients considérés", icon = icon("users"), width = 3, color = "aqua")
      })
      
      output$valuebox_CA <- renderUI({
        valueBox(paste(format(CA_valuebox, big.mark = " "),"k €"), "CA sur la période", icon = icon("file-invoice-dollar"), width = 3, color = "aqua")
      })

      output$valuebox_NPS <- renderUI({
        valueBox(NPS_valuebox, "NPS", icon = icon("thumbs-up"), width = 3, color = "aqua")
      })

      output$valuebox_newNPS <- renderUI({
        valueBox(paste(case_when(delta_CA >= 0 ~ "+ ", TRUE ~ "- "), round(abs(as.numeric(delta_CA)),2), " €/cl.", sep=""), "Hausse du CA si NPS + 1 pt", icon = icon("chart-line"), width = 3, color = "aqua")
      })

      output$valuebox_promoteur <- renderUI({
        valueBox(paste(promoteur_valuebox,"%"), "Clients promoteurs", icon = icon("smile"), width = 4, color = "green")
      })

      output$valuebox_neutre <- renderUI({
        valueBox(paste(neutre_valuebox,"%"), "Clients neutres", icon = icon("meh"), width = 4, color = "yellow" )
      })

      output$valuebox_detracteur <- renderUI({
        valueBox(paste(detracteur_valuebox,"%"), "Clients detracteurs", icon = icon("frown"), width = 4, color = "red")
      })

  
      # demi-camembert représentant les pourcentages de clients promoteurs, neutres et détracteurs
      output$plot_demi_camembert_satisfaction_percentage <- renderPlot({
        y <- data_nps_ca_count[data_nps_ca_count$DATEenvoi >= date_1 & data_nps_ca_count$DATEenvoi <= date_2 & data_nps_ca_count$Lot %in% input$lot_input,c("Detracteur_count","Neutre_count","Promoteur_count")] %>% summarise(Detracteur_count = sum(Detracteur_count), Neutre_count = sum(Neutre_count), Promoteur_count = sum(Promoteur_count))
        count_total <- y$Detracteur_count + y$Neutre_count + y$Promoteur_count
        y$Detracteur_count <- y$Detracteur_count * 100 / count_total
        y$Neutre_count <- y$Neutre_count * 100 / count_total
        y$Promoteur_count <- y$Promoteur_count * 100 / count_total

        y_pivot <- data.frame(groupe=c("Detracteur","Neutre","Promoteur"),
                              start = c(-pi/2, sum(y$Promoteur_count)*pi/100-pi/2, (sum(y$Promoteur_count) + sum(y$Neutre_count))*pi/100-pi/2),
                              end = c(y$Promoteur_count*pi/100-pi/2, (y$Promoteur_count + y$Neutre_count)*pi/100-pi/2, (y$Promoteur_count + y$Neutre_count + y$Detracteur_count)*pi/100-pi/2),
                              percentage = c(y$Detracteur_count,y$Neutre_count,y$Promoteur_count))

        ggplot(y_pivot) +
          geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, start = start, end = end, fill = groupe),size = 0)+
          expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) +
          scale_fill_manual(values = c("#00BA38","#FFBF00","#F13E3E")) +
          coord_fixed() +
          theme_no_axes() +
          theme(panel.border = element_blank(), legend.position = "none") +
          geom_text(aes(x = sin((start+end)/2)*1.25, y=cos((start+end)/2)*1.25), label = c(paste("Promoteurs \n", as.character(round(y_pivot$percentage[3],0)), " %", sep=""), paste("Neutres \n", as.character(round(y_pivot$percentage[2],0)), " %", sep=""), paste("Detracteurs \n", as.character(round(y_pivot$percentage[1],0)), " %", sep="")), size = 4) +
          geom_text(aes(x = 0, y = 0.1), label = paste("NPS = ", as.character(round(y_pivot$percentage[3],0) - round(y_pivot$percentage[1],0)), sep = ""), size = 7, fontface = "bold", colour = case_when((y_pivot$percentage[3] - y_pivot$percentage[1]) >= 0 ~ "#31B404", TRUE ~ "#DF0101")) +
          labs(fill = "",
               x = "",
               y = "")
      })

  
      # graphe représentant l'évolution des pourcentages de clients promoteurs, neutres et détracteurs au cours du temps    
      output$plot_line_DNP_T <- renderPlot({
        data <- data_nps_ca_dnp[data_nps_ca_dnp$Lot %in% input$lot_input,] %>% group_by(DATEenvoi, Client_group) %>% summarise(count = sum(count))
        for (date in unique(data$DATEenvoi)){
          date <- as_date(date)
          count_detracteur <- data[data$DATEenvoi == date & data$Client_group == "Detracteur",]$count
          count_neutre <- data[data$DATEenvoi == date & data$Client_group == "Neutre",]$count
          count_promoteur <- data[data$DATEenvoi == date & data$Client_group == "Promoteur",]$count
          count_total <- count_detracteur + count_neutre + count_promoteur
          data[data$DATEenvoi == date & data$Client_group == "Detracteur",]$count <- count_detracteur * 100 / count_total
          data[data$DATEenvoi == date & data$Client_group == "Neutre",]$count <- count_neutre * 100 / count_total
          data[data$DATEenvoi == date & data$Client_group == "Promoteur",]$count <- count_promoteur * 100 / count_total
        }

        ggplot(data, aes(x = DATEenvoi, y = count), color = Client_group) +
          geom_area(aes(group=Client_group, fill = factor(Client_group))) +
          scale_x_date(date_labels = "%m-%Y") +
          scale_fill_manual(values = c("#F13E3E","#FFBF00","#00BA38")) +
          expand_limits(y=80) +
          theme(legend.position = "bottom") +
          labs(fill = "",
               x = "Date",
               y = "Pourcentages")
      })
      
      
      # graphe représentant le modèle linéaire établi précédemment, et les points ayant permis de calculer la régression linéaire
      output$plot_NPS_CA_T_lag_sem <- renderPlot({
        ggplot(data, aes(x=x_model, y=y_model)) +
          geom_smooth(formula = y ~ x, method = "lm") +
          geom_point() +
          labs(subtitle =  paste("R2 ajusté = ", format(r2_adj, digits = 2), sep=""),
               x = "Combinaison du NPS des semaines précédentes",
               y = "Chiffre d'affaire par client en €")
      })
      
      # tableau des coefficients du modèle linéaire
      output$coeff_NPS_CA <- renderDataTable({df}, options = list(pageLength = 28, dom = 't'))
      
      # explication de la régression linéaire
      output$explication_NPS_CA_1 <- renderText({
        "Ce graphe repésente la corrélation entre le Chiffre d'Affaire et le Net Promoteur Score évalué sur une période de 6 mois.
        Pour cela, un modèle de régression linéaire a été construit à partir du CA à un instant donné et les valeurs du NPS pendant les 26 semaines précédentes (6 mois)."
      })
      output$explication_NPS_CA_2 <- renderText({
        paste("Le coefficient R2 ajusté du model est alors égal à ", format(summary(model_lm)$adj.r.squared, digits = 2), ".", sep="")
      })
      output$explication_NPS_CA_3 <- renderText({
        "Les coefficients utilisés pour la régression sont quant à eux visibles dans l'onglet \"Coefficients de la régression\"."
      })
      

      # graphe représentant le NPS et le CA superposé
      output$plot_NPS_CA_superpose_T <- renderPlot({
        data <- data_nps_ca_count[data_nps_ca_count$Lot %in% input$lot_input,] %>% group_by(DATEenvoi) %>% summarise(count_total = sum(Detracteur_count) + sum(Neutre_count) + sum(Promoteur_count), Detracteur_count = sum(Detracteur_count)*100/count_total, Promoteur_count = sum(Promoteur_count)*100/count_total, NPS = Promoteur_count - Detracteur_count, CA = sum(CA)/count_total)

        ggplot(data) +
          geom_line(aes(x=DATEenvoi, y=NPS, colour = "#00BFC4")) +
          geom_line(aes(x=DATEenvoi, y=CA/5, colour = "#F8766D")) +  # changement d'échelle du CA pour avoir des graphes avec des échelles cohérentes
          scale_y_continuous(name = "Net Promoter Score",
                             sec.axis = sec_axis(~.*5, name = "Chiffre d'affaire par client (en € par mois)")) +
          theme(axis.title.y.left = element_text(color = "#F8766D"), axis.text.y.left = element_text(color = "#F8766D")) +
          theme(axis.title.y.right = element_text(color = "#00BFC4"), axis.text.y.right = element_text(color = "#00BFC4")) +
          theme(legend.position='none') +
          labs(x = "Date")
      })
      
      
      # histogramme représentant le chiffre d'affaire moyen par mois des clients ayant mis chaque note de satisfaction
      # sur les barres, on trouve également le nombre de clients ayant mis chaque note de satisfaction
      output$hist_satisfaction <- renderPlot({
        ggplot(data_s, aes(x = factor(Satisfaction), y = CA, fill = factor(Satisfaction))) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values=c("#EA0000","#FFBF00","#00D223","#00D223")) +
          expand_limits(y=c(0,max(data_s$CA) + 30)) +
          theme(legend.position = "none") +
          geom_text(aes(label= paste(as.character(format(count_total, big.mark = " ")), " cl.", sep="")), vjust = -0.3, color="black", size=4) +
          labs(subtitle = "Êtes-vous satisfait(e) des produits et / ou services proposés par votre magasin ?  \n\nNotes sur une échelle de 1 (pas du tout satisfait) à 4 (très satisfait)",
               x = "Notes des clients",
               y = "Chiffre d'affaire par client (en € par mois)")  
      })
      
      
      # histogramme représentant le chiffre d'affaire moyen par mois des clients ayant mis chaque note de recommandation
      # sur les barres, on trouve également le nombre de clients ayant mis chaque note de recommandation
      output$hist_recommandation <- renderPlot({
        ggplot(data_r, aes(x = factor(Recommandation), y = CA, fill = Recommandation)) +
          geom_bar(stat = "identity") +
          scale_fill_gradientn(colours=c("#EA0000","#FF8000","#00D223","#00D223"), values=c(0,0.25,0.5,1)) +
          expand_limits(y=c(0,max(data_r$CA) + 30)) +
          theme(legend.position = "none") +
          geom_text(aes(label= paste(as.character(format(count_total, big.mark = " ")), " cl.", sep="")), vjust = -0.3, color="black", size=4) +
          labs(subtitle = "Recommanderiez-vous ce magasin à un proche ? \n\nNotes de 0 (Pas du tout) à 10 (Tout à fait)",
               x = "Notes des clients",
               y = "Chiffre d'affaire par client (en € par mois)")
      })
      
    } 
    ## tous les affichages sont nuls lorsqu'il n'y a pas de lot sélectionné pour l'étude : cette partie évite l'affichage de messages d'erreur
    else {
      output$nb_client <- renderUI({
        valueBox(0, "Nombre de clients considérés", icon = icon("users"), width = 3, color = "aqua")
      })
      
      output$valuebox_CA <- renderUI({
        valueBox(paste(0," €"), "CA sur la période", icon = icon("file-invoice-dollar"), width = 3, color = "aqua")
      })
      
      output$valuebox_NPS <- renderUI({
        valueBox(0, "NPS", icon = icon("thumbs-up"), width = 3, color = "aqua")
      })

      output$valuebox_newNPS <- renderUI({
        valueBox("+ 0 €/cl.", "Hausse du CA si NPS + 1 pt", icon = icon("chart-line"), width = 3, color = "aqua")
      })

      output$valuebox_promoteur <- renderUI({
        valueBox(paste(0,"%"), "Clients promoteurs", icon = icon("smile"), width = 4, color = "green")
      })

      output$valuebox_neutre <- renderUI({
        valueBox(paste(0,"%"), "Clients neutres", icon = icon("meh"), width = 4, color = "yellow" )
      })

      output$valuebox_detracteur <- renderUI({
        valueBox(paste(0,"%"), "Clients detracteurs", icon = icon("frown"), width = 4, color = "red")
      })

      output$plot_demi_camembert_satisfaction_percentage <- renderPlot(return())

      output$plot_line_DNP_T <- renderPlot(return())
      
      output$plot_NPS_CA_T_lag_sem <- renderPlot(return())
      
      output$coeff_NPS_CA <- renderTable(return())
      
      output$explication_NPS_CA_1 <- renderText({
        "Ce graphe repésente la corrélation entre le Chiffre d'Affaire et le Net Promoteur Score évalué sur une période de 9 mois.
        Pour cela, un modèle de régression linéaire a été construit à partir du CA à un instant donné et les valeurs du NPS pendant les 36 semaines précédentes (9 mois)."
      })
      output$explication_NPS_CA_2 <- renderText({
        paste("Le coefficient R2 ajusté du model est alors égal à ", format(summary(model_lm)$adj.r.squared, digits = 2), ".", sep="")
      })
      output$explication_NPS_CA_3 <- renderText({
        "Les coefficients utilisés pour la régression sont quant à eux visibles dans l'onglet \"Coefficients de la régression\"."
      })

      output$plot_NPS_CA_superpose_T <- renderPlot(return())

      output$plot_satisfaction <- renderPlot(return())

      output$plot_recommandation <- renderPlot(return())
      
      output$hist_satisfaction <- renderPlot(return())
      
      output$hist_recommandation <- renderPlot(return())

    }
  })

}
