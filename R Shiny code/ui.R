dashboardPage(
  dashboardHeader(title = "Net Promoter Score"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Aperçu général", tabName = "general", icon = icon("dashboard")),
      menuItem("Détails par enquête", tabName="lot", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tabItems(
      ## page d'accueil / 1er onglet
      tabItem(
        tabName = "general",
        fluidPage(
          h2("Aperçu général"),
          
          valueBox(paste(round(sum(data_nps_ca[year(data_nps_ca$DATEenvoi) == "2019","CA"])/1e+06, 2),"m €"), "Chiffre d'affaires de 2019", icon = icon("file-invoice-dollar"), width = 3, color = "aqua"),
          valueBox(round(nps_lot_1,0), "NPS du lot 1 sur le dernier mois", icon = icon("user-plus"), width = 3, color = "teal"),
          valueBox(round(nps_lot_2,0), "NPS du lot 2 sur le dernier mois", icon = icon("credit-card"), width = 3, color = "olive" ),
          valueBox(round(nps_lot_3,0), "NPS du lot 3 sur le dernier mois", icon = icon("wallet"), width = 3, color = "green"),
          
          box(title = "Evolution du NPS et du CA par mois pour chaque groupe de clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              plotOutput("plot_NPS_CA_superpose_T_facet")
          ),
          

          box(title = "Evolution du NPS pour chaque groupe de clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              fluidRow(
                column(width = 3, 
                       selectInput(input = "lot_input_1", label="Choisissez un lot d'étude :", 
                                      choices = list("Enquête 1" = 1, "Enquête  2" = 2, "Enquête 3" = 3), selected = c(1))
                ),
                column(width = 8,
                       sliderInput(inputId = "date_range_1", 
                                   label = "Choisissez une période de temps :", 
                                   min = as.Date("2017-12-01"), max = as.Date("2019-11-01"), 
                                   value = c(as.Date("2019-11-01"), as.Date("2019-11-01")),
                                   timeFormat = "%B %Y", step = 30))
              ),
              plotOutput("plot_hist_NPS", height = "350px")
          ),
          
          box(title = "Force de la corrélation (R2) entre NPS et le CA pour chaque groupe de clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              plotOutput("plot_hist_R2", height = "350px")
          ),
          
          box(title = "Répartition des clients promoteurs, neutres et détracteurs pour chaque groupe de clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              fluidRow(
                column(width = 3, 
                       selectInput(input = "lot_input_2", label="Choisissez un lot d'étude :", 
                                      choices = list("Enquête 1" = 1, "Enquête 2" = 2, "Enquête 3" = 3), selected = c(1))
                ),
                column(width = 8,
                       sliderInput(inputId = "date_range_2", 
                                   label = "Choisissez une période de temps :", 
                                   min = as.Date("2017-12-01"), max = as.Date("2019-11-01"), 
                                   value = c(as.Date("2019-11-01"), as.Date("2019-11-01")),
                                   timeFormat = "%B %Y", step = 30))
              ),
              radioButtons(input = "indicateur_dnp", label = "Choissisez un indicateur à afficher",
                           choices = list("CA moyen par client" = "CA", "Panier moyen" = "panier", "Fréquence d'achat" = "frequence"), selected = "CA", inline = TRUE),
              plotOutput("plot_camembert_hist_DNP")
          ),
          
          box(title = "Répartition de la satisfaction des clients par mois", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              fluidRow(
                column(width = 3, 
                       selectInput(input = "lot_input_3", label="Choisissez un lot d'étude :", 
                                      choices = list("Enquête 1" = 1, "Enquête 2" = 2, "Enquête 3" = 3), selected = c(1))
                ),
                column(width = 8,
                       sliderInput(inputId = "date_range_3", 
                                   label = "Choisissez une période de temps :", 
                                   min = as.Date("2017-12-01"), max = as.Date("2019-11-01"), 
                                   value = c(as.Date("2019-11-01"), as.Date("2019-11-01")),
                                   timeFormat = "%B %Y", step = 30))
              ),
              plotOutput("plot_hist_satisfaction")
          ),
          
          box(title = "Répartition de la recommandation des clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=12,
              fluidRow(
                column(width = 3, 
                       selectInput(input = "lot_input_4", label="Choisissez un lot d'étude :", 
                                      choices = list("Enquête 1" = 1, "Enquête 2" = 2, "Enquête 3" = 3), selected = c(1))
                ),
                column(width = 8,
                       sliderInput(inputId = "date_range_4", 
                                   label = "Choisissez une période de temps :", 
                                   min = as.Date("2017-12-01"), max = as.Date("2019-11-01"), 
                                   value = c(as.Date("2019-11-01"), as.Date("2019-11-01")),
                                   timeFormat = "%B %Y", step = 30))
              ),
              plotOutput("plot_hist_recommandation")
          )
        )
      ),
      
      ## 2ème onglet
      tabItem(
        tabName = "lot",
        fluidPage(
          uiOutput("h2_title"),
          fluidRow(
            column(selectInput(input = "lot_input", label="Choisissez une enquête clients :", 
                                  choices = list("Enquête 1" = 1, "Enquête 2" = 2, "Enquête 3" = 3), selected = c(1)),
                   width = 4),
            #column(h1(" "), actionButton("action_button",label = "Valider"), width = 1),
            column(uiOutput("select_date"),width = 7)
          ),
          
          uiOutput("nb_client"),
          uiOutput("valuebox_CA"),
          uiOutput("valuebox_NPS"),
          uiOutput("valuebox_newNPS"),
          uiOutput("valuebox_promoteur"),
          uiOutput("valuebox_neutre"),
          uiOutput("valuebox_detracteur"),
          
          
          box(title = "Répartition des clients promoteurs, neutres et détracteurs", status = "primary", solidHeader = F,
              collapsible = TRUE,width=4,
              plotOutput("plot_demi_camembert_satisfaction_percentage", height="175px")
          ),
          box(title = "Evolution des proportions de clients promoteurs, neutres et détracteurs", status = "primary", solidHeader = F,
              collapsible = TRUE,width=8,
              plotOutput("plot_line_DNP_T",height = "195px")
          ),
          
          tabBox(title = "Corrélation entre le NPS et le CA (CA = a + b.NPS(t) + c.NPS(t-1) + ...)",width=8,
                 tabPanel("Graphe", plotOutput("plot_NPS_CA_T_lag_sem",height = "210px")),
                 tabPanel("Coefficients de la régression", DT::dataTableOutput("coeff_NPS_CA"), style = "height:210px; overflow-y: scroll;"),
                 tabPanel("Explication", textOutput("explication_NPS_CA_1"), textOutput("explication_NPS_CA_2"), textOutput("explication_NPS_CA_3"))
          ),
          box(title = "Evolution du NPS et du CA par mois", status = "primary", solidHeader = F,
              collapsible = TRUE,width=4,
              plotOutput("plot_NPS_CA_superpose_T",height = "228px")
          ),
          
          box(title = "Répartition de la satisfaction des clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=6,
              plotOutput("hist_satisfaction",height = "210px")
          ),
          box(title = "Répartition de la recommandation des clients", status = "primary", solidHeader = F,
              collapsible = TRUE,width=6,
              plotOutput("hist_recommandation",height = "210px")
          )
        )
      )
    )
  )
)
