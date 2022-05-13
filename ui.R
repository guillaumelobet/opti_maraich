



ui <- dashboardPage(
  
  skin='green',
  
  dashboardHeader(title = "Planificateur maraichage - V1.02"),
  
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; margin-left: 1em; margin-bottom: 1em;}"),
      tags$style("h4 { margin-left: 1em; margin-bottom: 1em;}"),
      
      menuItem("Planificateur", tabName = "results", icon = icon("leaf")),
      
      tags$hr()
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      # 
      tabItem(tabName = "results",
              fluidRow(
                column(width = 4,
                   tabBox(
                     # Title can include an icon
                     title = tagList(shiny::icon("gear"), ""), width = NULL,
                     tabPanel( "Optimisation",
                         selectInput("target", 
                                     label = "Selectionner la cible d'optimisation", 
                                     choices = c("Maximiser le revenu" = "price",
                                                 "Maximiser la production" = "prod",
                                                 "Egaliser la production" = "prod_equal",
                                                 "Maximiser les calories" = "calorie",
                                                 "Minimiser l'utilisation de l'eau" = "water",
                                                 "Minimiser l'impact carbone" = "carbone")
          
                                     ), 
                         
                         selectInput("vegs_to_use",
                                     label = "Selectionner les légumes",
                                     choices = c("Wait"), selected = NULL, multiple = T),
                         
                         tags$hr(), 
                         numericInput("surf_tot", "Surface total de l'exploitation (m2)", surf_tot), 
                         sliderInput("surf_min", 
                                     "Surface minimale par légume (m2)", 
                                     min = 0, max=floor(surf_tot/veg_min), step = 5, value = surf_min),
                         sliderInput("surf_ratio", 
                                     "Ratio entre la plus petite et la plus grande planche", 
                                     min = 1, max=6.0, step = 0.1, value = 2.0),
                         numericInput("cost_max", "Coût maximal des cultures (€)", cost_max)
                         
                     ), 
                     tabPanel("Légumes", 
                        selectInput("vegs_to_mod",
                                    label = "Selectionner le légume",
                                    choices = c("Wait"), selected = NULL, multiple = F), 
                        selectInput("param_to_mod",
                                 label = "Selectionner le paramètre",
                                 choices = c("Wait"), selected = NULL, multiple = F)  , 
                        numericInput("param_value", "Valeur", NULL), 
                        actionButton(inputId = "updateVeg", label="Mettre à jours", 
                                     icon("cogs"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        
                      )
                   )
                ),
                column(width = 8,
                       
                       tabBox(
                         title = tagList(shiny::icon("leaf"), ""), width = NULL,
                         
                         tabPanel("Résultats d'optimisation",
                                  fluidRow(
                                    column(width = 6,
                                           plotOutput("surface_plot")
                                           ),
                                    column(width = 6,
                                           DT::dataTableOutput('table_opt')
                                           )
                                    ),
                                  tags$hr(),
                                  DT::dataTableOutput('table_opt_surf'),
                                  value=1
                         ),
                         
                         tabPanel("Fiches récapitulatives",
                                  fluidPage(
                                    selectInput("vegs_to_recap",
                                                label = "Selectionner le légume",
                                                choices = c("Wait"), selected = NULL, multiple = F),
                                    actionButton(inputId = "updateVegrecap", label="Mettre à jours", 
                                                 icon("cogs"), 
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    
                                    titlePanel("Fiches techniques"),
                                    #includeMarkdown(paste0("www/HTML/",vegetaux,".md"))
                                    htmlOutput("recap")
                                    
                                  )
                           
                         ),
                         
                         tabPanel("Simulation de base (surface =)",
                                  fluidRow(
                                    column(width = 6,
                                           plotOutput("surface_plot_base")
                                    ),
                                    column(width = 6,
                                           DT::dataTableOutput('table_base')
                                    )
                                  ),
                                  value=2
                         ), 
                         tabPanel("Paramètres des légumes",
                                  DT::dataTableOutput('param_legumes'),
                                  value=2
                         ),
                         tabPanel("Calendrier des légumes",
                                  basicPage(
                                    h2("Calendrier cultural"),
                                    DT::dataTableOutput("calendrier")
                                    
                         ),
                         
                         
                         )
                       )
                )
              )
      )
      
    )
  )
)
