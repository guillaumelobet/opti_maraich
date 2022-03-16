# Copyright © 2018, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2018 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.

library(shiny)

shinyServer(function(input, output, clientData, session) {
  
  rsc <- reactiveValues(calendrier = NULL)
  
  rs <- reactiveValues(veg = NULL, 
                       output = NULL, 
                       base = NULL, 
                       optim = NULL)
  
  
  ## Load the data -----
  observe({
    rs$veg <- veg
  })
  
  
  observe({
    rsc$calendrier <- calendrier
  })
  ## Update the UI -----
  observe({
    if(is.null(rs$veg)){return()}
    
    vars <- unique(rs$veg$vegetable)
    sel <- input$vegs_to_use
    if(length(sel) == 0) sel = vars
    updateSelectInput(session, "vegs_to_use", choices = vars, selected=sel) 
  })  
  
  
  ## Update the UI -----
  observe({
    if(is.null(rs$veg)){return()}
    vars <- unique(rs$veg$vegetable)
    sel <- input$vegs_to_mod
    if(length(sel) == 0) sel = vars
    updateSelectInput(session, "vegs_to_mod", choices = vars, selected=sel) 
  })  
  
  
  observe({
    if(is.null(rs$veg)){return()}
    vars <- colnames(rs$veg)[-1]
    ct_options <- list()
    sel <- input$param_to_mod
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    updateSelectInput(session, "param_to_mod", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$veg)){return()}
    vars <- unique(rs$veg$vegetable)
    sel <- input$vegs_to_recap
    if(length(sel) == 0) sel = vars
    updateSelectInput(session, "vegs_to_recap", choices = vars, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$veg)){return()}
    if(is.null(input$param_to_mod)){return()}
    if(is.null(input$vegs_to_mod)){return()}
    if(input$param_to_mod == "Wait"){return()}
    if(input$vegs_to_mod == "Wait"){return()}
    
    val <- rs$veg %>% filter(vegetable == input$vegs_to_mod)
    updateNumericInput(session, "param_value", value = val[[input$param_to_mod]])
    
  }) 
  
  
  # Compute the base simulation, with the same surface for each vegetable
  observe({
    
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_use)
    
    # Total surface, in m2
    surf_tot <- input$surf_tot 
    
    # number of different vegetables
    veg_min <- length(unique(veg$vegetable))

    surface = rep(surf_tot / veg_min, veg_min)
    
    rs$base <- data.frame("Variable" = c("Surface", "Production", "Revenu", "Calorie", "Eau"), 
                          "Valeur" = c(sum(surface),
                                       round(sum(surface * veg$yield)),
                                       round(sum(surface * veg$yield * veg$price)),
                                       round(sum(surface * veg$yield * veg$calorie)),
                                       round(sum(surface * veg$yield * veg$water))
                          )
    )
    
  })
  
  
  # Render de plot for the base simulation
  output$surface_plot_base <- renderPlot({
    
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_use)
    
    # Total surface, in m2
    surf_tot <- input$surf_tot 
    
    # number of different vegetables
    veg_min <- length(unique(veg$vegetable))
    
    surface = rep(surf_tot / veg_min, veg_min)
    
    dat <- data.frame(name = veg$vegetable, 
                      surface = round(surface))
    
    ggplot(dat, aes(x="", y=surface, fill=name)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(surface, " m2")), 
                position = position_stack(vjust=0.5)) +
      labs(x = NULL, y = NULL) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) 
    
  })
  
  # Render a table for the base simulation
  output$table_base <- DT::renderDataTable({
    if(is.null(rs$base)){return()}
    temp <- rs$base
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })   
  
  
  # optimize the surface used for each vegetalbe base ion different targets
  
  output$surface_plot <- renderPlot({
    if(is.null(rs$base)){
      return(NULL)
    }
    
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_use)
    
    # Total surface, in m2
    surf_tot <- input$surf_tot 
    
    # number of different vegetables
    veg_min <- length(unique(veg$vegetable))
    
    # min surface for one parcel, in m2
    surf_min <- input$surf_min
    surf_max <- (surf_tot / veg_min ) * input$surf_ratio
    if(surf_min > surf_max) surf_min <- surf_max
    
    # Max cost allowed by the user
    cost_max <- input$cost_max
    if(cost_max < sum(surf_min * veg$price)){
      cost_max <- sum(surf_min * veg$price)
    }
    
    ### OPTIMISATION
    ## Exemple and tutorial here https://www.r-bloggers.com/2012/07/linear-programming-in-r-an-lpsolveapi-example/
    
    #define the datasets
    lprec <- make.lp(0, veg_min)
    lp.control(lprec, sense="max")
    
    # Objective function
    if (input$target == "price") set.objfn(lprec, veg$yield * veg$price) # optimize on price
    else if (input$target == "prod") set.objfn(lprec, veg$yield) # optimize on production
    else if (input$target == "prod_equal") set.objfn(lprec, veg$yield*veg$price) # optimize on production
    else if (input$target == "calorie") set.objfn(lprec, veg$calorie) # optimize on calories
    else if (input$target == "water"){
      lp.control(lprec, sense="min")
      set.objfn(lprec, veg$yield *veg$water) # optimize on water (minimal)      
    } 

    # Total surface contrains
    add.constraint(lprec, rep(1, veg_min), "=", surf_tot) 
    
    # price constrains
    add.constraint(lprec, veg$cost, "<=", cost_max)
    
    # Individual production constrain. Same production for each veg
    if (input$target == "prod_equal"){
      for(i in c(2:veg_min)){
        inds <- rep(0, veg_min)
        inds[i] <- -1
        inds[1] <- 1
        inds <- inds * veg$yield
        add.constraint(lprec, inds, "=", 0)
      }
    }else{
      # Individual surface contrains
      for(i in c(1:veg_min)){
        inds <- rep(0, veg_min)
        inds[i] <- 1
        add.constraint(lprec, inds, ">=", surf_min)
        add.constraint(lprec, inds, "<=", surf_max)
      }
    }
    

    # solve the optimisation and get the result back
    solve(lprec)
    
    rs$optim = lprec
    
    # Save the processed result
    output <- data.frame("Variable" = c("Surface", "Production", "Revenu", "Calorie", "Eau"), 
                            "Valeur" = c(sum(round(get.variables(lprec))),
                                        round(sum(get.variables(lprec) * veg$yield)),
                                        round(sum(get.variables(lprec) * veg$yield * veg$price)),
                                        round(sum(get.variables(lprec) * veg$yield * veg$calorie)),
                                        round(sum(get.variables(lprec) * veg$yield * veg$water))
                            )
    )
    
    output$Difference = round(((output$Valeur / rs$base$Valeur) - 1 ) *100, 1)
    
    rs$output <- output

    dat <- data.frame(name = veg$vegetable, 
                      surface = round(get.variables(lprec)))
    
    ggplot(dat, aes(x="", y=surface, fill=name)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(surface, " m2")), 
                position = position_stack(vjust=0.5)) +
      labs(x = NULL, y = NULL) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) 
    
  })
  
  output$table_opt <- DT::renderDataTable({
    if(is.null(rs$output)){return()}
    temp <- rs$output
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  }) 
  
  output$param_legumes <- DT::renderDataTable({
    if(is.null(rs$veg)){return()}
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_use)
      temp <- veg
      DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  }) 
  
  #calendrier
  output$calendrier <- DT::renderDataTable({
    if(is.null(rsc$calendrier)){return()}
    calendrier <- rsc$calendrier %>% 
      filter(vegetable %in% input$vegs_to_use)
    
    df <- DT::datatable(calendrier, options = list(scrollX = FALSE, pageLength = 10))%>% 
      formatStyle(c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre",
                    "novembre","decembre"), backgroundColor = styleEqual(c("S", "P","R"), c('lime', 'yellow',"green"))) %>%
      formatStyle(c("janvier","fevrier","mars","avril","mai","juin","juillet","aout","septembre","octobre",
                    "novembre","decembre"), textAlign = "center")
  })
  
 #Fiches recapitulatives (+Update) 
  #iconv sert a convertir en UTF-8 les fichiers HTML
  #pathwww car de base le script ne lit que les fichiers html hors www
  observeEvent(input$updateVegrecap,{
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_recap)
    vegs <- unique(veg$vegetable)
    for (vegetaux in vegs){
      getPage<-function() {
        pathwww <- here()
        setwd(paste0(pathwww,"/www/HTML"))
        html <- includeHTML(paste0(vegetaux,".html"))
        HTMLUTF8 <- iconv(html)
      return(HTMLUTF8)}
    
      
    }
    if (input$vegs_to_recap == vegs){
      getPage()
    }
    output$recap <- renderUI({
      getPage()
    })
  })
  
  
  
  output$table_opt_surf <- DT::renderDataTable({
    if(is.null(rs$optim)){return(NULL)}
    
    lprec <- rs$optim
    
    veg <- rs$veg %>% 
      filter(vegetable %in% input$vegs_to_use)
    
    vegs <- unique(veg$vegetable)
    
    temp <- data.frame("Légume" = vegs, 
                       "Surface" = round(get.variables(lprec)), 
                       "Production" = round(get.variables(lprec) * veg$yield), 
                       "Prix" = round(get.variables(lprec) * veg$yield * veg$price))
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })  
  
  
  
  
  
  
    
  }
)
