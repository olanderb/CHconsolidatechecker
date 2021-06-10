#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(DT)

ui <- fluidPage(
  
    navbarPage("CH Data Consolidater",
             tabPanel("Plot"),

  fileInput("file1", "select your excel sheet containing the tableur resultats",
            multiple = TRUE,
            accept = c(".xslx")),
  
    #show current_outcomes
  #show projected_outcomes
  #show tableur_resultat_current
  tabPanel("Results"),
    h1("Current Results"),
    #show output of the current join
    DTOutput('joined_current'),
    h1("Projected Results"),
    #show output of the projected join
    DTOutput('joined_projected')
  
  #tabPanel("View Joins"),
  #show  curr_doesnt_join_1 - with text  "areas that are in Tableau 4 - Estimation Pop but not in Tableau 3 - Synthèse & ClassifC "
  #show  curr_doesnt_join_2 - with text  "areas that are in in Tableau 3 - Synthèse & ClassifC but not in Tableau 4 - Estimation Pop "
  
  #show  proj_doesnt_join_1 - with text  "areas that are in Tableau 4 - Estimation Pop but not in Tableau 3 - Synthèse & ClassifP "
  #show  proj_doesnt_join_2 - with text  "areas that are in in Tableau 3 - Synthèse & ClassifP but not in Tableau 4 - Estimation Pop "
  
)
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    
    #data <- eventReactive(input$file, {
    #  read_sf(input$file$datapath)
    #})
    output$excel_file <-  renderTable({
      input$file1
    })
    
    
    #current_outcomes <- eventReactive(input$file, {
    #  read_excel(input$file$datapath, 
     #            sheet = "Tableau 3 - Synthèse & ClassifC",
     #            skip = 1)
    #})
    
    #Import Outcomes - Current - Tableau 3 - Synthèse & ClassifC, rename variables and then calculate the final_phase_calc
    current_outcomes <- read_excel("www/tableur resultat.xlsm", 
                                sheet = "Tableau 3 - Synthèse & ClassifC",
                              skip = 1)
    
    

    
    #rename and only select 
    current_outcomes <- current_outcomes %>% rename(`FCS` = `INDICATEURS DE RESULTATS`, `LHCS` = '...5', `Nut` = '...6', `Mort` = '...7') %>%
        select(`Admin 1`:`Admin 2`,`FCS`:`Mort`)
    #remove all blank rows (because of merged cells)
    current_outcomes <- current_outcomes %>% filter(!is.na(`Admin 1`))
    #converts to numeric and converts NA to 0 
    current_outcomes <- current_outcomes %>% mutate_at(vars(c("FCS","LHCS","Nut","Mort")), ~replace(., is.na(.), 0))
    current_outcomes <- current_outcomes %>% mutate_at(vars(c("FCS","LHCS","Nut","Mort")), ~as.numeric(.))
    #make sures this accounts for missing values
    #make sures this accounts for missing values
    current_outcomes <- current_outcomes %>% mutate(final_phase_current_calc = case_when(
      LHCS == 0 &  Nut == 0 & Mort == 0 ~ FCS,
      FCS != 0 & LHCS != 0 & Nut == 0 & Mort == 0 ~ (3*FCS + 2 * LHCS)/5,
      FCS != 0 & LHCS != 0 & Nut != 0 & Mort == 0 ~ (3*FCS + 2 * LHCS + Nut)/6,
      FCS != 0 & LHCS != 0 & Nut != 0 & Mort != 0 ~ (3*FCS + 2 * LHCS + Nut +Mort)/7)
    )
    current_outcomes <- current_outcomes %>% mutate(final_phase_current_calc = round(final_phase_current_calc))
    
    #Import Outcomes - Projected - Tableau 3 - Synthèse & ClassifP, rename variables and then calculate the final_phase_calc
    projected_outcomes <- read_excel("www/tableur resultat.xlsm", 
                                     sheet = "Tableau 3 - Synthèse & ClassifP",
                                     skip = 1)
    #rename and only select 
    projected_outcomes <- projected_outcomes %>% rename(`FCS` = `INDICATEURS DE RESULTATS`, `LHCS` = '...5', `Nut` = '...6', `Mort` = '...7') %>%
        select(`Admin 1`:`Admin 2`,`FCS`:`Mort`)
    #remove all blank rows (because of merged cells)
    projected_outcomes <- projected_outcomes %>% filter(!is.na(`Admin 1`))
    #converts to numeric and converts NA to 0 
    projected_outcomes <- projected_outcomes %>% mutate_at(vars(c("FCS","LHCS","Nut","Mort")), ~replace(., is.na(.), 0))
    projected_outcomes <- projected_outcomes %>% mutate_at(vars(c("FCS","LHCS","Nut","Mort")), ~as.numeric(.))
    #make sures this accounts for missing values
    #make sures this accounts for missing values
    projected_outcomes <- projected_outcomes %>% mutate(final_phase_projected_calc = case_when(
      LHCS == 0 &  Nut == 0 & Mort == 0 ~ FCS,
      FCS != 0 & LHCS != 0 & Nut == 0 & Mort == 0 ~ (3*FCS + 2 * LHCS)/5,
      FCS != 0 & LHCS != 0 & Nut != 0 & Mort == 0 ~ (3*FCS + 2 * LHCS + Nut)/6,
      FCS != 0 & LHCS != 0 & Nut != 0 & Mort != 0 ~ (3*FCS + 2 * LHCS + Nut +Mort)/7)
    ) 
    projected_outcomes <- projected_outcomes %>% mutate(final_phase_projected_calc = round(final_phase_projected_calc))
    
    #import Final results - splitting into current and projected and selecting and renaming only key variables
    #current
    tableur_resultat <- read_excel("www/tableur resultat.xlsm", 
                                   sheet = "Tableau 4 - Estimation Pop")
    #current - rename and only select 
    tableur_resultat_current <- tableur_resultat %>% select(`Admin 0` = "1er niveau administratif", `Admin 1` = "2ème niveau administratif", `Admin 2` = "3ème niveau administratif", "Geocode", "Date du cycle","final_phase_current" = "SITUATION COURANTE")
    #remove all blank rows (because of merged cells)
    tableur_resultat_current <- tableur_resultat_current %>% filter(!is.na(`Admin 1`))
    #make numeric
    tableur_resultat_current <- tableur_resultat_current %>% mutate(final_phase_current = as.numeric(final_phase_current)) %>% mutate(final_phase_current = round(final_phase_current))
    #projected - - splitting into current and projected and renaming so they can join 
    tableur_resultat_projected <- tableur_resultat %>% select(`Admin 0` = "1er niveau administratif", `Admin 1` = "2ème niveau administratif", `Admin 2` = "3ème niveau administratif", "Geocode", "Date du cycle","final_phase_projected" = "SITUATION PROJETEE")
    #remove all blank rows (because of merged cells)
    tableur_resultat_projected  <- tableur_resultat_projected  %>% filter(!is.na(`Admin 1`))
    #make numeric
    #tableur_resultat_projected  <- tableur_resultat_projected  %>% mutate(final_phase_projected = as.numeric(final_phase_projected)) %>% mutate(final_phase_projected = round(final_phase_projected))
        
    #now join together the current calculated final phase and the actual final phase
    #the joins and antijoins should be launched by an action button
    #display whatever doesnt join from curr_doesnt_join_1 & curr_doesnt_join_1
    curr_doesnt_join_1 <- anti_join(tableur_resultat_current, current_outcomes, by = c("Admin 1","Admin 2"))
    curr_doesnt_join_2 <- anti_join(current_outcomes, tableur_resultat_current, by = c("Admin 1","Admin 2"))
        #real join
    joined_current <- left_join(tableur_resultat_current, current_outcomes, by = c("Admin 1","Admin 2"))
    #calculate difference
    joined_current <- joined_current %>% mutate(equal = (final_phase_current == final_phase_current_calc))
    #create a file export button to download the file
    #export
    #joined_current %>% write.csv("joined_current.csv")
    
    # turn joined projected into datatable
    output$joined_current <- renderDT({
      datatable(joined_current, extensions = 'Buttons', 
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               paging = TRUE, searching = TRUE,
                               fixedColumns = TRUE, autoWidth = TRUE,
                               ordering = TRUE, dom = 'tB',
                               buttons = c('copy', 'csv', 'excel')))
    })

    
    
    
    #now join - need to figure out how to only do this at adm1 if not analyzed at adm2 level
    proj_doesnt_join_1 <- anti_join(tableur_resultat_projected, projected_outcomes, by = c("Admin 1","Admin 2"))
    proj_doesnt_join_2 <- anti_join(projected_outcomes, tableur_resultat_projected, by = c("Admin 1","Admin 2"))
    #real join
    projected_current <- left_join(tableur_resultat_projected, projected_outcomes, by = c("Admin 1","Admin 2"))
    #calculate difference
    joined_projected <- projected_current %>% mutate(equal = (final_phase_projected == final_phase_projected_calc))
    #export
    #joined_projected %>% write.csv("joined_projected.csv")
    
    # turn joined projected into datatable
    output$joined_projected <- renderDT({
      datatable(joined_projected, extensions = 'Buttons', 
                options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                               paging = TRUE, searching = TRUE,
                               fixedColumns = TRUE, autoWidth = TRUE,
                               ordering = TRUE, dom = 'tB',
                               buttons = c('copy', 'csv', 'excel')))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
