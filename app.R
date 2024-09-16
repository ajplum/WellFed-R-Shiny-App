##clear your working directory
rm(list=ls())

library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)
library(openxlsx)
library(jsonlite)
library(flexdashboard)

recipes <- read.xlsx("/Users/AustinTheBoss/Documents/Documents - User’s MacBook Pro (2)/COSC 5500/myApp/v5/data/appRecipes2.xlsx")
macros <- read.xlsx("/Users/AustinTheBoss/Documents/Documents - User’s MacBook Pro (2)/COSC 5500/myApp/v5/data/macros.xlsx")


# Function to extract steps from a JSON string
extract_steps <- function(json_str) {
  # Parse the JSON string
  json_list <- fromJSON(json_str, simplifyVector = FALSE)
  
  # Extract steps from the list
  steps <- unlist(json_list)
  
  # Remove empty strings
  steps <- steps[steps != ""]
  
  # Return the list of steps
  return(steps)
}


# Apply the function to each row of the dataframe
recipes$steps <- sapply(recipes$directions, extract_steps)
recipes$quantities <- sapply(recipes$ingredients, extract_steps)

ingredients <- macros %>%
  mutate(ingredient_singular = gsub("s$", "", food_name)) %>%
  distinct(ingredient_singular, .keep_all = TRUE)

ingredients$food_name <- tolower(ingredients$food_name)
ingredients$ingredient_singular <- tolower(ingredients$ingredient_singular)


# Define UI
ui <- fluidPage(

    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: white;
        color: red;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
    ),
  
  titlePanel("Welcome to WellFed! Your recipe for balanced living"),
  
  sidebarPanel(
    checkboxGroupInput("selected_ingredients", "Select Ingredients:",
                       choices = unique(ingredients$ingredient_singular),
                       selected = "almond")
  ),
  mainPanel(
    tabsetPanel(id = "main_tabs",
                tabPanel("Personal Information", 
                         numericInput("height", label = h3("Height input (lbs)"), value = 60),
                         numericInput("weight", label = h3("Weight input (inches)"), value = 160),
                         numericInput("age", label = h3("Age input"), value = 24),
                         selectInput("gender", label = h3("Gender input"), 
                                     choices = c("Male","Female","Prefer not say"),
                                     selected = "Male"),
                         actionButton("calc", "Calculate", class = "btn-primary"),
                         fluidRow(column(3, textOutput("camel"))),
                         fluidRow(column(3, textOutput("popcorn"))),
                         fluidRow(column(3, textOutput("caravan"))),
                         fluidRow(column(3, textOutput("falafel")))),
                tabPanel("Recipes", DT::DTOutput("foods")),
                tabPanel("Recipe Information", htmlOutput("rhino"),
                         hr(),
                         htmlOutput("namaste"),
                         hr(),
                         htmlOutput("koala"),
                         hr(),
                         htmlOutput("macro"),
                         hr(),
                         htmlOutput("llama"),
                         hr(),
                         actionButton("add", "Add", class = "btn-primary"),
                         actionButton("remove", "Remove", class = "btn-primary"),
                         DT::DTOutput("log"),
                         column(6, wellPanel(
                           h4("Calories"),
                           gaugeOutput("cauge")),wellPanel(
                             h4("Fat"),
                                gaugeOutput("falala"))),
                         column(6,wellPanel(
                           h4("Protein"), gaugeOutput("paige")),
                           wellPanel(
                             h4("Carbs"),
                                gaugeOutput("cabbage")))),

              )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter recipes based on selected ingredients
  filtered_recipes <- reactive({
    selected_recipes <- recipes
    for (ingredient in input$selected_ingredients) {
      ingredient_plural <- paste0(ingredient, "s")
      selected_recipes <- selected_recipes[grepl(paste0("(^|,)\\s*", ingredient, "\\s*(,|$)|(^|,)\\s*", ingredient_plural, "\\s*(,|$)"), selected_recipes$ingredient_list, ignore.case = TRUE), ]
    }
    return(selected_recipes)
  })
  
  # Store the datatable as its own object
  output$foods <- DT::renderDataTable({
    selected_recipes <- filtered_recipes()
    
    # Subset selected recipes to include only recipe name and NER columns
    selected_recipes <- selected_recipes[, c("ID", "title", "ingredient_list"), drop = TRUE]
    
    # Store the datatable as its own object
    datatable <- DT::datatable(selected_recipes, options = list(rownames = FALSE))
    
    # Return the datatable
    datatable
  })
  
  # Define reactive expression to retrieve more information about the selected recipe
  selected_recipe_info <- reactive({
    req(input$foods_rows_selected)
    
    # Reference the selected row index in the data table
    selected_row <- input$foods_rows_selected
    
    # Retrieve the ID of the selected recipe directly from the filtered dataframe
    selected_recipe_id <- filtered_recipes()$ID[selected_row]
    
    # Retrieve the corresponding recipe information from the original dataframe
    selected_recipe <- recipes[recipes$ID == selected_recipe_id, ]
    
    # Return the selected recipe
    selected_recipe
  })
  
  output$namaste <- renderUI({
    selected_recipe <- selected_recipe_info()
    if (!is.null(selected_recipe)) {
      # Create the HTML for the ingredient list
      namaste <- paste("<h2>",selected_recipe$title,"</h2>")
      
      # Return the ingredient list as HTML
      HTML(namaste)
    }
    
  })
    
  
  # Display additional information about the selected recipe as HTML
  output$llama <- renderUI({
    selected_recipe <- selected_recipe_info()
    if (!is.null(selected_recipe)) {
      # Initialize an empty character vector to store the HTML lines
      step_listed <- character()
      
      # Iterate over each step in the "steps" list
      for (step in selected_recipe$steps) {
        # Check if the step is not NULL
        if (!is.null(step)) {
          # Create the HTML line for the current step and add it to the vector
          step_listed <- c(step_listed, paste("<li>", step, "</li>"))
        }
      }
      
      # Combine the HTML lines into a bulleted list for the steps
      list_steps <- paste("<h4>Directions:</h4>",paste("<ul>", paste(step_listed, collapse = ""), "</ul>"))
      
      # Return the bulleted list for steps
      HTML(list_steps)
    }
  })
  
  # Display additional information about the selected recipe as HTML
  output$koala <- renderUI({
    selected_recipe <- selected_recipe_info()
    if (!is.null(selected_recipe)) {
      # Initialize an empty character vector to store the HTML lines
      quant_list <- character()
      
      for (quantity in selected_recipe$quantities) {
        # Check if not NULL
        if (!is.null(step)) {

          quant_list <- c(quant_list, paste("<li>", quantity, "</li>"))
        }
      }
      

      list_quant <- paste("<h4>Ingredients:</h4>",paste("<ul>", paste(quant_list, collapse = ""), "</ul>"))
      
      HTML(list_quant)
    }
  })
  
  
  
  # Define a reactive expression to calculate the total macronutrient values for the selected recipe
  macro_values <- reactive({
    selected_recipe <- selected_recipe_info()
    if (!is.null(selected_recipe)) {
      # Split the ingredient list into individual ingredients
      ingredients_list <- unlist(strsplit(selected_recipe$ingredient_list, ", "))
      
      # Initialize empty vectors to store the total nutritional values
      total_calories <- 0
      total_fat <- 0
      total_protein <- 0
      total_carbs <- 0
      
      # Iterate through each ingredient in the list
      for (ingredient in ingredients_list) {
        # Check if the ingredient exists in either ingredient_singular or food_name
        if (ingredient %in% ingredients$ingredient_singular || ingredient %in% ingredients$food_name) {
          # Retrieve the nutritional values for the ingredient from the ingredients dataframe
          ingredient_nutrition <- ingredients[ingredients$ingredient_singular == ingredient | ingredients$food_name == ingredient, c("energy_100g", "proteins_100g", "carbohydrates_100g", "fat_100g")]
          # Add the nutritional values to the totals
          total_calories <- total_calories + sum(ingredient_nutrition$energy_100g)
          total_fat <- total_fat + sum(ingredient_nutrition$fat_100g)
          total_protein <- total_protein + sum(ingredient_nutrition$proteins_100g)
          total_carbs <- total_carbs + sum(ingredient_nutrition$carbohydrates_100g)
        }
      }
      
      # Return the total macronutrient values as a named list
      list(Calories = total_calories, Fat = total_fat, Protein = total_protein, Carbs = total_carbs)
    } else {
      # Return NULL if no recipe is selected
      NULL
    }
  })
  
  # Render the HTML for the total macronutrient values
  output$macro <- renderUI({
    macro_info <- macro_values()
    if (!is.null(macro_info)) {
      macro_html <- paste("<h4>Total Macronutrient Values:</h4>",
                          "<ul>",
                          paste("<li>Calories:", macro_info$Calories, "</li>"),
                          paste("<li>Fat:", macro_info$Fat, "</li>"),
                          paste("<li>Protein:", macro_info$Protein, "</li>"),
                          paste("<li>Carbs:", macro_info$Carbs, "</li>"),
                          "</ul>")
      HTML(macro_html)
    }
  })
  
  # Predefined column names
  column_names <- c("ID", "Title", "Calories", "Fat", "Protein", "Carbs")
  
  # Data frame to store selected recipes with predefined column names
  initial_data <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(initial_data) <- column_names
  selected_recipes_df <- reactiveVal(initial_data)
  
  # Add button to add selected recipe to data table
  observeEvent(input$add, {
    if (!is.null(input$foods_rows_selected)) {
      selected_row <- input$foods_rows_selected
      selected_recipe <- filtered_recipes()[selected_row, ]
      # Get the macro values for the selected recipe
      macro_info <- macro_values()
      if (!is.null(macro_info)) {
        # Add the selected recipe to the data table
        new_row <- c(ID = selected_recipe$ID, Title = selected_recipe$title, 
                     Calories = macro_info$Calories, Fat = macro_info$Fat, 
                     Protein = macro_info$Protein, Carbs = macro_info$Carbs)
        # Merge the new row with the existing data and set column names
        existing_data <- selected_recipes_df()
        new_data <- rbind(existing_data, new_row)
        colnames(new_data) <- column_names
        selected_recipes_df(new_data)
      }
    }
  })
  
# Remove button to remove selected recipe from data table
observeEvent(input$remove, {
  if (!is.null(input$log_rows_selected)) {
    selected_row <- input$log_rows_selected
    selected_recipes_df(selected_recipes_df()[-selected_row, ])
  }
})

  
  # Render data table for selected recipes
  output$log <- DT::renderDataTable({
    DT::datatable(selected_recipes_df(), options = list(rownames = FALSE))
  })
  
  
  ### Calculator Section
  calc_result <- eventReactive(input$calc, {
    height <- input$height
    weight <- input$weight
    age <- input$age
    gender <- input$gender
    if (gender == "Male") {
      result <- 66.47 + (6.24 * weight) + (12.7 * height) - (6.755 * age)
    } else if (gender == "Female") {
      result <- 655.1 + (4.35 * weight) + (4.7 * height) - (4.7 * age)
    } else if (gender == "Prefer not say"){
      result <- ((66.47 + (6.24 * weight) + (12.7 * height) - (6.755 * age)) + 
                   (655.1 + (4.35 * weight) + (4.7 * height) - (4.7 * age))) / 2
    }
    bmr <- round(result * 1.5, digits = 0)
    
    protein <- round((bmr * 0.25) / 4,digits = 0)
    carbs <- round((bmr * 0.5) / 4, digits = 0)
    fat <- round((bmr * 0.25) / 9, digits = 0)
    return(list(bmr = bmr, protein = protein, fat = fat,carbs = carbs))
  })
  
  
  # Calculate the calorie goal based on user input
  calorie_goal <- reactive({
    if (!is.null(input$calc) && input$calc > 0) {
      bmr <- calc_result()$bmr
      return(round(bmr, digits = 2))
    } else {
      return(0)  # Return 0 if calc hasn't been clicked
    }
  })
  
  
  # Calculate the calorie goal based on user input
  protein_goal <- reactive({
    if (!is.null(input$calc) && input$calc > 0) {
      protein <- calc_result()$protein
      return(round(protein, digits = 2))
    } else {
      return(0)  # Return 0 if calc hasn't been clicked
    }
  })
  
  # Calculate the calorie goal based on user input
  fat_goal <- reactive({
    if (!is.null(input$calc) && input$calc > 0) {
      fat <- calc_result()$fat
      return(round(fat, digits = 2))
    } else {
      return(0)  # Return 0 if calc hasn't been clicked
    }
  })
  
  # Calculate the calorie goal based on user input
  carb_goal <- reactive({
    if (!is.null(input$calc) && input$calc > 0) {
      carbs <- calc_result()$carbs
      return(round(carbs, digits = 2))
    } else {
      return(0)  # Return 0 if calc hasn't been clicked
    }
  })
  
  #### Make text HTML and larger to be more legible and pop
  output$camel <- renderText({
    if (!is.null(input$calc) && input$calc > 0) {
      paste("Daily Calorie Goal:", round((calc_result()$bmr), digits = 2))
    }
  })
  
  output$popcorn <- renderText({
    if (!is.null(input$calc) && input$calc > 0) {
      paste("Daily Protein Goal (g):", round(calc_result()$protein, digits = 2))
    }
  })
  
  output$caravan <- renderText({
    if (!is.null(input$calc) && input$calc > 0) {
      paste("Daily Carbs Goal (g):", round(calc_result()$carbs, digits = 2))
    }
  })
  
  output$falafel <- renderText({
    if (!is.null(input$calc) && input$calc > 0) {
      paste("Daily Fat Goal (g):", round(calc_result()$fat, digits = 2))
    }
  })
  
  # Define a reactive expression to calculate the total calories for the log
  total_mac <- reactive({
    # Get the data frame of selected recipes
    log_data <- selected_recipes_df()
    
    # Calculate the total calories
    total_calories <- sum(as.numeric(log_data$Calories), na.rm = TRUE)
    total_protein <- sum(as.numeric(log_data$Protein), na.rm = TRUE)
    total_fat <- sum(as.numeric(log_data$Fat), na.rm = TRUE)
    total_carbs <- sum(as.numeric(log_data$Carbs), na.rm = TRUE)
    
    # Return the total calories
    list(Calories = total_calories, Protein = total_protein, Fat = total_fat, Carbs = total_carbs)
  })
  
  
  # Render the gauge based on the total calories
  output$cauge <- renderGauge({
    total_mac_value <- total_mac()
    gauge(total_mac_value$Calories, 
          min = 0, 
          max = calorie_goal(), 
          sectors = gaugeSectors(success = c(0.8 * calorie_goal(), calorie_goal()), 
                                warning = c(0.2 * calorie_goal(), 0.8 * calorie_goal()),
                                danger = c(0, 0.2 * calorie_goal())))
  })
  
  # Render the gauge based on the total proteins
  output$paige <- renderGauge({
    total_mac_value <- total_mac()
    gauge(total_mac_value$Protein, 
          min = 0, 
          max = protein_goal(), 
          sectors = gaugeSectors(success = c(0.8 * protein_goal(), protein_goal()), 
                                 warning = c(0.2 * protein_goal(), 0.8 * protein_goal()),
                                 danger = c(0, 0.2 * protein_goal())))
  })
  
  # Render the gauge based on the total fat
  output$falala <- renderGauge({
    total_mac_value <- total_mac()
    gauge(total_mac_value$Fat, 
          min = 0, 
          max = fat_goal(), 
          sectors = gaugeSectors(success = c(0.8 * fat_goal(), fat_goal()), 
                                 warning = c(0.2 * fat_goal(), 0.8 * fat_goal()),
                                 danger = c(0, 0.2 * fat_goal())))
  })
  
  # Render the gauge based on the total fat
  output$cabbage <- renderGauge({
    total_mac_value <- total_mac()
    gauge(total_mac_value$Carbs, 
          min = 0, 
          max = carb_goal(), 
          sectors = gaugeSectors(success = c(0.8 * carb_goal(), carb_goal()), 
                                 warning = c(0.2 * carb_goal(), 0.8 * carb_goal()),
                                 danger = c(0, 0.2 * carb_goal())))
  })
  
}


# Run the app
shinyApp(ui = ui, server = server)
