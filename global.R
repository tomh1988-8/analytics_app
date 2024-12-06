rm(list = ls())

# Libraries galore...
library(shinydashboard)
library(shiny)
library(DT)
library(tidyverse)
library(kableExtra)
library(reshape2)
library(janitor)
library(zoo)
library(rlang)
library(glue)
library(stringr)
library(zoo)
library(plotly)
library(fresh)
library(sass)
library(sf)
library(shinyWidgets)
library(writexl)

# Theme for the app
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#333333" # Dark gray for accent colors
  ),
  adminlte_global(
    content_bg = "#FFFFFF", # White background for the main body
    box_bg = "#FFFFFF", # White background for box elements
    info_box_bg = "#FFFFFF" # White background for info boxes
  ),
  adminlte_sidebar(
    dark_bg = "#333333", # Charcoal for sidebar background
    dark_color = "#FFFFFF", # White text for sidebar elements
    dark_hover_bg = "#555555", # Slightly lighter gray for hover effect
    dark_hover_color = "#FFFFFF" # White text for hover state
  )
)

# read in objects - DEFAULT APP
MART.Dash <- readRDS("MART.Dash.rds") %>%
  select(-c(Demographic.Age,BP_ID))


# # 
# MART.Dash <- readRDS("MART.Dash.Extended.rds") %>%
#   #mutate(across(where(is.character), as.factor)) %>%
#   select(-Demographic.Age)

# # ###  SWITCH TO ANY DATAFRAME YOU WANT
# MART.Dash <- readRDS("VL_MART.Dash.rds") %>%
#   select(-Demographic.Age)



shape_data <- readRDS("shape_data.rds")
shapes_la <- readRDS("shapes_la.rds")
shapes_bmra <- readRDS("Shape.BMRA.rds")
shapes_icb <- readRDS("shapes_icb.rds")

# Create a copy of the original dataset
MART.Dash_Original <- MART.Dash