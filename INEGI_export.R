library(readxl)
library(tidyverse)

### DATA IMPORT AND TIDY
### --------------------

## Import from Excel
estados <- excel_sheets('ignore/EAEF_Entidad_Subsector.xlsx')
exportation <- read_xlsx('ignore/EAEF_Entidad_Subsector.xlsx',
                           range = "A5:N5", sheet = 1,
                         col_types = c("numeric", "text", rep("numeric", 12)))
exportation <- mutate(exportation, state = "Aguascalientes")

for (i in 1:length(estados)){
    extracted <- read_xlsx('ignore/EAEF_Entidad_Subsector.xlsx',
                           range = "A7:N31", sheet = i,
                 col_names = F, na = "-",
                 col_types = c("numeric", "text", rep("numeric", 12)))
    colnames(extracted) <- colnames(exportation)[1:14]
    extracted <- mutate(extracted,
                        state = estados[i])
    exportation <- full_join(exportation, extracted)
}

colnames(exportation)[c(3, 14)] <- c("2007", "2018")
exportation$`Código` <- parse_factor(as.character(exportation$`Código`))

## Tidying data

export1 <- exportation %>%
    pivot_longer(cols = `2007`:`2018`,
                 names_to = "year",
                 values_to = "USD",
                 values_drop_na = T) %>%
    mutate(year = parse_double(year))

export <- export1 %>%
    pivot_wider(names_from = "Descripción",
                values_from = USD,
                id_cols = c("state", "year"))

## Save this last 2 for later usage

write_csv(export1, "exportations_activity_rows.csv")
write_csv(export, "exportations_activity_cols.csv")

### GENERAL ANALYSIS
### ----------------

## Now let's import the data as our new CSV files

export.rows <- read_csv("exportations_activity_rows.csv")
export.cols <- read_csv("exportations_activity_cols.csv")

## get the exact names of each variable
categorias <- colnames(export.cols)[3:27]
colnames(export.cols)

## look at the totals

## Total exportations per state
##

export.cols %>%
    group_by(state) %>%
    summarise(`total export` = sum(!!sym(categorias[1]))) %>%
    arrange(desc(`total export`)) %>%
    print(n = Inf)

export.cols %>%
    group_by(state) %>%
    summarise(`total export` = sum(!!sym(categorias[1]))) %>%
    ggplot() +
    geom_bar(aes(y = `total export`,
                 x = reorder(state, `total export`, FUN = abs),
                 fill = `total export`),
             stat = 'identity') +
    coord_flip()

## Totals per category

export.rows %>%
    filter(`Descripción` != categorias[1]) %>%
    group_by(`Descripción`) %>%
    summarise(Total = sum(USD)) %>%
    arrange(desc(Total)) %>%
    print(n = Inf)

export.rows %>%
      filter(`Descripción` != categorias[1]) %>%
      group_by(`Descripción`) %>%
      summarise(Total = sum(USD)) %>%
      ggplot() +
      geom_bar(aes(y = Total,
                   x = reorder(`Descripción`, Total, FUN = abs),
                   fill = Total),
               stat = 'identity') +
      coord_flip()

## Total exportations per year
##

export.cols %>%
    group_by(year) %>%
    summarise(`total export` = sum(!!sym(categorias[1]))) %>%
    print(n = Inf)

export.rows %>%
    filter(`Descripción` != categorias[1]) %>%
    group_by(year) %>%
    summarise(Total = sum(USD)) %>%
    ggplot(aes(x = year, y = Total)) +
    geom_line() +
    geom_point() 

export.rows %>%
    filter(`Descripción` != categorias[1]) %>%
    group_by(year, state) %>%
    summarise(Total = sum(USD)) %>%
    ggplot(aes(x = year, y = Total)) +
    geom_line(aes(colour = state))+
    geom_point(aes(colour = state))

## To answer the question: Is every year the same state and same
## activity making the biggest money?

## STATE
export.cols %>%
    group_by(year) %>%
    filter(!!sym(categorias[1]) == max(!!sym(categorias[1]))) %>%
    select(year, state, !!sym(categorias[1])) %>%
    arrange(year)

## Activity
export.rows %>%
    filter(`Descripción` != categorias[1]) %>%
    group_by(year) %>%
    filter(USD == max(USD)) %>%
    arrange(year)

## Very interesting results: the states exporting the most are different than
## the states with the activity contributing the most, per year. Also,
## until 2009 is the same state and then it changes, but per activity
## it remains until 2013, and then it changes, both state and activity.
##
## IDEA: Maybe we can observe this 4 states with more detail
    

### IDEA ---------------------------------------------------------------
### You can make a shiny app that shows per state, activity, or USD,
### statistics like, the main production per state, or the main states
### producing X stuff.
###
### Here is a starting point for it

## FUNCTION TO CHOSEE MAIN ACTIVITY PER STATE
plot_state <- function(estado, USD_min = 5000000){
    export.rows %>%
        filter(`Descripción` != "Exportaciones totales") %>%
        group_by(state, `Descripción`)  %>%
        summarise(Total = sum(USD)) %>%
        filter(state == estado &
               Total >= USD_min) %>%
        ggplot() +
        geom_bar(aes(y = Total,
                     x = reorder(`Descripción`, Total, FUN = abs),
                     fill = Total),
                 stat = 'identity') +
        coord_flip() +
        labs(title = estado,
             y = "Total USD", x = NULL) +
        theme(legend.position="none")
}

## Now the following two plots are equivalent

## CHIHUAHUA
export.rows %>%
    filter(state == "Chihuahua" &
           `Descripción` != "Exportaciones totales") %>%
    group_by(`Descripción`) %>%
    summarise(Total = sum(USD)) %>%
    filter(Total > 5000000) %>%
    ggplot() +
    geom_bar(aes(y = Total,
                 x = reorder(`Descripción`, Total, FUN = abs),
                 fill = Total),
             stat = 'identity') +
    coord_flip()

plot_state("Chihuahua")

## FUNCTION TO CHOSEE MAIN STATES IN A GIVEN ACTIVITY
plot_activity <- function(activity_id, USD_min = 5000000){
    activity <- colnames(export.cols)[3:27]
    export.cols %>%
        select(state, year, activity[activity_id]) %>%
        group_by(state)  %>%
        summarise(Total = sum(!!sym(activity[activity_id]))) %>%
        filter(Total >= USD_min) %>%
        ggplot() +
        geom_bar(aes(y = Total,
                     x = reorder(state,
                                 Total, FUN = abs),
                     fill = Total),
                 stat = 'identity') +
        coord_flip() +
        labs(title = activity[activity_id],
             y = "Total USD", x = NULL) +
        theme(legend.position="none")
}

## Now let's create clear id numbers for the activities
activities.id <- data.frame(id = c(1:25),
                            Activity = colnames(export.cols)[3:27])

## Test
plot_activity(1) # Exportaciones totales
plot_activity(21, USD_min = 1000)

### analysis :: BACK

## Check other main exporters (Top 5)

plot_state("Chihuahua")
plot_state("Baja California")
plot_state("Coahuila de Zaragoza")
plot_state("Nuevo León")
plot_state("Tamaulipas")

### IDEA |
### It seems that producing computer equipment is the main in 3 of the
### top 5 states, while the wealthiest activity over all is transport
### equipment production. Let's look with more detail at this

plot_activity(14) 
plot_activity(15)

## Now is very clear the reason why
