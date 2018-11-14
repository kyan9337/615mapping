library(leaflet)
library(sp)
library(magrittr)
library(maps)
library(htmltools)
library(rgdal)
library(data.table)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram

dono <- read_csv("11_5_MASSCONTRIBUTIONS_csv.csv")

# 
# donor_Q01 <- dono %>% group_by(contrib, city, state) %>%
#   summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
# 
# 
# 
# donor_Q02 <- dono %>% group_by(party) %>% 
#   summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
# 
# 
# 
# 
# donor_Q04 <- dono %>% group_by(recipient, party) %>% 
#   summarize(total = sum(amount), num = n(), avg = mean(amount)) %>% arrange(desc(total))
# 
# 
# 
# donor_Q08a <- dono %>% group_by(city, state) %>% 
#   summarize(total = sum(amount), num = n()) %>% arrange(desc(total))


donor_Q08aa <- dono %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))

#dim(donor_Q08aa)

dmap <- spread(donor_Q08aa, party, party)

#dim(dmap)

dmap_stR <- dmap %>% filter(R==R)

dmap_stD <- dmap %>% filter(D==D)

dmap_stI <- dmap %>% filter(I==I)

# dim(dmap_stR)
# sum(is.na(dmap_stR$R))
# sum(is.numeric(dmap_stR$total))
# sum(is.na(dmap_stR$D))
# sum(is.na(dmap_stR$I))
# 
# 
# dim(dmap_stD)
# sum(is.na(dmap_stD$R))
# sum(is.na(dmap_stD$D))
# sum(is.na(dmap_stD$I))
# 
# 
# dim(dmap_stI)
# sum(is.na(dmap_stI$R))
# sum(is.na(dmap_stI$D))
# sum(is.na(dmap_stI$I))

###################################### Republican donor map

dmapR <- dmap_stR %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

### usa <- map_data("usa")

states_R <- map_data("state")
states_R %<>% select(long,lat,group,order,region) %>% rename(state=region)

st_name_r <- unique(states_R$state)

st_abrev_r <- dmapR$state

## st_name

# st_abrev <- as.data.frame(st_abrev)
# write_delim(st_abrev,"abrev.csv")
# 
# st_name <- as.data.frame(st_name)
# write_csv(st_name, "st_name.csv")

# dim(st_name)

st_r <- read_csv("states.csv")
st_r %<>% rename(state=st_name)

states_R <- left_join(states_R, st_r, by="state")

dmapR %<>% rename(st_abrev_r=state)
states_R%<>% rename(st_abrev_r=st_abrev)
states_R <- left_join(states_R, dmapR, by="st_abrev_r")
states_R$Donors <- as.character(states_R$Donors)



states_center_r <- states_R %>% group_by(st_abrev_r) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = st_abrev_r)

states_data_r <- states_R %>% select(st_abrev_r,Donations, Donors) %>% unique()


states_center_r <- left_join(states_center_r, states_data_r, by=c("st_abrev_r"))
#####################################

## capture number of donors before these rows are deleted from states_center
##states_center[states_center$st_abrev=="MA",]
don_ma <- states_center_r[states_center_r$st_abrev_r=="MA",]$Donors
don_ri <- states_center_r[states_center_r$st_abrev_r=="RI",]$Donors
don_ct <- states_center_r[states_center_r$st_abrev_r=="CT",]$Donors

## remove rows for MA, Ri, CT

states_center_r %<>% filter(!(st_abrev_r=="MA" | st_abrev_r=="RI" | st_abrev_r=="CT"))

### Adjust location of state labels

states_center_r[states_center_r$st_abrev_r=="ID",]$long=-115.5
states_center_r[states_center_r$st_abrev_r=="MI",]$long=-84.7
states_center_r[states_center_r$st_abrev_r=="MI",]$lat=43
states_center_r[states_center_r$st_abrev_r=="VT",]$long=-72.7
states_center_r[states_center_r$st_abrev_r=="VT",]$lat=44.4
states_center_r[states_center_r$st_abrev_r=="NH",]$lat=43.6
states_center_r[states_center_r$st_abrev_r=="FL",]$long=-81.7
states_center_r[states_center_r$st_abrev_r=="LA",]$long=-92.5


dmapD <- dmap_stD %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

### usa <- map_data("usa")

states_D <- map_data("state")
states_D %<>% select(long,lat,group,order,region) %>% rename(state=region)

st_name_d <- unique(states_D$state)

st_abrev_d <- dmapR$state

## st_name

# st_abrev <- as.data.frame(st_abrev)
# write_delim(st_abrev,"abrev.csv")
# 
# st_name <- as.data.frame(st_name)
# write_csv(st_name, "st_name.csv")

# dim(st_name)

st_d <- read_csv("states.csv")
st_d %<>% rename(state=st_name)

states_D <- left_join(states_D, st_d, by="state")

dmapD %<>% rename(st_abrev_d=state)
states_D%<>% rename(st_abrev_d=st_abrev)
states_D <- left_join(states_D, dmapD, by="st_abrev_d")
states_D$Donors <- as.character(states_D$Donors)



states_center_d <- states_D %>% group_by(st_abrev_d) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = st_abrev_d)

states_data_d <- states_D %>% select(st_abrev_d,Donations, Donors) %>% unique()


states_center_d <- left_join(states_center_d, states_data_d, by=c("st_abrev_d"))
#####################################

## capture number of donors before these rows are deleted from states_center
##states_center[states_center$st_abrev=="MA",]
don_ma <- states_center_d[states_center_d$st_abrev_d=="MA",]$Donors
don_ri <- states_center_d[states_center_d$st_abrev_d=="RI",]$Donors
don_ct <- states_center_d[states_center_d$st_abrev_d=="CT",]$Donors

## remove rows for MA, Ri, CT

states_center_d %<>% filter(!(st_abrev_d=="MA" | st_abrev_d=="RI" | st_abrev_d=="CT"))

### Adjust location of state labels

states_center_d[states_center_d$st_abrev_d=="ID",]$long=-115.5
states_center_d[states_center_d$st_abrev_d=="MI",]$long=-84.7
states_center_d[states_center_d$st_abrev_d=="MI",]$lat=43
states_center_d[states_center_d$st_abrev_d=="VT",]$long=-72.7
states_center_d[states_center_d$st_abrev_d=="VT",]$lat=44.4
states_center_d[states_center_d$st_abrev_d=="NH",]$lat=43.6
states_center_d[states_center_d$st_abrev_d=="FL",]$long=-81.7
states_center_d[states_center_d$st_abrev_d=="LA",]$long=-92.5

# code for reviewing and changine location values

#
# states_center[states_center$st_abrev=="LA",]
# 
# states_center[states_center$st_abrev=="LA",]$long=-92.5
# states_center[states_center$st_abrev=="NH",]$lat=43.6
# 
# states_center[states_center$st_abrev=="ID",]

##########################

###########################





ui <- fluidPage(
  titlePanel("Donation Info"),
    sidebarPanel(
      selectInput("Party", "Choose Political Party:", choices = c("Democrats","Republican"), selected = "Democrats")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )





# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$plot <- renderPlotly({
  if(input$Party=="Republican"){
   p <- ggplot(data = states_R) + 
     geom_polygon(aes(x = long, y = lat, fill = log10(Donations), 
                      group = group), color = "white") + 
     coord_fixed(1.3) + 
     
     labs(title = "Donors to Massachusetts Republicans",
          caption = "Number of Donors by State") +
     
     
     scale_fill_gradient("Donations", low="pink1", 
                         high="red4", 
                         breaks=c(3.4, 4.7, 6),
                         labels=c("low", "", "high") ) +
     
     
     geom_text(data = states_center_r,
               aes(x=long, y=lat, label = Donors),size=2, color="white") +
     
     
     annotate("text", x = -66, y = 42, label = paste("MA:",don_ma), size=3)  +
     annotate("text", x = -66, y = 41, label = paste("RI:", don_ri), size=3) +
     annotate("text", x = -66, y = 40, label = paste("CT:", don_ct), size=3) +
     
     theme(text = element_text(size=10),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank(),
           
           panel.background = element_rect(fill = "gray71", color="blue"))
   options(viewer = T)
   ggplotly(p) }
    else if(input$Party=="Democrats"){
      p <- ggplot(data = states_D) + 
        geom_polygon(aes(x = long, y = lat, fill = log10(Donations), 
                         group = group), color = "white") + 
        coord_fixed(1.3) + 
        
        labs(title = "Donors to Massachusetts Democrats",
             caption = "Number of Donors by State") +
        
        
        scale_fill_gradient("Donations", low="pink1", 
                            high="blue4", 
                            breaks=c(3.4, 4.7, 6),
                            labels=c("low", "", "high") ) +
        
        
        geom_text(data = states_center_d,
                  aes(x=long, y=lat, label = Donors),size=2, color="white") +
        
        
        annotate("text", x = -66, y = 42, label = paste("MA:",don_ma), size=3)  +
        annotate("text", x = -66, y = 41, label = paste("RI:", don_ri), size=3) +
        annotate("text", x = -66, y = 40, label = paste("CT:", don_ct), size=3) +
        
        theme(text = element_text(size=10),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              
              panel.background = element_rect(fill = "gray71", color="blue"))
      
      ggplotly(p) }
    
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)