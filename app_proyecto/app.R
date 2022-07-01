library(ggmosaic)
library(tidyverse)
library(here)
library(readxl)
library(xtable)
library(maptools)
library(rgdal)
library(patchwork)
library(plotly)
library(shiny)
#install.packages('shinythemes')
library(shinythemes)
#install.packages('bslib')
library(bslib)
library(shinydashboard)
datos <- read_xlsx(here("Datos","Datos_base_ganadera.xlsx"))
ui <- fluidPage(theme= bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Datos de Ministerio de Ganadería, Agricultura y Pesca del Uruguay"),
  actionButton("ver", "Mostrar los graficos y tabla",icon = icon("redo")),
  actionButton("nover", "Ocultar los graficos y tabla"),
  hr(),
  navbarPage("Grupo 2",
             tabPanel(icon("home"),
              fluidRow(column(tags$img(src="imag1.jpg",width="200px",height="260px"),width=2),
                      column(
                        br(),
                        p(" ", 
                          strong("
                          En este trabajo se propone una descripción de los resultados de
                                 la encuesta sin expandir a través de ponderadores por lo que los 
                                 resultados a los que arribemos solo serán validos para estos casos
                                 y no serán conclusiones que se puedan extender al conjunto de los
                                 productores ganaderos.")),
                        br(),
                        p("Los integrantes son nacho victoria y angel",
                        em(" ")),
                        width=8))),
             tabPanel("Analisis de los productores",
                      plotOutput("scat"),
                      selectInput('vor','Variable',c('Condicion Juridica'='a9','Nivel de enseñanza'='a12',
                                                     'Rango de edad'='cut(a11, breaks = 5,labes=FALSE)',
                                                     'Innovacion'='a1')),
                      selectInput('var','Colorear por',c('Condicion Juridica'='a9','Nivel de enseñanza'='a12',
                                                 'Personal Profesional'='prof_1',
                                                 'Sexo'='a10'))),
             tabPanel("Analisis general",
                      selectInput('grafico','Que grafico le interesa?',
                                  c('mosaico','barras','boxplot','boxplot2')),
                      plotOutput("barplot"))))
server <- function(input, output){
  datos_1 <- data.frame(Variable = c("a11","a9","a12","aiv_nueva","a16_i","dpto","b3_i","b3_7","est"),
                        Descripcion = c("Edad","Condición Jurídica","Nivel de Educación",
                                        "Cantidad innovaciones realizadas en 8 posibles ramas distintas",
                                        "Tipo de energía utilizada",
                                        "Departamento donde se ubica la propiedad",
                                        "Tenencia de Tierras","Total del territorio en ha",
                                        "Unidades ganaderas en el territorio (1-8)"))
    output$barplot<- renderPlot({
      if(input$grafico=='mosaico'){
        datos %>% 
          group_by(b5,b3_7,est) %>% 
          filter(b5!=0) %>% 
          summarise(a=b5*b3_7/100)%>%
          mutate(a = case_when(a %in% c(0:200)  ~ "0-200",
                               a %in% c(201:400)  ~ "200-400",
                               a %in% c(401:600)  ~ "400-600",
                               a > 600 ~ "600")) %>%
          filter(a=="0-200",a=="200-400",a=="400-600",a=="600") %>% 
          ggplot() +
          geom_mosaic(aes(x = product(a, est), fill= a))+
          scale_fill_brewer(palette = "Set2")+
          theme_bw()
      }else{
        if(input$grafico=='barras'){
        Cant_CJ<- datos %>%
          group_by(a9) %>% summarise(Cant=n())
        
        Cant_Z<-datos %>% 
          filter(pz_1==1,pz_2>0,pz_3>0) %>% 
          group_by(a9) %>% 
          summarise(Cant_z=n())
        
        Cant_PP<- datos %>% 
          filter(prof_1==1,prof1==1|prof2==1|prof3==1|prof4==1|prof5==1|prof6==1|prof7==1|prof8==1) %>%
          group_by(a9) %>% 
          summarise(Cant_pp=n())
        
        tabla_porcentaje<- inner_join(inner_join(Cant_CJ, Cant_Z, by = "a9"),Cant_PP,by="a9") %>%
          mutate(Porc_PP=Cant_pp/Cant) %>%
          mutate(Porc_Z=Cant_z/Cant)
        
        ggplot(tabla_porcentaje)+
          geom_col(aes(y=Porc_PP*100,x=a9,fill=factor(a9)))+
          geom_text(aes(x=a9,y=Porc_PP*100,label=round(Porc_PP*100)),vjust=2)+
          theme(legend.position = 'none')+
          scale_y_continuous()+
          labs(y='Porcentaje Personal Profesional')+
          scale_y_continuous(breaks = c(20,40,60,80))+
          ggplot(tabla_porcentaje)+
          geom_col(aes(x=a9,y=Porc_Z*100,fill=factor(a9)))+
          geom_text(aes(x=a9,y=Porc_Z*100,label=round(Porc_Z*100)),vjust=2)+
          theme(legend.position = 'none')+
          scale_y_continuous()+
          labs(y='Porcentaje Personal zafral')+
          scale_y_continuous(breaks = c(20,40,60,80)) 
        }else
        {if(input$grafico=='boxplot'){
          datos %>% 
            filter(pz_1==1,pz_2>0,pz_3>0) %>% 
            group_by(a9,a12) %>%
            ggplot()+geom_boxplot(aes(y=pz_3/pz_2,fill=factor(a9)))+
            scale_y_log10()+
            facet_grid(~a9)+
            scale_x_discrete(name='Condicion Juridica')+
            labs(y="Renumeración/Jornal")
        }else{
          datos %>% 
            filter(pz_1==1,b3_7>100,pz_2!=0,pz_3!=0) %>% 
            summarise('Sueldo_por_jornal'=pz_3/pz_2) %>% 
            ggplot()+geom_boxplot(aes(y=Sueldo_por_jornal))
        }
        
      }
      
    }
  })
  output$scat<- renderPlot({
    if(input$vor=='a9' | input$vor=='a12'){
      datos %>% 
      group_by(a9) %>% 
      ggplot(aes(x=(.data[[input$vor]]),fill=factor(.data[[input$var]])))+
      geom_bar()+
      labs(x='',y="Cantidad")+
      theme_bw()
    }else{
      if(input$vor=='a1'){
        aio_nueva <- datos %>% 
          mutate(aio_1 = ifelse(aio_1 == "2", 0, aio_1)) %>% 
          mutate(aio_2 = ifelse(aio_2 == "2", 0, aio_2)) %>% 
          mutate(aio_3 = ifelse(aio_3 == "2", 0, aio_3)) %>% 
          mutate(aio_4 = ifelse(aio_4 == "2", 0, aio_4)) %>% 
          mutate(aio_5 = ifelse(aio_5 == "2", 0, aio_5)) %>% 
          mutate(aio_6 = ifelse(aio_6 == "2", 0, aio_6)) %>% 
          mutate(aio_7 = ifelse(aio_7 == "2", 0, aio_7)) %>% 
          mutate(aio_8 = ifelse(aio_8 == "2", 0, aio_8))
        
        aio_nueva = rowSums(aio_nueva[ ,c("aio_1","aio_2","aio_3","aio_4","aio_5","aio_6","aio_7","aio_8")])
        
        datos<-cbind(datos,aio_nueva)
        datos %>% 
          group_by(a9,aio_nueva,a12) %>% 
          mutate(a12 = case_when(a12 %in% c(0,1,2)  ~ "Básico",
                                 a12 %in% c(3,4) ~ "Secundaria",
                                 a12 %in% c(5,6) ~ "Técnico univeritario")) %>% 
          filter(a12!="Otros") %>% 
          mutate(aio_nueva = case_when(aio_nueva %in% c(0)  ~ "0 nin",
                                       aio_nueva %in% c(1,2)  ~ "1 o 2",
                                       aio_nueva %in% c(3,4,5) ~ "3 a 5",
                                       aio_nueva %in% c(6,7,8) ~ "6 a 8")) %>% 
          filter(a9=="1",a12!="0") %>% 
          summarise(n=n() ) %>%
          mutate(freq=n/sum(n)) %>%
          ggplot(aes(x=factor(aio_nueva),y=freq,fill=factor(.data[[input$var]])))+
          geom_bar(stat="identity")+
          labs(x="Innovación",y="Frecuencia")+
          scale_fill_brewer(name="Nivel educativo",palette = "Accent")+ theme_bw()+scale_x_discrete(name='none')
        
      }else{
        datos %>% 
          filter(a9!=0,a10!=0,a10>0) %>% 
          ggplot(aes(x=cut(a11, breaks = 5,labes=FALSE),fill=factor(.data[[input$var]])))+
          geom_bar()+                  
          labs(x='Rango de edades',y='Cantidad')+
          scale_x_discrete(name="Rango de edad",
                           labels = c('Menor a 20','Entre 20 y 40',
                                      'Entre 41 y 60','Entre 61 y 80',
                                      'Mayor a 81'))+
          scale_fill_brewer(name="Sexo",palette="Accent",labels=c('1'='M',
                                                                  '2'='F'))+
          theme_bw()
      }
    }
  })
}
shinyApp(ui, server)
