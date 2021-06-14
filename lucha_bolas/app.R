
library(ggplot2)
library(shiny)
library(dplyr)

min_angle <- 25
max_angle <- 90

min_strength <- 5
max_strength <- 15

min_x_canvas <- 0
max_x_canvas <- 100

min_y_canvas <- 0
max_y_canvas <- 100

gravity <- -2
radius<-10
N<- 45
t_max <- 20


TURN_USER <- "Usuario"
TURN_IA <- "IA"

# Definimos el objeto UI. 
# La función fluidPage() crea una estructura HTML que automáticamente se ajusta 
# a las dimensiones del navegador. Dentro de esta función se definen los elementos
# que se mostrarán en la web.
ui <- fluidPage(
  

  # Crea un diseño con una barra lateral (sidebarPanel) y un panel principal (mainPanel)
  sidebarLayout(
    
    # Barra lateral con texto:
    sidebarPanel(
      #Slider para seleccionar el angulo
      sliderInput(
        "angle", label = "Angulo de disparo:",
        min = min_angle, value = mean(c(min_angle,max_angle)) , max = max_angle, step=1, sep=''
      ),
      sliderInput(
        "strength", label = "Fuerza de disparo:",
        min = min_strength, value = mean(min_strength,max_strength) , max = max_strength, step=1, sep=''
      ),
      textOutput("txt_out"),
      actionButton("shot", "Dispara")
    ),
    # Panel principal
    mainPanel(
      # Output: Gráfico que dibujamos
      plotOutput(outputId = "game_plot")
      
    )
  )
)

calc_initial_positions<-function(min_x, max_x, y_val){
  ball1<-c(x=runif(1,min_x, ((max_x-min_x)/2+min_x ))*0.8,y=0)
  ball2<-c(x=runif(1,((max_x-min_x)/2+min_x)*1.2,max_x ),y=0)
  list(ball1=ball1,ball2=ball2)
}

balls_pos <- calc_initial_positions(min_x_canvas, max_x_canvas, y_val=0)

plot_canvas <- function(min_x_canvas,max_x_canvas,min_y_canvas,max_y_canvas){
  df_canvas<-data.frame(x=seq(from=min_x_canvas,
                              to=max_y_canvas,
                              length.out=N),
                        y=seq(from=min_y_canvas,
                              to=max_y_canvas,
                              length.out=N)
                        )
  ggplot(df_canvas,
         aes(x=x,y=y))+geom_line(y=min_y_canvas)+geom_line(alpha=0)+coord_fixed()
}
plot_ball <- function(g,ball_pos,angle, color='red'){
  g+geom_point(x=ball_pos["x"],y=ball_pos["y"],size=radius,color=color)+
    geom_segment(x=ball_pos["x"],y=ball_pos["y"],
                 xend=ball_pos["x"]+radius*cos(angle),yend=ball_pos["y"]+radius*sin(angle),color=color, size=2)
}

plot_shot <- function(g,df_shot){
  g+geom_point(x=df_shot$x,y=df_shot$y, alpha=0.5)
}

calc_shot <- function(ball_pos, angle, strength){
  
  vx0<-cos(angle)*strength
  vy0<-sin(angle)*strength
  
  t<-seq(0,t_max,length.out=N)
  data.frame(t=t,
             x=ball_pos["x"]+radius*cos(angle)+vx0*t,
             y=ball_pos["y"]+radius*sin(angle)+vy0*t+0.5*gravity*t^2)
  
}

target_destroyed <- function(calc_shot, ball_pos){
  dist<-min(sqrt((calc_shot$x-ball_pos["x"])^2+(calc_shot$y-ball_pos["y"])^2))
  print(dist)
  return(dist <= radius/2)
}

# Función que controla el código. Cada vez que cambia alguna entrada, 
# es decir, cualquier valor de `input`, se ejecuta esta función.
# También ocurre al inicio de la aplicación.
server <- function(input, output, session) {
  #df_shot<-calc_shot(balls_pos[[1]],0,0)
  num_shots <<- 0
  turn <- TURN_USER
  angle_rads2 <- (180-runif(1,min_angle,max_angle))*pi/180
  
  output$game_plot <- renderPlot({
    print("plot")
    angle_rads <- pi/180*input$angle
    g<-plot_canvas(min_x_canvas,max_y_canvas,min_y_canvas,max_y_canvas) 
    txt_end <- NULL
    color1 <- 'green'
    color2 <- 'green'
    if (input$shot>num_shots){
      num_shots<<- as.numeric(input$shot)
      print(paste("Has disparado", num_shots,"veces"))
      if (turn == TURN_USER){
        turn <<- TURN_IA
        df_shot<-calc_shot(balls_pos[[1]],angle_rads,input$strength)
        if (target_destroyed(df_shot, balls_pos[[2]])){
          txt_end <- "Has dado en el blanco!"
          color2<-'red'
        }
      }else{
        turn <<- TURN_USER
        angle_rads2 <- (180-runif(1,min_angle,max_angle))*pi/180
        strenght <- runif(1,min = min_strength, max_strength)
        df_shot<-calc_shot(balls_pos[[2]],angle_rads2,strenght)
        if (target_destroyed(df_shot, balls_pos[[1]])){
          txt_end <- "Te han dado, skynet se despierta!"
          color1<-'red'
        }
      }
      output$txt_out<-renderText({paste("Turno ",turn)})  
      
      g <- g %>% plot_shot(df_shot) 
    }
    
    g <- g %>% plot_ball(balls_pos[[1]],angle_rads,color1) %>% 
               plot_ball(balls_pos[[2]],angle_rads2,color2) 
    
    if (!is.null(txt_end)){
      balls_pos <<- calc_initial_positions(min_x_canvas, max_x_canvas, y_val=0)
      output$txt_out<-renderText({txt_end})  
    }
    return(g)
  })
  
  
  
}

shinyApp(ui = ui, server = server)

