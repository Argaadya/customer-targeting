library(plotly)

ax1 <- get("education",df)
ax2 <- df$age
ax3 <- df$gender
ax4 <- df$flag
df_sankey <- data.frame(ax1 = ax1,
                        ax2 = ax2,
                        ax3 = ax3,
                        ax4 = ax4)

ax12 <- df_sankey %>% 
  group_by(ax1, ax2) %>% 
  summarise(total = n()) %>% 
  ungroup()

ax23 <- df_sankey %>% 
  group_by(ax2, ax3) %>% 
  summarise(total = n()) %>% 
  ungroup()

ax34 <- df_sankey %>% 
  group_by(ax3, ax4) %>% 
  summarise(total = n()) %>% 
  ungroup()

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(levels(ax1), 
              levels(ax2),
              levels(ax3),
              levels(ax4)),
    color = c(rep("blue",n_distinct(ax1)), rep("red", n_distinct(ax2)),
              rep("green", n_distinct(ax3)), rep("yellow", n_distinct(ax4))),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(seq_rep(ax1,ax2)-1, 
               seq_rep(ax2,ax3)-1+n_distinct(ax1),
               seq_rep(ax3,ax4)-1+n_distinct(ax1)+n_distinct(ax2)),
    target = c(rep(1:n_distinct(ax2)+n_distinct(ax1)-1, n_distinct(ax1)),
               rep(1:n_distinct(ax3)+n_distinct(ax1)+n_distinct(ax2)-1, n_distinct(ax2)),
               rep(1:n_distinct(ax4)+n_distinct(ax1)+n_distinct(ax2)+n_distinct(ax3)-1, 
                   n_distinct(ax3))),
    value =  c(ax12$total, ax23$total, ax34$total)
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )
p


education_to_age

rep(1:n_distinct(ax1),4)

seq_rep <- function(x,y){
  lol <- 1:n_distinct(x)
  vec <- numeric()
  for (i in 1:n_distinct(x)) {
    vec <- c(vec,rep(lol[i],n_distinct(y)))
  }
  return(vec)
}

seq_rep(ax1,ax2)
levels(ax1)
