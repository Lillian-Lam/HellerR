#' Creates a customizable PRISM-like bar graph. Currently, error bars and significant bars only work if you want ggplot to graph a stat summary. Imports: ggplot2, ggsignif, ggprism
#'
#' @param df The data frame placed into ggplot.
#' @param x_variable The independent variable. The bar labels. Written as a string name of a column.
#' @param y_variable The dependent variable. The measured values. Written as a string name of a column
#' @param group_order A vector. The exact order that the groups/bars are wanted.
#' @param group_label_size The font size of the x variable labels. Default is 5.
#' @param statistics A string or function. The statistics placed in stat_summary. Default is mean.
#' @param control A string. The x variable that is the control of the df. Is used in fold change calculations.
#' @param bar_width Integer or float. The width of the bars. Default is 0.4.
#' @param title The title of the graph. Written as a string.
#' @param title_position Edits the margins around the title. Enter as margin\(top, right, bottom, left, unit\). Example: margin\(0,0,0,0, "cm"\).
#' @param x_label Adds a label under the x-axis. Written as a string.
#' @param y_label Adds a label next to the y-axis. Written as a string.
#' @param x_label_angle Integer or float. Angle of the bar group labels. Default is 0.
#' @param minor_ticks Boolean value. Adds minor ticks.
#' @param bar_colors Changes the bar colors. Write as a vector with hex color strings.
#' @param individual_points Boolean value. Adds jitter points.
#' @param point_size Integer or float. Changes the size of the individual points. Default is 3.
#' @param point_shapes A numeric vector. Changes the shape of the individual points.
#' @param point_color Changes the color of the individual points. Write as a vector with hex color strings.
#' @param error_bars Boolean value. Adds error bars to the graph.
#' @param error_bar_width Integer or float. Changes the width of the error bars. Default is 0.2.
#' @param error_bar_method A string or function. What the error bars plot. Default is mean of standard error.
#' @param signif_bars Boolean value. Adds significant bars.
#' @param signif_test A string. Is the type of statistical significance test. Default is t.test.
#' @param comparision_groups A list argument of what vector pairs to conduct a t-tests on. Example: list\(c\(x1, x2\), c\(x2, x3\)\).
#' @param signif_y_position A numeric vector. Changes the specific y-position of the significant bars.
#' @param signif_step_increase A numeric vector.Minimizes overlap by increasing in fraction of total height for every bar.
#' @param margins Edits the margins around the graph. Enter as margin\(top, right, bottom, left, unit\). Example: margin\(0,0,0,0, "cm"\).
#' @param y_axis_limits Changes the limits of the y-axis. Write as c(min, max).
#' @return A PRISM-themed ggplot graph.
#' @import ggplot2, ggsignif, ggprism
heller_graph<- function(df,
                        x_variable= "",
                        y_variable= "",
                        group_order=c(),
                        group_label_size=5,
                        statistics=mean,
                        control=NULL,
                        bar_width=0.4,
                        title=NULL,
                        title_position=NULL,
                        x_label=NULL,
                        y_label=NULL,
                        x_label_angle=0,
                        minor_ticks=TRUE,
                        bar_colors=c(),
                        individual_points=TRUE,
                        point_size=3,
                        point_shapes=c(),
                        point_colors=c(),
                        error_bars=TRUE,
                        error_bar_width=0.2,
                        error_bar_method=mean_se,
                        signif_bars=TRUE,
                        signif_test="t.test",
                        comparision_groups=c(),
                        signif_y_position=NULL,
                        signif_step_increase=0.2,
                        margins=NULL,
                        y_axis_limits=NULL
){
  # Order the bar groups on x-axis by creating levels
  if(length(group_order)==0){
    group_order <- c(unique(df[[x_variable]]))
  }

  df[[x_variable]] <-factor(df[[x_variable]], levels = group_order)
  # Makes the default color of the bars white
  if(length(bar_colors)==0){
    bar_colors<-c(rep("white", length(unique(df[[x_variable]]))))
  }
  #The closure fold-change function to enter into stat_summary
  if(is.character(statistics)){
    if (grepl("fold_change", statistics)){
      reference_mean <- mean(df[which(df[[x_variable]]== control), ][[y_variable]])
      statistics <- function(x) {
        return(mean(x)/reference_mean)
      }
    }
  }

  #Uses stat_summary to find the statisitcs and make the bars on the plot
  plot<-ggplot(df, aes(x=.data[[x_variable]], y=.data[[y_variable]]))+
    stat_summary(fun=statistics,
                 geom= "bar",
                 width= bar_width,
                 fill= bar_colors,
                 colour="black",
                 linewidth=1.5)
  #Creates the error bars
  if(error_bars){
    plot<-plot+stat_summary(fun.data=error_bar_method,
                            geom= "errorbar",
                            fun.args=list(mult=1),
                            width=error_bar_width,
                            linewidth=1)
  }
  #Gives the graph a title if user enters a title
  if(!is.null(title)){
    plot<-plot+ggtitle(title)
  }

  else{
    plot<-plot+ggtitle(NULL)
  }
  #Gives the graph a x-axis label if user enters a label
  if(!is.null(x_label)){
    plot<-plot+xlab(x_label)
  }

  else{
    plot<-plot+xlab(NULL)
  }
  #Gives the graph a y-axis label if user enters a label
  if(!is.null(y_label)){
    plot<-plot+ylab(y_label)
  }

  else{
    plot<-plot+ylab(NULL)
  }
  #Makes the point shapes 21 if the user does not enter any
  if(length(point_shapes)==0){
    point_shapes <- c(rep(21, length(unique(df[[x_variable]]))))
  }
  #Makes the point colors if the user did not enter any
  if(length(point_colors)==0){
    point_colors <- c(rep("grey",length(unique(df[[x_variable]]))))
  }

  #Adds the jitter points to the graph if the user wants them
  if(individual_points){
    plot<-plot+geom_jitter(aes(shape=.data[[x_variable]], fill=.data[[x_variable]]),
                           color="black",
                           width=0.1,
                           size=point_size,
                           stroke=1.5 )+
      scale_shape_manual(values=point_shapes)+
      scale_fill_manual(values=point_colors)
  }
  #Creates the PRISM-like graph theme
  plot<-plot+theme_prism()+
    #Changes the angle of the bar labels to what the user wants it to be
    theme(axis.text.x=element_text(angle=x_label_angle, hjust=1, vjust=1),legend.position="none")+
    #Adds space between the data and axes
    scale_y_continuous(expand= expansion(mult= c(0,0)))+
    #Changes the font size of the labels
    theme(axis.text.x=element_text(size=group_label_size))
  #Sets the y-axis limits


  #Limits are different depending on if the individual points are graphed or not, since they can be way higher than the statistics
  if (individual_points){
    if(max(df[[y_variable]], na.rm=TRUE)<1){
      #Function for the scaling factor
      scale= 10^(ceiling(1- log(max(df[[y_variable]], na.rm=TRUE),  base = 10)))
      #Creates a line at y=0 and bolds it if points go below 0
      if(min(df[[y_variable]], na.rm=TRUE)<0){
        plot<-plot+coord_cartesian(clip="off", ylim=c(min(df[[y_variable]], na.rm=TRUE),    ceiling(max(df[[y_variable]], na.rm=TRUE)*scale)/scale))
        plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)
      }
      else{
        plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling(max(df[[y_variable]], na.rm=TRUE)*scale)/scale))
      }
    }
    else{
      if(min(df[[y_variable]], na.rm=TRUE)<0){
          plot<-plot+coord_cartesian(clip="off", ylim=c(min(df[[y_variable]], na.rm=TRUE),    ceiling(max(df[[y_variable]], na.rm=TRUE))))
          plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)
        }
      else{
          plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling(max(df[[y_variable]], na.rm=TRUE))))
        }
      }
    }

  #Not individual points
  else{
    # Adds the height of the error bars in the y limits
    if (error_bars){
      error_bar_data <-ggplot_build(plot)$data[[2]]
      if (all(is.na(error_bar_data[["ymax"]]))){
        error_bar_data<-ggplot_build(plot)$data[[1]]
      }
      if(max(error_bar_data[["ymax"]], na.rm=TRUE) <1){
        scale= 10^(ceiling(1- log(max(error_bar_data[["ymax"]], na.rm=TRUE),  base = 10)))
        if(min(error_bar_data[["ymin"]], na.rm=TRUE)<0){
          plot<-plot+coord_cartesian(clip="off", ylim=c(floor(min(error_bar_data[["ymax"]], na.rm=TRUE))*scale)/scale,ceiling(max(error_bar_data[["ymax"]], na.rm=TRUE)*scale)/scale)
          plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)

        }

        else{
          plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling((max(error_bar_data[["ymax"]], na.rm=TRUE)*scale)/scale)))
        }
      }

      else{
        if(min(error_bar_data[["ymax"]], na.rm=TRUE)<0){
          plot<-plot+coord_cartesian(clip="off", ylim=c(floor(min(error_bar_data[["ymax"]], na.rm=TRUE)), ceiling(max(error_bar_data[["ymax"]], na.rm=TRUE))))
          plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)
        }
        else{
          plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling(max(error_bar_data[["ymax"]], na.rm=TRUE))))
        }
      }
    }
    else{
      col_data<-ggplot_build(plot)$data[[1]]
      scale= 10^(ceiling(1- log(max(col_data[["ymax"]], na.rm=TRUE),  base = 10)))
      if(max(col_data[["ymax"]], na.rm=TRUE) <1){
        if(min(col_data[["ymin"]], na.rm=TRUE)<0){
          plot<-plot+coord_cartesian(clip="off", ylim=c(floor(min(col_data[["ymax"]], na.rm=TRUE))*scale)/scale,ceiling(max(col_data[["ymax"]], na.rm=TRUE)*scale)/scale)
          plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)

        }

        else{
          plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling((max(col_data[["ymax"]], na.rm=TRUE)*scale)/scale)))
        }
      }

      else{
        if(min(col_data[["ymin"]], na.rm=TRUE)<0){
          plot<-plot+coord_cartesian(clip="off", ylim=c(floor(min(col_data[["ymax"]], na.rm=TRUE)), ceiling(max(col_data[["ymax"]], na.rm=TRUE))))
          plot<-plot+geom_hline(yintercept =0, linetype="solid", color="black", size=1)
        }
        else{
          plot<-plot+coord_cartesian(clip="off", ylim=c(0, ceiling(max(col_data[["ymax"]], na.rm=TRUE))))
        }
      }
    }
  }

  #Adds minor ticks to the the graph
  if(minor_ticks){
    plot<-plot+guides(y=guide_axis(minor.ticks=TRUE))
  }

  #Adds significant bars
  if(signif_bars){
    #Adds the significant bars for all groups in a specific order (see all_group_pairs documentation)
    if(length(comparision_groups)==0){
      comparision_groups<-all_group_pairs(df[[x_variable]])
      #Adds space for the significant bars by moving the title up. Assumes that significant bars will not exceed 5 cm.
      if(is.null(title_position)){
        plot<-plot+theme(plot.title= element_text(margin=margin(0,0,5,0,"cm")))
      }
    }
    #Increases the step of each bars if user does not specify y position so that bars are not on top of each other
    if(is.null(signif_y_position)){
      plot<-plot+geom_signif(comparisons= comparision_groups,
                             step_increase = signif_step_increase,
                             test=signif_test,
                             family="Sans",
                             map_signif_level=c("*/**"= 0.001, "**"=0.01, "*"=0.05, "ns"=1))
    }
    else{
      plot<-plot+geom_signif(comparisons= comparision_groups,
                             y_position=signif_y_position,
                             test=signif_test,
                             family="Sans",
                             map_signif_level=c("*/**"= 0.001, "**"=0.01, "*"=0.05, "ns"=1))
    }
  }
  #Changes the title position
  if(!is.null(title_position)){
    plot<-plot+theme(plot.title= element_text(margin=title_position))
  }
  #Changes the plot margins
  if(!is.null(margins)){
    plot<-plot+theme(plot.margin=margins)
  }
  #Changes the y-limits of the graph if user does not like the rounding
  if(!is.null(y_axis_limits)){
    if (min(y_axis_limits)<0){
      plot<- plot+coord_cartesian(ylim= y_axis_limits)+geom_hline(yintercept =0, linetype="solid", color="black", size=1)
    }
    else{
      plot<-plot+coord_cartesian(ylim= y_axis_limits)
    }
  }

  return(plot)

}
