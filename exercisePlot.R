exercisePlot <- function(df, nec, startDate, endDate, priorityCheck, countCheck){
  
  # if all is selected don't filter, otherwise do
  dfSub <- if('All' %in% nec){
    df
  }else{
    df %>% filter(asunit %in% nec)
  }
  # filter by date
  dfSub <- 
    dfSub %>%
    filter(reportdate>=startDate & closedate<=endDate)
  # create a new variable to combine NEC and priority
  dfSub <- dfSub %>% 
    mutate(NECpriority = paste(asunit, priority, sep=':')) %>% 
    mutate(NECpriority = factor(NECpriority,
                                levels=paste(rep(nec, each=4),
                                             c('Critical','High','Medium','Low'),
                                             sep=':')))
  # initiate the plot
  p <- ggplot(dfSub)
  # if priority breakdown selected create plot and data table
  # here you can also put all of your chart display options
  if(priorityCheck){
    p <- p +
      geom_boxplot(aes(x=NECpriority, y=daystoclose)) +
      labs(title='Boxplot of Days to Close a Ticket',
           x='NEC:Priority',
           y='Days') +
      theme(axis.text.x = element_text(angle=90))
    dataTab <- dfSub %>%
      group_by(Unit=asunit, Priority=priority) %>%
      summarize(Obs=n(),
                Mean=mean(daystoclose, na.rm=T),
                Median=median(daystoclose, na.rm=T),
                SD=sd(daystoclose, na.rm=T))
    # if the counts is checked add a layer to the plot
    if(countCheck){
      p <- p +
        geom_text(data=dfSub %>%
                    group_by(NECpriority) %>% 
                    summarize(count=n()),
                  aes(x=NECpriority, y=-5, label=count))
    }
    # priority not checked, create plot and data table
  }else{
    p <- p +
      geom_boxplot(aes(x=asunit, y=daystoclose)) + 
      labs(title='Boxplot of Days to Close a Ticket',
           x='NEC',
           y='Days') +
      theme(axis.text.x = element_text(angle=45,
                                       hjust=0,
                                       vjust=0))
    dataTab <- dfSub %>%
      group_by(Unit=asunit) %>%
      summarize(Tickets=n(),
                Mean=mean(daystoclose, na.rm=T),
                Median=median(daystoclose, na.rm=T),
                SD=sd(daystoclose, na.rm=T))
    # add count layer if checked
    if(countCheck){
      p <- p +
        geom_text(data=dfSub %>% group_by(asunit) %>% summarize(count=n()),
                  aes(x=asunit, y=-5, label=count))
    }
  }
  # return both objects inside a list
  return(list(plot=p, tab=dataTab))
}
