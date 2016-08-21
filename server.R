library(shiny)
library(scales)
server<-function(input, output)
{

  #############################################Page 1#################################################################
  #first provide initial valueBox
  input_group<-data.frame(table(input_data_date$loan_status_agg))
  names(input_group)[1]='status'
  names(input_group)[2]='Freq'
  total_loans<-sum(input_group$Freq)
  total_current<-subset(input_group, status=='Current')$Freq
  total_late<-subset(input_group, status=='Late')$Freq
  total_fully_paid<-subset(input_group, status=='Fully Paid')$Freq
  total_default<-total_loans-total_current-total_late-total_fully_paid
  
  #then provide initial distribution plot
  input_date_group<-data.frame(table(input_data_date$issue_date,input_data_date$loan_status_agg))
  names(input_date_group)[1]='issue_d'
  names(input_date_group)[2]='status'
  names(input_date_group)[3]='Freq'
  
  input_dist_curr<-input_date_group[input_date_group$status=='Current',]
  input_dist_curr$cfreq=cumsum(input_dist_curr$Freq)
  
  input_dist_late<-input_date_group[input_date_group$status=='Late',]
  input_dist_late$cfreq=cumsum(input_dist_late$Freq)
  
  input_dist_fully_paid<-input_date_group[input_date_group$status=='Fully Paid',]
  input_dist_fully_paid$cfreq=cumsum(input_dist_fully_paid$Freq)
  
  input_dist_default<-input_date_group[input_date_group$status=='Default',]
  input_dist_default$cfreq=cumsum(input_dist_default$Freq)
  
  input_dist_fnl<-rbind(input_dist_curr, input_dist_late, input_dist_fully_paid, input_dist_default)
  input_dist_fnl$issue_date<-as.Date(as.character(input_dist_fnl$issue_d), format="%Y-%m-%d")
  cbPalette<-c("#66CC99","#CC6666", "#56B4E9", "#F0E442")
  
  #vintage default destribution
  input_date_group_all<-data.frame(table(input_data_date$issue_date))
  names(input_date_group_all)[1]='issue_d'
  input_dist_default$default<-input_dist_default$Freq
  input_date_group_all$total<-input_date_group_all$Freq
  names(input_dist_default)[1]='issue_d'
  default_vintage<-merge(input_date_group_all, input_dist_default, by.x='issue_d', by.y='issue_d')
  default_vintage$default_rate<-default_vintage$default/default_vintage$total
  default_vintage_sort <- head(default_vintage[order(-default_vintage$default_rate),], 10)
  default_vintage_sort$issue_date<-default_vintage_sort$issue_d;
  default_vintage_sort_fnl<-default_vintage_sort[,c('issue_date','default_rate')]
  
  output$count<-renderValueBox(
    {
      valueBox(value=total_loans, icon = icon("users"), subtitle='Total Loans', color = "blue")
    }
  )
  
  output$current<-renderValueBox(
    {
      valueBox(value=total_current, icon = icon("bell"), subtitle='Total Current', color = "blue")
    }
  )
  
  output$late<-renderValueBox(
    {
      valueBox(value=total_late, icon = icon("exclamation-circle"), subtitle='Total Lates', color = "yellow")
    }
  )
  
  output$default<-renderValueBox(
    {
      valueBox(value=total_fully_paid, icon = icon("thumbs-down"), subtitle='Total Defaults', color = "red")
    }
  )
  
  output$fully_paid<-renderValueBox(
    {
      valueBox(value=total_default, icon = icon("check-circle"), subtitle='Total Fully Paids', color = "green")
    }
  )
  
  output$overall_dist_date<-renderPlot(
    {
      ggplot(input_dist_fnl, aes(x=issue_date, y=cfreq,group=status, colour=status))+geom_line(size=1.5)+xlab("Issue Date")+ylab("Cum Freq")+scale_y_continuous(labels = comma)+scale_colour_manual(values=cbPalette)
    }
  )
  
  output$vintages_dist<-renderPlot(
    {
      ggplot(default_vintage_sort_fnl, aes(x=issue_date, y=default_rate))+geom_bar(colour="black",fill="#DD8888", stat = "identity")+xlab("Issue Date")+ylab("Default Rate")+coord_flip()+geom_text(aes(label = sprintf("%1.2f%%", 100*default_rate)), size = 3)+scale_y_continuous(labels = percent_format())
    }
  )
  
  #############################################Page 2 Uni-Variate#################################################################
  input_data_var<-data.frame(input_data_date['loan_status_agg'], input_data_date[uni_var])
  var_mean<-mean(input_data_var[,uni_var],na.rm = TRUE)
  var_max<-max(input_data_var[,uni_var],na.rm = TRUE)
  var_min<-min(input_data_var[,uni_var],na.rm = TRUE)
  ######univariate plot######
  #default bin is 10
  #first sort by the variable
  input_data_var_sort<-input_data_var[order(input_data_var[,uni_var]),]
  input_data_var_sort$bad_1<-ifelse(input_data_var_sort$loan_status_agg %in% c('Default'),1,0)
  input_data_var_sort$bad_2<-ifelse(input_data_var_sort$loan_status_agg %in% c('Late'),1,0)
  
  ttl_num<-nrow(input_data_var_sort)
  bin<-10
  piece<-ttl_num%/%bin #number of observation per bin
  mean_var<-rep(0,times=bin)
  mean_bad_1<-rep(0,times=bin)
  mean_bad_2<-rep(0,times=bin)
  for (i in 1:bin) # calculate mean and bad for ith bin
  {
    if(i!=bin) 
    {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):(piece*i), uni_var])
     mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):(piece*i)])
     mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):(piece*i)])} 
    else {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):ttl_num, uni_var])
          mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):ttl_num])
          mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):ttl_num])
          }
  }
  
  uni_plot_data<-data.frame(mean_var, mean_bad_1, mean_bad_2)
  ############################################################################
  
  output$uni_mean<-renderValueBox(
    {
      valueBox(value=formatC(var_mean, digits = 3, format = "f"), icon = icon("battery-half"), subtitle='Variable Mean', color = "blue")
    }
  )
  
  output$uni_min<-renderValueBox(
    {
      valueBox(value=var_min, icon = icon("battery-quarter"), subtitle='Variable Min', color = "blue")
    }
  )
  
  output$uni_max<-renderValueBox(
    {
      valueBox(value=var_max, icon = icon("battery-full"), subtitle='Variable Max', color = "blue")
    }
  )
  
  output$uni_histogram<-renderPlot(
    {
      ggplot(input_data_var, aes_string(uni_var))+geom_histogram(colour="black",fill="#DD8888",bins = 10)
    }
  )
  
  output$uni_plot<-renderPlot(
    {
      ggplot(uni_plot_data, aes(x=mean_var, y=mean_bad_1))+geom_line(size=1, colour="blue")+xlab("Mean Var")+ylab("Mean Default")+scale_y_continuous(labels = percent_format())
    }
  )
  
  
  
  ###########################################Page 3 Bi-Variate#####################################################################
  bi_input_data_var<-input_data_date[,c('loan_status_agg', bi_var_1, bi_var_2)]
  bi_input_data_var$bad_1<-ifelse(bi_input_data_var$loan_status_agg %in% c('Default'),1,0)
  bi_input_data_var$bad_2<-ifelse(bi_input_data_var$loan_status_agg %in% c('Late'),1,0)
  bi_input_data_var_sort<-bi_input_data_var[order(bi_input_data_var[,bi_var_1],bi_input_data_var[,bi_var_2]),]
  
  bi_ttl_num<-nrow(bi_input_data_var_sort)
  bi_bin<-10
  total_bin<-bi_bin*bi_bin;
  bi_piece<-bi_ttl_num%/%(total_bin) #number of observation per bin
  bin_var_1<-rep(0,times=total_bin)
  bin_var_2<-rep(0,times=total_bin)
  mean_default<-rep(0,times=total_bin)
  for (i in 1:total_bin) # calculate mean and bad for ith bin
  {
    if(i<total_bin) 
    {bin_var_1[i]<-(i-1)%/%bi_bin+1
     bin_var_2[i]<-(i-1)%%bi_bin+1
    mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):(bi_piece*i)])
    } 
    else {
      bin_var_1[i]<-bi_bin
      bin_var_2[i]<-bi_bin
      mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):bi_ttl_num])
    }
  }
  
  
  bi_plot_data<-data.frame(bin_var_1, bin_var_2, mean_default)
  
  output$bi_heat<-renderPlot(
    {
      ggplot(bi_plot_data, aes(bin_var_1, bin_var_2))+geom_raster(aes(fill = mean_default))+
      scale_fill_gradient2(low="lightblue", high="darkblue", guide="colorbar", labels = percent)+
      scale_x_continuous(breaks =seq(1, 10, by = 1))+scale_y_continuous(breaks = seq(1, 10, by = 1))+xlab(bi_var_1)+ylab(bi_var_2)
    }
  )

  #################################################################################################################################
  #then provide plots after change date range updates
  #################################################################################################################################
  observeEvent(input$date_update, 
    {
    
    if ((input$date_range[1]!=global_start)|(input$date_range[2]!=global_end))
    {
    
    input_data_date<-subset(input_data_date, issue_date>=input$date_range[1]&issue_date<=input$date_range[2])
    #first provide initial valueBox
    input_group<-data.frame(table(input_data_date$loan_status_agg))
    names(input_group)[1]='status'
    names(input_group)[2]='Freq'
    total_loans<-sum(input_group$Freq)
    total_current<-subset(input_group, status=='Current')$Freq
    total_late<-subset(input_group, status=='Late')$Freq
    total_fully_paid<-subset(input_group, status=='Fully Paid')$Freq
    total_default<-total_loans-total_current-total_late-total_fully_paid
    
    #then provide initial distribution plot
    input_date_group<-data.frame(table(input_data_date$issue_date,input_data_date$loan_status_agg))
    names(input_date_group)[1]='issue_d'
    names(input_date_group)[2]='status'
    names(input_date_group)[3]='Freq'
    input_dist_curr<-input_date_group[input_date_group$status=='Current',]
    input_dist_curr$cfreq=cumsum(input_dist_curr$Freq)
    
    input_dist_late<-input_date_group[input_date_group$status=='Late',]
    input_dist_late$cfreq=cumsum(input_dist_late$Freq)
    
    input_dist_fully_paid<-input_date_group[input_date_group$status=='Fully Paid',]
    input_dist_fully_paid$cfreq=cumsum(input_dist_fully_paid$Freq)
    
    input_dist_default<-input_date_group[input_date_group$status=='Default',]
    input_dist_default$cfreq=cumsum(input_dist_default$Freq)
    
    input_dist_fnl<-rbind(input_dist_curr, input_dist_late, input_dist_fully_paid, input_dist_default)
    input_dist_fnl$issue_date<-as.Date(as.character(input_dist_fnl$issue_d), format="%Y-%m-%d")
    cbPalette<-c("#66CC99","#CC6666", "#56B4E9", "#F0E442")
    
    #vintage default destribution
    input_date_group_all<-data.frame(table(input_data_date$issue_date))
    names(input_date_group_all)[1]='issue_d'
    input_dist_default$default<-input_dist_default$Freq
    input_date_group_all$total<-input_date_group_all$Freq
    names(input_dist_default)[1]='issue_d'
    default_vintage<-merge(input_date_group_all, input_dist_default, by.x='issue_d', by.y='issue_d')
    default_vintage$default_rate<-default_vintage$default/default_vintage$total
    default_vintage_sort <- head(default_vintage[order(-default_vintage$default_rate),], 10)
    default_vintage_sort$issue_date<-default_vintage_sort$issue_d;
    default_vintage_sort_fnl<-default_vintage_sort[,c('issue_date','default_rate')]
    
    output$count<-renderValueBox(
      {
        valueBox(value=total_loans, icon = icon("users"), subtitle='Total Loans', color = "blue")
      }
    )
    
    output$current<-renderValueBox(
      {
        valueBox(value=total_current, icon = icon("bell"), subtitle='Total Current', color = "blue")
      }
    )
    
    output$late<-renderValueBox(
      {
        valueBox(value=total_late, icon = icon("exclamation-circle"), subtitle='Total Lates', color = "yellow")
      }
    )
    
    output$default<-renderValueBox(
      {
        valueBox(value=total_fully_paid, icon = icon("thumbs-down"), subtitle='Total Defaults', color = "red")
      }
    )
    
    output$fully_paid<-renderValueBox(
      {
        valueBox(value=total_default, icon = icon("check-circle"), subtitle='Total Fully Paids', color = "green")
      }
    )
    
    output$overall_dist_date<-renderPlot(
      {
        ggplot(input_dist_fnl, aes(x=issue_date, y=cfreq,group=status, colour=status))+geom_line(size=1.5)+xlab("Issue Date")+ylab("Cum Freq")+scale_y_continuous(labels = comma)+scale_colour_manual(values=cbPalette)
      }
    )
    
    output$vintages_dist<-renderPlot(
      {
        ggplot(default_vintage_sort_fnl, aes(x=issue_date, y=default_rate))+geom_bar(colour="black",fill="#DD8888", stat = "identity")+xlab("Issue Date")+ylab("Default Rate")+coord_flip()+geom_text(aes(label = sprintf("%1.2f%%", 100*default_rate)), size = 3)+scale_y_continuous(labels = percent_format())
      }
    )
    
    
    #############################################Page 2 Uni-Variate#################################################################
    input_data_var<-data.frame(input_data_date['loan_status_agg'], input_data_date[uni_var])
    var_mean<-mean(input_data_var[,uni_var], na.rm = TRUE)
    var_max<-max(input_data_var[,uni_var], na.rm = TRUE)
    var_min<-min(input_data_var[,uni_var], na.rm = TRUE)
    ######univariate plot######
    #default bin is 10
    #first sort by the variable
    input_data_var_sort<-input_data_var[order(input_data_var[,uni_var]),]
    input_data_var_sort$bad_1<-ifelse(input_data_var_sort$loan_status_agg %in% c('Default'),1,0)
    input_data_var_sort$bad_2<-ifelse(input_data_var_sort$loan_status_agg %in% c('Late'),1,0)
    
    ttl_num<-nrow(input_data_var_sort)
    bin<-10
    piece<-ttl_num%/%bin #number of observation per bin
    mean_var<-rep(0,times=bin)
    mean_bad_1<-rep(0,times=bin)
    mean_bad_2<-rep(0,times=bin)
    for (i in 1:bin) # calculate mean and bad for ith bin
    {
      if(i!=bin) 
      {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):(piece*i), uni_var])
      mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):(piece*i)])
      mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):(piece*i)])} 
      else {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):ttl_num, uni_var])
      mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):ttl_num])
      mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):ttl_num])
      }
    }
    
    uni_plot_data<-data.frame(mean_var, mean_bad_1, mean_bad_2)
    ############################################################################
    
    output$uni_mean<-renderValueBox(
      {
        valueBox(value=formatC(var_mean, digits = 3, format = "f"), icon = icon("battery-half"), subtitle='Variable Mean', color = "blue")
      }
    )
    
    output$uni_min<-renderValueBox(
      {
        valueBox(value=var_min, icon = icon("battery-quarter"), subtitle='Variable Min', color = "blue")
      }
    )
    
    output$uni_max<-renderValueBox(
      {
        valueBox(value=var_max, icon = icon("battery-full"), subtitle='Variable Max', color = "blue")
      }
    )
    
    output$uni_histogram<-renderPlot(
      {
        ggplot(input_data_var, aes_string(uni_var))+geom_histogram(colour="black",fill="#DD8888",bins = 10)
      }
    )
    
    output$uni_plot<-renderPlot(
      {
        ggplot(uni_plot_data, aes(x=mean_var, y=mean_bad_1))+geom_line(size=1, colour="blue")+xlab("Mean Var")+ylab("Mean Default")+scale_y_continuous(labels = percent_format())
      }
    )
    
    
    
    ###########################################Page 3 Bi-Variate#####################################################################
    bi_input_data_var<-input_data_date[,c('loan_status_agg', bi_var_1, bi_var_2)]
    bi_input_data_var$bad_1<-ifelse(bi_input_data_var$loan_status_agg %in% c('Default'),1,0)
    bi_input_data_var$bad_2<-ifelse(bi_input_data_var$loan_status_agg %in% c('Late'),1,0)
    bi_input_data_var_sort<-bi_input_data_var[order(bi_input_data_var[,bi_var_1],bi_input_data_var[,bi_var_2]),]
    
    bi_ttl_num<-nrow(bi_input_data_var_sort)
    bi_bin<-10
    total_bin<-bi_bin*bi_bin;
    bi_piece<-bi_ttl_num%/%(total_bin) #number of observation per bin
    bin_var_1<-rep(0,times=total_bin)
    bin_var_2<-rep(0,times=total_bin)
    mean_default<-rep(0,times=total_bin)
    for (i in 1:total_bin) # calculate mean and bad for ith bin
    {
      if(i<total_bin) 
      {bin_var_1[i]<-(i-1)%/%bi_bin+1
      bin_var_2[i]<-(i-1)%%bi_bin+1
      mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):(bi_piece*i)])
      } 
      else {
        bin_var_1[i]<-bi_bin
        bin_var_2[i]<-bi_bin
        mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):bi_ttl_num])
      }
    }
    
    
    bi_plot_data<-data.frame(bin_var_1, bin_var_2, mean_default)
    
    output$bi_heat<-renderPlot(
      {
        ggplot(bi_plot_data, aes(bin_var_1, bin_var_2))+geom_raster(aes(fill = mean_default))+
          scale_fill_gradient2(low="lightblue", high="darkblue", guide="colorbar", labels = percent)+
          scale_x_continuous(breaks =seq(1, 10, by = 1))+scale_y_continuous(breaks = seq(1, 10, by = 1))+xlab(bi_var_1)+ylab(bi_var_2)
      }
    )
    
    global_start<-input$date_range[1]
    global_end<-input$date_range[2]
  }
    })
  
  
  ###############################update the univariate chart only########################
  observeEvent(input$uni_update, 
    {
      if(input$uni_sel!=uni_var)
      {
        #############################################Page 2 Uni-Variate#################################################################
        uni_var<-input$uni_sel
        input_data_var<-data.frame(input_data_date['loan_status_agg'], input_data_date[uni_var])
        var_mean<-mean(input_data_var[,uni_var],na.rm = TRUE)
        var_max<-max(input_data_var[,uni_var],na.rm = TRUE)
        var_min<-min(input_data_var[,uni_var],na.rm = TRUE)
        ######univariate plot######
        #default bin is 10
        #first sort by the variable
        input_data_var_sort<-input_data_var[order(input_data_var[,uni_var]),]
        input_data_var_sort$bad_1<-ifelse(input_data_var_sort$loan_status_agg %in% c('Default'),1,0)
        input_data_var_sort$bad_2<-ifelse(input_data_var_sort$loan_status_agg %in% c('Late'),1,0)
        
        ttl_num<-nrow(input_data_var_sort)
        bin<-10
        piece<-ttl_num%/%bin #number of observation per bin
        mean_var<-rep(0,times=bin)
        mean_bad_1<-rep(0,times=bin)
        mean_bad_2<-rep(0,times=bin)
        for (i in 1:bin) # calculate mean and bad for ith bin
        {
          if(i!=bin) 
          {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):(piece*i), uni_var])
          mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):(piece*i)])
          mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):(piece*i)])} 
          else {mean_var[i]<-mean(input_data_var_sort[((i-1)*piece+1):ttl_num, uni_var])
          mean_bad_1[i]<-mean(input_data_var_sort$bad_1[((i-1)*piece+1):ttl_num])
          mean_bad_2[i]<-mean(input_data_var_sort$bad_2[((i-1)*piece+1):ttl_num])
          }
        }
        
        uni_plot_data<-data.frame(mean_var, mean_bad_1, mean_bad_2)
        ############################################################################
        
        output$uni_mean<-renderValueBox(
          {
            valueBox(value=formatC(var_mean, digits = 3, format = "f"), icon = icon("battery-half"), subtitle='Variable Mean', color = "blue")
          }
        )
        
        output$uni_min<-renderValueBox(
          {
            valueBox(value=var_min, icon = icon("battery-quarter"), subtitle='Variable Min', color = "blue")
          }
        )
        
        output$uni_max<-renderValueBox(
          {
            valueBox(value=var_max, icon = icon("battery-full"), subtitle='Variable Max', color = "blue")
          }
        )
        
        output$uni_histogram<-renderPlot(
          {
            ggplot(input_data_var, aes_string(uni_var))+geom_histogram(colour="black",fill="#DD8888",bins = 10)
          }
        )
        
        output$uni_plot<-renderPlot(
          {
            ggplot(uni_plot_data, aes(x=mean_var, y=mean_bad_1))+geom_line(size=1, colour="blue")+xlab("Mean Var")+ylab("Mean Default")+scale_y_continuous(labels = percent_format())
          }
        )
      }
    }
  )
  
  ##########################update the bi-variate chart#################
  observeEvent(input$bi_update, 
              {
                if((input$bi_sel_1!=bi_var_1)|(input$bi_sel_2!=bi_var_2))
                {
                  bi_var_1<-input$bi_sel_1
                  bi_var_2<-input$bi_sel_2
                  bi_input_data_var<-input_data_date[,c('loan_status_agg', bi_var_1, bi_var_2)]
                  bi_input_data_var$bad_1<-ifelse(bi_input_data_var$loan_status_agg %in% c('Default'),1,0)
                  bi_input_data_var$bad_2<-ifelse(bi_input_data_var$loan_status_agg %in% c('Late'),1,0)
                  bi_input_data_var_sort<-bi_input_data_var[order(bi_input_data_var[,bi_var_1],bi_input_data_var[,bi_var_2]),]
                  
                  bi_ttl_num<-nrow(bi_input_data_var_sort)
                  bi_bin<-10
                  total_bin<-bi_bin*bi_bin;
                  bi_piece<-bi_ttl_num%/%(total_bin) #number of observation per bin
                  bin_var_1<-rep(0,times=total_bin)
                  bin_var_2<-rep(0,times=total_bin)
                  mean_default<-rep(0,times=total_bin)
                  for (i in 1:total_bin) # calculate mean and bad for ith bin
                  {
                    if(i<total_bin) 
                    {bin_var_1[i]<-(i-1)%/%bi_bin+1
                    bin_var_2[i]<-(i-1)%%bi_bin+1
                    mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):(bi_piece*i)])
                    } 
                    else {
                      bin_var_1[i]<-bi_bin
                      bin_var_2[i]<-bi_bin
                      mean_default[i]<-mean(bi_input_data_var_sort$bad_1[((i-1)*bi_piece+1):bi_ttl_num])
                    }
                  }
                  
                  
                  bi_plot_data<-data.frame(bin_var_1, bin_var_2, mean_default)
                  
                  output$bi_heat<-renderPlot(
                    {
                      ggplot(bi_plot_data, aes(bin_var_1, bin_var_2))+geom_raster(aes(fill = mean_default))+
                        scale_fill_gradient2(low="lightblue", high="darkblue", guide="colorbar", labels = percent)+
                        scale_x_continuous(breaks =seq(1, 10, by = 1))+scale_y_continuous(breaks = seq(1, 10, by = 1))+xlab(bi_var_1)+ylab(bi_var_2)
                    }
                  )
                  
                }
                
              }
  )
  
}
