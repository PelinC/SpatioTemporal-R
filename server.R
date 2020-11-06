#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize=150*1024^2)
install.packages('DT')
install.packages('lubridate')
install.packages('tcltk')
install.packages('leaflet')
install.packages('ggthemes')
install.packages('RColorBrewer')
install.packages('spdep')
install.packages('shiny')
install.packages('data.table')
install.packages('plotly')
install.packages('scales')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('reshape2')
install.packages('maptools')
install.packages('corrplot')


shinyServer(function(input, output) {
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
      
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })

    

  
  output$choose_columns <- renderUI( {
    # Get the data set with the appropriate name
    colnames <- names(Dataset())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,selected=TRUE)
  })
  
  output$data_table <- DT::renderDataTable({
    A <- Dataset()
    
    DT::datatable(A[, input$columns, drop = FALSE])
  })
  

  output$value <- renderPrint({ input$filter })
  
  output$value <- renderPrint({ input$filter2 })
  
  output$value <- renderPrint({ input$filter3 })
  
  output$value <- renderPrint({ input$filter4 })
  
  output$value <- renderPrint({ input$filter5 })
  
  output$value <- renderPrint({ input$filter6 })
  output$value <- renderPrint({ input$filter7 })
  
  output$value <- renderPrint({ input$filter8 })
  
  output$value <- renderPrint({ input$filter9 })
 
  output$value <- renderPrint({ input$filter11 })
  
  output$value <- renderPrint({ input$filter12 })
  
  output$value <- renderPrint({ input$filter13 })
  output$value <- renderPrint({ input$filter14 })
  
  
  output$value <- renderPrint({ input$filter15 })
  output$value <- renderPrint({ input$filter16 })
  
  output$value <- renderPrint({ input$filter17 })
  output$value <- renderPrint({ input$filter18 })
  
  
  output$value <- renderPrint({ input$filter20 })
  output$value <- renderPrint({ input$filter21 })
  output$value <- renderPrint({ input$filter22 })
  output$value <- renderPrint({ input$filter23 })
  output$value <- renderPrint({ input$filter24 })
  
  
  
  
output$combobox<-renderUI( {
  P<-names(Dataset())
    
 selectizeInput("combo", "Choose Date", choices=P, selected = NULL, multiple = FALSE,
                 options = NULL)
})
 output$combobox2<-renderUI( {
   Q<-names(Dataset())
   
   selectizeInput("combo2", "Search in column", choices=Q, selected = NULL, multiple = FALSE,
                  options = NULL)

})
 
 output$combobox3<-renderUI( {
   G<-names(Dataset())
   
   selectizeInput("combo3", "Search in column", choices=G, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox4<-renderUI( {
   U<-names(Dataset())
   
   selectizeInput("combo4", "Search in column", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox5<-renderUI( {
   U<-names(Dataset())
   
   selectizeInput("combo5", "Search in column", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox6<-renderUI( {
   myshape<- input$inputdata
   if (is.null(myshape)) 
     return(NULL)     
   dir<-dirname(myshape[1,4])
   
   for ( i in 1:nrow(myshape)) {
     file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
   
   getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
   shape<-readShapePoly(getshp)
 shape@data$ID<-c(1:length(shape@data[,1]))
   
   A<-as.data.frame(shape)
   U<-names(A)
   
   selectizeInput("combo6", "Object Id in Shapefile", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 
 output$combobox7<-renderUI( {
   myshape<- input$inputdata
   if (is.null(myshape)) 
     return(NULL)     
   dir<-dirname(myshape[1,4])
   
   for ( i in 1:nrow(myshape)) {
     file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
   
   getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
   shape<-readShapePoly(getshp)
   shape@data$ID<-c(1:length(shape@data[,1]))
   
   A<-as.data.frame(shape)
   U<-names(A)
   
   selectizeInput("combo7", "Location Name in Shapefile:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox8<-renderUI( {
 
   U<-names(Dataset())
   
   selectizeInput("combo8", "Location Name in Dataset", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 
 output$combobox9<-renderUI( {
   myshape<- input$inputdata
   if (is.null(myshape)) 
     return(NULL)     
   dir<-dirname(myshape[1,4])
   
   for ( i in 1:nrow(myshape)) {
     file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
   
   getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
   shape<-readShapePoly(getshp)
   A<-as.data.frame(shape)
   U<-names(A)
   
   selectizeInput("combo9", "Location Name in Shapefile:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox10<-renderUI( {
   
   U<-names(Dataset())
   
   selectizeInput("combo10", "Location Name in Dataset", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox11<-renderUI( {
   
   U<-names(Dataset())
   
   selectizeInput("combo11", "Group by Time Component:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox12<-renderUI( {
   myshape<- input$inputdata
   if (is.null(myshape)) 
     return(NULL)     
   dir<-dirname(myshape[1,4])
   
   for ( i in 1:nrow(myshape)) {
     file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
   
   getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
   shape<-readShapePoly(getshp)
   A<-as.data.frame(shape)
   U<-names(A)
   
   selectizeInput("combo12", "Location Name in Shapefile:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox13<-renderUI( {
   
   U<-names(Dataset())
   
   selectizeInput("combo13", "Location Name in Dataset", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 output$combobox14<-renderUI( {
   
   U<-names(Dataset())
   
   selectizeInput("combo14", "Group by Time Component:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 
 
 output$combobox15<-renderUI( {
   
   U<-names(Dataset())
   
   selectizeInput("combo15", "Group by Time Component:", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })
 
 
 output$combobox16<-renderUI( {
   U<-names(Dataset())
   
   selectizeInput("combo16", "Search in column", choices=U, selected = NULL, multiple = FALSE,
                  options = NULL)
   
 })

  output$slider<-renderUI({
   
    B<-Dataset()
    W<-B[,input$combo]
    M<-max(W,na.rm=TRUE)
    Z<-min(W,na.rm=TRUE)
   
    numericInput("date", "Date:",Z,
                            min = Z, max = M)
  })
  
  output$slider2<-renderUI({
    
    B<-Dataset()
    W<-B[,input$combo11]
    M<-max(W,na.rm=TRUE)
    Z<-min(W,na.rm=TRUE)
    
    numericInput("date2", "Date:",Z,
                 min = Z, max = M)
  })
  
  observe({
  output$plot<-renderPlot({ 
    if (input$action9%%2) {
    B<-Dataset()
    Y1<-B[,input$combo4]
    B$Date = mdy(paste( B$Month, B$Day,B$Date))
    
    filtered_data <- B %>% filter(grepl(input$filter15, Y1))
    filtered_data2 <- filtered_data %>%
      group_by_(as.symbol(input$filter16)) %>%
      summarize(count = n())
  
    ggplot(filtered_data2, aes_string(x=input$filter16,y=input$filter24)) +
      geom_line(color = "#F2CA27", size = 0.5) +
      geom_smooth(color = "#1A1A1A") +
      theme_solarized(light=FALSE)

    
    }
    else{
      B<-Dataset()
      Y1<-B[,input$combo4]
      
      filtered_data <- B %>% filter(grepl(input$filter15, Y1))
      ggplot(filtered_data, aes_string(x=input$filter16,y=input$filter24)) +
        geom_line(color = "#F2CA27", size = 0.5) +
        geom_smooth(color = "#1A1A1A") +
        theme_solarized(light=FALSE)
      
      
      
    }
  
  })   
    
  })
  observe({
  output$data_point_plot<-renderPlot({ 
  
    if (input$action8%%2) {
      B<-Dataset()
      map1 <- get_map(location = input$filter20, source = "stamen", maptype = "toner-lite")
      
      ggmap(map1) +
        geom_point(data = B, aes_string(x=input$filter, y=input$filter2), color = "#27AE60", size = 0.5, alpha = 0.01) +
        theme_solarized() 
    }
    else{
      B<-Dataset()
      lat1<-as.numeric(input$filter3)
      lon1<-as.numeric(input$filter4)
      lat2<-as.numeric(input$filter5)
      lon2<-as.numeric(input$filter6)
      bbox = c(lat1,lon1,lat2,lon2)
      
      
      map <- get_map(location = bbox, source = "stamen", maptype = "toner-lite")
      
      
      
      ggmap(map) +
        geom_point(data = B, aes_string(x=input$filter, y=input$filter2), color = "#27AE60", size = 0.5, alpha = 0.01) +
        theme_solarized() 
      
      
     
      
    }
    
    
    
    
    }) 
    
  })
  
  output$choose_columns3 <- renderUI( {
    # Get the data set with the appropriate name
    colnames <- names(Dataset())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns3", "Group by:", 
                       choices  = colnames,selected=TRUE)
  })
  
  observe({
    output$st_plot<-renderPlot({ 
    if (input$action5%%2) {

    B<-Dataset()

    Y1<-B[,input$combo3]
 
    filtered_data <- B %>% filter(grepl(input$filter11, Y1))
    dots<-input$columns3
    
    filtered_data2 <- filtered_data %>%
      group_by_(.dots=dots) %>%
      summarize(count = n())
    
    
    #read shpfile
    
    myshape<- input$inputdata
    if (is.null(myshape)) 
      return(NULL)     
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    
    districts <-shape  
    P<-districts@data[,input$combo12]
    Z<-unlist(P)
    row.names(districts) <- as.character(Z)
    
    districts@polygons<-districts@polygons[order(Z)]    

    
   
    D1<-filtered_data2[,input$combo14]
    D2<-unlist(D1)

    o <- order(D2)
   sf1<-filtered_data2[o, ]
   K<-sf1[,input$combo14]
   L<-unlist(K)
    dts0 <- paste(as.character(unique(L), 2000), "-01-01", sep = "")
    dts2 <- as.POSIXct(dts0, format = "%Y-%m-%d")
    sf1<-as.data.frame(sf1)
    
    districts@data <- data.frame(districts@data, sf1[match(districts@data[,input$combo12], sf1[,input$combo13]),])
    
    
    sf_st <- STFDF(as(districts,"SpatialPolygons"),dts2,sf1, endTime=delta(dts2))
    stplot(sf_st[, , "count"],mode = "ts", auto.key = list(space = "right",cex = 0.7))
    
    
    
    
 
    }
      else
     {
  
        B<-Dataset()
        
      
        
        
        #read shpfile
        
        myshape<- input$inputdata
        if (is.null(myshape)) 
          return(NULL)     
        dir<-dirname(myshape[1,4])
        
        for ( i in 1:nrow(myshape)) {
          file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
        
        getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
        shape<-readShapePoly(getshp)
        
        districts <-shape  
        P<-districts@data[,input$combo12]
        Z<-unlist(P)
        row.names(districts) <- as.character(Z)
        
        districts@polygons<-districts@polygons[order(Z)]    
        
        
        
        D1<-B[,input$combo14]
        D2<-unlist(D1)
        
        o <- order(D2)
        sf1<-B[o, ]
        K<-sf1[,input$combo14]
        L<-unlist(K)
        dts0 <- paste(as.character(unique(L), 1900), "-01-01", sep = "")
        dts2 <- as.POSIXct(dts0, format = "%Y-%m-%d")
        sf1<-as.data.frame(sf1)
        
        districts@data <- data.frame(districts@data, sf1[match(districts@data[,input$combo12], sf1[,input$combo13]),])
        
        
        sf_st <- STFDF(as(districts,"SpatialPolygons"),dts2,sf1, endTime=delta(dts2))
       stplot(sf_st[, , input$filter11],mode = "ts", auto.key = list(space = "right",cex = 0.7))
       
        
        
    
      
    }
  })
  })
  output$choose_columns2 <- renderUI( {
    # Get the data set with the appropriate name
    colnames <- names(Dataset())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns2", "Group by:", 
                       choices  = colnames,selected=TRUE)
  })
  
  observe({
  output$moran_plot<-renderPlot({ 
    if (input$action7%%2) {
    B<-Dataset()
    Y1<-B[,input$combo5]
    
    filtered_data <- B %>% filter(grepl(input$filter17, Y1))
    dots<-input$columns2
    filtered_data2 <- filtered_data %>%
      group_by_(.dots=dots) %>%
      summarize(count = n())
    
    
    D1<-filtered_data2[,input$combo11]
    D2<-unlist(D1)
    
    o <- order(D2)
    sf1<-filtered_data2[o, ]

    sf1<-as.data.frame(sf1)
    
    #read shpfile
    
    myshape<- input$inputdata
    if (is.null(myshape)) 
      return(NULL)     
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    
    districts <-shape  
    P<-districts@data[,input$combo9]
    Z<-unlist(P)
    
    districts@polygons<-districts@polygons[order(Z)]    
    
    districts@data <- data.frame(districts@data, sf1[match(districts@data[,input$combo9], sf1[,input$combo10]),])
    
    D3<-sf1[,input$combo11]
    D4<-unlist(D3)
    A<-subset(sf1,D4==as.numeric(input$date2))
    
    
    districts_nb<-poly2nb(districts,queen = FALSE)
    listw <- nb2listw(districts_nb)
    moran.plot(A$count,listw)
   
    

    }
    else{
      B<-Dataset()

      
      
      D1<-B[,input$combo11]
      D2<-unlist(D1)
      
      o <- order(D2)
      sf1<-B[o, ]
      
      sf1<-as.data.frame(sf1)
      
      #read shpfile
      
      myshape<- input$inputdata
      if (is.null(myshape)) 
        return(NULL)     
      dir<-dirname(myshape[1,4])
      
      for ( i in 1:nrow(myshape)) {
        file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
      
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      shape<-readShapePoly(getshp)
      
      districts <-shape  
      P<-districts@data[,input$combo9]
      Z<-unlist(P)
      
      districts@polygons<-districts@polygons[order(Z)]    
      
      districts@data <- data.frame(districts@data, sf1[match(districts@data[,input$combo9], sf1[,input$combo10]),])
      
      D3<-sf1[,input$combo11]
      D4<-unlist(D3)
      A<-subset(sf1,D4==as.numeric(input$date2))
      B<-A[,input$filter17]
      
      districts_nb<-poly2nb(districts,queen = FALSE)
      listw <- nb2listw(districts_nb)
      moran.plot(B,listw)
      
      
    }
    
  })
    
    
  })
  
  output$choose_columns5<- renderUI( {
    # Get the data set with the appropriate name
    colnames <- names(Dataset())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns5", "Choose columns", 
                       choices  = colnames,selected=TRUE)
  })
  output$corr_plot<-renderPlot({ 
    B<-Dataset()
    Y1<-B[,input$combo16]
    
    filtered_data <- B %>% filter(grepl(input$filter19, Y1))
    dots<-input$columns5
    filtered_data2 <- filtered_data %>%
      group_by_(.dots=dots) %>%
      summarize()
    

   filtered_data2<-as.data.frame(filtered_data2)    
   M<-cor(filtered_data2)
   corrplot(M,method = "circle") 
    
    
    
    
    
    
  })
  
  output$choose_columns4 <- renderUI( {
    # Get the data set with the appropriate name
    colnames <- names(Dataset())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns4", "Choose columns", 
                       choices  = colnames,selected=TRUE)
  })
  observe({
  output$map_by_year_selection<-renderPlot({ 
    if (input$action6%%2) {
    B<-Dataset()
    Y1<-B[,input$combo2]
    filtered_data <- B %>% filter(grepl(input$filter7, Y1))
    dots<-input$columns4
    filtered_data2 <- filtered_data %>%
      group_by_(.dots=dots) %>%
      summarize(count = n())
    
    myshape<- input$inputdata
    if (is.null(myshape)) 
      return(NULL)     
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-readShapePoly(getshp)
    
    districts <-shape
    
    P<-districts@data[,input$combo7]
    Z<-unlist(P)
    
    districts@polygons<-districts@polygons[order(Z)] 
    
    
    D1<-filtered_data2[,input$combo15]
    D2<-unlist(D1)
    o <- order(D2)
    sf1<-filtered_data2[o, ]
    K<-sf1[,input$combo15]
    L<-unlist(K)
    dts0 <- paste(as.character(unique(L), 2000), "-01-01", sep = "")
    dts2 <- as.POSIXct(dts0, format = "%Y-%m-%d")
    sf1<-as.data.frame(sf1)
    
    
    districts.df <- fortify(districts)
    districts.df <- merge(districts.df, districts@data, by.x="id", by.y=input$combo6, all.x=T, a..ly=F)
    districts.df <- merge(districts.df, sf1, by.x=input$combo7, by.y=input$combo8, all.x=T, a..ly=F)
    
    D3<-districts.df[,input$combo15]
    D4<-unlist(D3)
    A<-subset(districts.df,D4==as.numeric(input$date))
    
    
    ggp <- ggplot(data=A, aes(x=long, y=lat, group=group))  + geom_polygon(aes(fill=count))         # draw polygons
    ggp <- ggp + geom_path(color="grey", linemitre=2)  # draw boundaries
    ggp <- ggp + coord_equal() 
    ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                                     space = "Lab", na.value = "grey50",
                                     guide = "colourbar")
  
    
   print(ggp) 
   
   ggp
   
    }
    else{
      B<-Dataset()
     
      myshape<- input$inputdata
      if (is.null(myshape)) 
        return(NULL)     
      dir<-dirname(myshape[1,4])
      
      for ( i in 1:nrow(myshape)) {
        file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
      
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      shape<-readShapePoly(getshp)
      
      districts <-shape
      districts@data$ID<-c(1:length(districts@data[,1]))
      
      P<-districts@data[,input$combo7]
      Z<-unlist(P)
      
      districts@polygons<-districts@polygons[order(Z)] 
      
      
      D1<-B[,input$combo15]
      D2<-unlist(D1)
      o <- order(D2)
      sf1<-B[o, ]
      K<-sf1[,input$combo15]
      L<-unlist(K)
      dts0 <- paste(as.character(unique(L), 1900), "-01-01", sep = "")
      dts2 <- as.POSIXct(dts0, format = "%Y-%m-%d")
      sf1<-as.data.frame(sf1)
      
      
      districts.df <- fortify(districts)
      
      districts.df <- merge(districts.df, districts@data, by.x="id", by.y=input$combo6, all.x=T, a..ly=F)
      districts.df <- merge(districts.df, sf1, by.x=input$combo7, by.y=input$combo8, all.x=T, a..ly=F)
      
      D3<-districts.df[,input$combo15]
      D4<-unlist(D3)
      A<-subset(districts.df,D4==as.numeric(input$date))
      
      
      ggp <- ggplot(data=A, aes(x=long, y=lat, group=group))  + geom_polygon(aes(fill=count))         # draw polygons
      ggp <- ggp + geom_path(color="grey", linemitre=2)  # draw boundaries
      ggp <- ggp + coord_equal() 
      ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                                       space = "Lab", na.value = "grey50",
                                       guide = "colourbar")
      
      
      print(ggp) 
      
      ggp
      

    }
  
  })
  
  })
  output$HelpBox = renderUI({
    if (input$action%%2){helpText("Define limits of your map's X and Y coordinates")
    }else return()
  })

  
})