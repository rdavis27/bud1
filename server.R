library(ggplot2)
library(reshape)
library(stringr)
library(readxl)
in_shinyapps <- FALSE # fileEncoding="latin1" if TRUE
options(width = 200)
options(max.print = 2000)
#options(error=recover)

shinyServer(
    function(input, output, session) {
        #observe({
        #})
        currentTopic  <- ""
        currentXunits <- ""
        varlist <<- read.csv("varlist.csv", stringsAsFactors = FALSE)
        bvpos <<- varlist[varlist$select > 0 & varlist$select < 90,]
        bvpos <<- bvpos[order(bvpos$topic, bvpos$select),]
        bvall <<- varlist[order(varlist$topic, varlist$select),]
        output$myggPlot <- renderPlot({
            #cat(file=stderr(), "Locale =", Sys.getlocale(), "\n")
            Sys.setlocale(category = "LC_ALL", locale = "C")
            load_data()
            if (input$compareyr){
                sources <- paste("FY", input$year1, "and", input$year2)
                budgets <- "Budgets"
            }
            else{
                sources <- paste("FY", input$year1)
                budgets <- "Budget"
            }
            if (input$topic == "Deficit"){
                main = paste("Selected Surpluses or Deficits(-) from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 7.1, 13.1")
            }
            else if (input$topic == "Outlays"){
                main = paste("Federal Outlays from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 3.1, 10.1")
            }
            else if (input$topic == "Outlays2"){
                main = paste("Other Federal Outlays from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 3.1, 10.1")
            }
            else if (input$topic == "Outlays3"){
                main = paste("Other Federal Outlays from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 3.1, 10.1")
            }
            else if (input$topic == "Outlays vs. Receipts"){
                main = paste("Federal Outlays and Receipts from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 10.1")
            }
            else if (input$topic == "Receipts"){
                main = paste("Federal Receipts from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 1.1, 2.1, 10.1")
            }
            else{
                main = paste("Federal Debt from", sources, budgets)
                xlab = paste0("Source: U.S. Budget, ", sources, ", Historical Tables 7.1, 10.1, 13.1")
            }
            ylab = "Percent of GDP"
            num = 100
            div = gdp$GDP
            if (input$xunits == "Actual Dollars"){
                ylab = "Billions of Dollars"
                num = 1
                div = 1
            }
            else if (input$xunits == "Real Dollars")
            {
                ylab = "Billions of Real Dollars"
                num= 1
                div = gdp$DEFLATOR
            }
            if (input$growth > 0){
                if (input$growth == 1){
                    main <- paste("Growth of", main, "from", input$growth, "year earlier")
                }
                else{
                    main <- paste("Growth of", main, "from", input$growth, "years earlier")
                }
                ylab = paste("Percent Growth in", ylab)
            }
            df2 <- NULL
            if (input$topic == "Deficit"){
                updated <- update_vars("Deficit")
                df  <- def
                if (input$compareyr) df2 <- def2
            }
            else if (input$topic == "Outlays"){
                updated <- update_vars("Outlays")
                df <- out
                if (input$compareyr) df2 <- out2
            }
            else if (input$topic == "Outlays2"){
                updated <- update_vars("Outlays")
                df <- out
                if (input$compareyr) df2 <- out2
            }
            else if (input$topic == "Outlays3"){
                updated <- update_vars("Outlays")
                df <- out
                if (input$compareyr) df2 <- out2
            }
            else if (input$topic == "Outlays vs. Receipts"){
                updated <- update_vars("Receipts")
                df <- rec
                if (input$compareyr) df2 <- rec2
            }
            else if (input$topic == "Receipts"){
                updated <- update_vars("Receipts")
                df <- rec
                if (input$compareyr) df2 <- rec2
            }
            else {
                updated <- update_vars(input$topic)
                df  <- debt
                if (input$compareyr) df2 <- debt2
            }
            vselect <<- varselect
            vnames  <<- varnames
            vlabels <<- varlabels
            if (input$compareyr){
                if (!is.null(df2)){
                    df <- merge(df, df2, by="Year", all.x = TRUE)
                    vnames <<- colnames(df)
                    vnames <<- vnames[-1]
                    vselect2 <<- as.numeric(vselect) + NCOL(df2) - 1
                    vselect <<- c(vselect, vselect2)
                    yr1 <- input$year1 %% 100
                    yr2 <- input$year2 %% 100
                    vlabels1 <<- gsub("$", paste0(input$legendpad,yr1), vlabels)
                    vlabels2 <<- gsub("$", paste0(input$legendpad,yr2), vlabels)
                    vlabels <<- c(vlabels1, vlabels2)
                }
            } 
            ggdf <- calc_growth(data.frame(df$Year, num*subset(df, select = vnames[as.numeric(vselect)])/div), input$growth)
            colnames(ggdf) <- c("Year", vlabels[as.numeric(vselect)])
            if (!updated){
            #if (TRUE){
                ggdf <- melt(ggdf, id=c("Year"))
                if (input$theme == "theme_bw") mytheme <- theme_bw(base_size = 18)
                else if (input$theme == "theme_classic") mytheme <- theme_classic(base_size = 18)
                else if (input$theme == "theme_dark") mytheme <- theme_dark(base_size = 18)
                else if (input$theme == "theme_light") mytheme <- theme_light(base_size = 18)
                else if (input$theme == "theme_linedraw") mytheme <- theme_linedraw(base_size = 18)
                else if (input$theme == "theme_minimal") mytheme <- theme_minimal(base_size = 18)
                else mytheme <- theme_gray(base_size = 18)
                if(input$xscale != ""){
                    sxx <- unlist(strsplit(input$xscale, ","))
                    if (length(sxx) == 1){
                        xx <- as.numeric(sxx)
                        ggdf <- ggdf[ggdf$Year >= xx[1],]
                    }
                    else if (length(sxx) >= 2){
                        xx <- as.numeric(sxx)
                        ggdf <- ggdf[ggdf$Year >= xx[1] & ggdf$Year <= xx[2],]
                    }
                }
                miny = min(ggdf$value)
                yy = NULL
                if(input$yscale != ""){
                    syy <- unlist(strsplit(input$yscale, ","))
                    yy <- as.numeric(syy)
                    if (length(yy) >= 1){
                        miny = yy[1]
                    }
                }
                #parmlist = paste0("topic=",input$topic,"&xunits=",input$xunits,"&graph=",input$graph,"&print=",input$print,"&xscale=",input$xscale,"&yscale=",input$yscale,"&growth=",input$growth,"&theme=",input$theme,"\n")
                graphlist <- paste(input$graph, collapse=',')
                parmlist <- URLencode(paste0("?topic=",input$topic,"&xunits=",input$xunits,"&print=",input$print,
                                             "&xscale=",input$xscale,"&yscale=",input$yscale,"&growth=",input$growth,
                                             "&theme=",input$theme,"&color=",input$color,"&shape=",input$shape,
                                             "&graph=",graphlist))
                cat(file = stderr(), paste0(parmlist,"\n"))
                gg <- ggplot(ggdf, aes(x=Year, y=value, group=variable)) +
                    geom_line(aes(color=variable), size=1, alpha=0.7) +
                    geom_point(aes(color=variable, shape=variable), size=3, alpha=0.7) +
                    ggtitle(main) +
                    #theme(plot.title = element_text(hjust = 0.5)) +
                    xlab(xlab) + ylab(ylab) +
                    geom_vline(xintercept=min_est_yr-0.5) +
                    annotate("text", x=min_est_yr, y=miny, label="Actual    Estimate") +
                    #coord_cartesian(xlim=c(xmin,xmax), ylim=c(ymin,ymax)) +
                    #expand_limits(y = 0) +
                    mytheme
                if(input$xscale != ""){
                    sxx <- unlist(strsplit(input$xscale, ","))
                    if (length(sxx) == 3){
                        xx <- as.numeric(sxx)
                        gg <- gg + scale_x_continuous(name = xlab, breaks = seq(xx[1],xx[2],xx[3]),
                                                      minor_breaks = seq(xx[1],xx[2],xx[3]))
                     }
                    else if (length(sxx) == 4){
                        xx <- as.numeric(sxx)
                        gg <- gg + scale_x_continuous(name = xlab, breaks = seq(xx[1],xx[2],xx[3]),
                                                      minor_breaks = seq(xx[1],xx[2],xx[4]))
                    }
                }
                if(input$yscale != ""){
                    if (length(yy) == 1){
                        gg <- gg + coord_cartesian(ylim=c(yy[1],max(ggdf$value)))
                    }
                    else if (length(yy) == 2){
                        gg <- gg + coord_cartesian(ylim=c(yy[1],yy[2]))
                    }
                    else if (length(yy) == 3){
                        gg <- gg + coord_cartesian(ylim=c(yy[1],yy[2]))
                        gg <- gg + scale_y_continuous(name = ylab, breaks = seq(yy[1],yy[2],yy[3]),
                                                      minor_breaks = seq(yy[1],yy[2],yy[3]))
                    }
                    else if (length(yy) == 4){
                        gg <- gg + coord_cartesian(ylim=c(yy[1],yy[2]))
                        gg <- gg + scale_y_continuous(name = ylab, breaks = seq(yy[1],yy[2],yy[3]),
                                                      minor_breaks = seq(yy[1],yy[2],yy[4]))
                    }
                }
                if(input$color != ""){
                    vcolor <- unlist(strsplit(input$color, ","))
                    if (!input$compareyr) mult <- 1
                    else mult <- 2
                    vcolor <- rep(vcolor, length.out=mult*length(input$graph)) #DEBUG
                    gg <- gg + scale_color_manual(values = vcolor)
                }
                if(input$shape != ""){
                    vshape <- unlist(strsplit(input$shape, ","))
                    if (!input$compareyr) mult <- 1
                    else mult <- 2
                    vshape <- rep(as.numeric(vshape), length.out=mult*length(input$graph))
                    gg <- gg + scale_shape_manual(values = vshape)
                }
                if (input$theme == "theme_gray85"){
                    gg <- gg + theme(panel.background = element_rect(fill = "gray85"))
                    gg <- gg + theme(legend.key = element_rect(fill = "gray85"))
                }
                else if (input$theme == "theme_gray80"){
                    gg <- gg + theme(panel.background = element_rect(fill = "gray80"))
                    gg <- gg + theme(legend.key = element_rect(fill = "gray80"))
                }
                #gg <- gg + theme(panel.background = element_rect(fill = "lightcyan1")) #TEST
                #gg <- gg + theme(panel.border = element_rect(fill = NA, color = "black")) #TEST
                #gg <- gg + theme(panel.grid.major = element_line(colour = "grey50")) #TEST
                #gg <- gg + theme(panel.grid.minor = element_line(colour = "grey20")) #TEST
                #gg <- gg + theme(legend.position = "bottom")
                #gg <- gg + theme(legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0))
                gg
            }
        })
        output$myText <- renderPrint({
            load_data()
            if (input$topic == "Deficit"){
                # Print deficits as a percent of GDP
                mhdr <- data.frame(paste0("RECEIPTS, OUTLAYS, AND SURPLUSES OR DEFICITS(-): 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Deficit",]
                ingraph <- c("7","8","4","3","2","1","5","6") #FIX
                #ingraph <- c("1","2","3","4","5","6","7","8")
            }
            else if (input$topic == "Outlays"){
                # Print outlays as a percent of GDP
                mhdr <- data.frame(paste0("FEDERAL OUTLAYS: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Outlays",]
                ingraph <- c("6","10","13","11","18","15","4","16","21","22","23")
            }
            else if (input$topic == "Outlays2"){
                # Print outlays as a percent of GDP
                mhdr <- data.frame(paste0("OTHER FEDERAL OUTLAYS: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Outlays",]
                ingraph <- c("7","20","19","12","9","2","22","23")
            }
            else if (input$topic == "Outlays3"){
                # Print outlays as a percent of GDP
                mhdr <- data.frame(paste0("OTHER FEDERAL OUTLAYS: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Outlays",]
                ingraph <- c("1","14","17","5","8","3","22","23")
            }
            else if (input$topic == "Outlays vs. Receipts"){
                # Print receipts as a percent of GDP
                mhdr <- data.frame(paste0("FEDERAL RECEIPTS: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Receipts",]
                ingraph <- c("1","2","3","6","7","8","11")
            }
            else if (input$topic == "Receipts"){
                # Print receipts as a percent of GDP
                mhdr <- data.frame(paste0("FEDERAL RECEIPTS: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Receipts",]
                ingraph <- c("1","2","3","6","7","8","11")
            }
            else{
                # Print debts as a percent of GDP
                mhdr <- data.frame(paste0("FEDERAL DEBT AT THE END OF FISCAL YEAR: 1940-", max_est_yr), stringsAsFactors = FALSE)
                mhdr[2,] <-               "(percentage of GDP)"
                colnames(mhdr) <- " "
                bvtopic <- bvall[bvall$topic == "Debt",]
                ingraph <- c("1","2","3","4","5","6")
            }
            if (input$print == TRUE){
                ingraph <- input$graph
            }
            bvmatch <- bvtopic[match(c(0,as.numeric(ingraph),99),bvtopic$select),]
            chdr <- data.frame(bvmatch$hdr1,bvmatch$hdr2,bvmatch$hdr3,bvmatch$hdr4)
            chdr <- t(chdr)
            num = 100
            div = gdp$GDP
            adj = gdp$GDP
            dpz = 1

            if (input$xunits == "Actual Dollars"){
                mhdr[2,] <-           "(billions of dollars)"
                ylab = "Billions of Dollars"
                num = 1
                div = 1
                adj = gdp$GDP
            }
            else if (input$xunits == "Real Dollars")
            {
                mhdr[2,] <-           "(billions of real dollars)"
                chdr[1,ncol(chdr)] <- "Composite"
                chdr[2,ncol(chdr)] <- "Outlay"
                chdr[3,ncol(chdr)] <- "Deflator"
                num= 1
                div = gdp$DEFLATOR
                adj = gdp$DEFLATOR
                dpz = 4
            }
            if (input$growth > 0){
                yrs_earlier <- "years earlier)"
                if (input$growth == 1){
                    yrs_earlier <- "year earlier)"
                }
                mhdr[1,] <- paste("GROWTH OF", mhdr[1,])
                if (input$xunits == "Actual Dollars"){
                    mhdr[2,] <- paste("(percent growth in dollars from", input$growth, yrs_earlier)
                }
                else if (input$xunits == "Real Dollars"){
                    mhdr[2,] <- paste("(percent growth in real dollars from", input$growth, yrs_earlier)
                }
                else{
                    mhdr[2,] <- paste("(percent growth in percent of GDP from", input$growth, yrs_earlier)
                }
            }
            #cat(file=stderr(), "Locale =", Sys.getlocale(), "\n")
            #Sys.setlocale(category = "LC_ALL", locale = "C")
            dp <- bvmatch$dp
            dp[length(dp)] <- dpz
            if (input$topic == "Deficit"){
                tbl <- create_str_table(chdr, def[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else if (input$topic == "Outlays"){
                tbl <- create_str_table(chdr, out[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else if (input$topic == "Outlays2"){
                tbl <- create_str_table(chdr, out[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else if (input$topic == "Outlays3"){
                tbl <- create_str_table(chdr, out[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else if (input$topic == "Outlays vs. Receipts"){
                tbl <- create_str_table(chdr, rec[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else if (input$topic == "Receipts"){
                tbl <- create_str_table(chdr, rec[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            else{
                tbl <- create_str_table(chdr, debt[,c(0,as.numeric(ingraph))+1], dp, num, div, adj, input$growth)
            }
            center_print(chdr[4,], mhdr[1,])
            center_print(chdr[4,], mhdr[2,])
            print(tbl, print.gap = 1, row.names = FALSE)
            graphlist <- paste(input$graph, collapse=',')
            parmlist <- URLencode(paste0("?topic=",input$topic,"&xunits=",input$xunits,"&print=",input$print,
                                         "&xscale=",input$xscale,"&yscale=",input$yscale,"&growth=",input$growth,
                                         "&theme=",input$theme,"&color=",input$color,"&shape=",input$shape,
                                         "&graph=",graphlist))
            cat("\n* = estimated\n\n")
            cat("URL parameters=\n")
            cat(paste0(parmlist,"\n"))
            #cat(file = stderr(), paste0(parmlist,"\n"))
        })
        update_vars <- function(bvtop){
            #if (input$topic == currentTopic){
            if (input$topic == currentTopic & input$xunits == currentXunits){
                varselect <<- input$graph
                updated <- FALSE
            }
            else {
                updated <- TRUE
                if (input$topic == currentTopic) updated <- FALSE
                currentTopic  <<- input$topic
                currentXunits <<- input$xunits
                varnames <<- bvpos$varname[bvpos$topic == bvtop]
                varlabels <<- bvpos$label[bvpos$topic == bvtop]
                #varchoice <- 1:length(varnames)
                varchoice <<- bvpos$select[bvpos$topic == bvtop]
                names(varchoice) <- varlabels
                maxyear <- as.numeric(input$year1)+10
                ysvalue <- ""
                if (input$topic == "Deficit"){
                    #varselect <<- c("1","2","3","4") # FIX
                    varselect <<- c("1","3","4") # remove OASDI
                    #varselect <<- c("6","5","4","3")
                    if (input$xunits == "Percent of GDP") ysvalue <- "-14,2,2"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1970,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    #updateTextInput(session, "color",  label = NULL, value = "red,green4,blue,purple")
                    #updateTextInput(session, "shape",  label = NULL, value = "15,16,17,18,0,1,2,5")
                    updateTextInput(session, "color",  label = NULL, value = "red,blue,green4") # remove OASDI
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,0,1,2") # remove OASDI
                }
                else if (input$topic == "Outlays"){
                    varselect <<- c("6","10","13","11","18","15","4","16")
                    if (input$xunits == "Percent of GDP") ysvalue <- "-1,8,1"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1970,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,green2,green4,blue,orange2,purple,brown,cyan3")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,18,11,9,7,8,0,1,2,5,6,3,4,96")
                    
                }
                else if (input$topic == "Outlays2"){
                    varselect <<- c("7","20","19","12","9","2")
                    if (input$xunits == "Percent of GDP") ysvalue <- "0,1.2,0.1"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1970,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,green4,blue,orange2,purple,black")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,18,9,7,0,1,2,5,3,4")
                }
                else if (input$topic == "Outlays3"){
                    varselect <<- c("1","14","17","5","8","3")
                    if (input$xunits == "Percent of GDP") ysvalue <- "-0.2,0.52,0.1"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1970,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,green4,blue,orange2,purple,black")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,18,9,7,0,1,2,5,3,4")
                }
                else if (input$topic == "Outlays vs. Receipts"){
                    varselect <<- c("11","8")
                    if (input$xunits == "Percent of GDP") ysvalue <- "14,24,1"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1950,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,blue")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,0,1")
                }
                else if (input$topic == "Receipts"){
                    varselect <<- c("1","2","3","6","7")
                    if (input$xunits == "Percent of GDP") ysvalue <- "0,10,1"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1940,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,green4,blue,black,orange2")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,8,18,0,1,2,3,5")
                }
                else {
                    varselect <<- c("1","2","3")
                    if (input$xunits == "Percent of GDP") ysvalue <- "0,120,10"
                    updateTextInput(session, "xscale", label = NULL, value = paste0("1940,",maxyear,",10"))
                    updateTextInput(session, "yscale", label = NULL, value = ysvalue)
                    updateTextInput(session, "color",  label = NULL, value = "red,green4,blue")
                    updateTextInput(session, "shape",  label = NULL, value = "15,16,17,0,1,2")
                }
                updateSelectInput(session, "graph", label = NULL,
                                  choices  = varchoice,
                                  selected = varselect)
            }
            varselect_new <- input$graph # needed for reactive processing
            check_url(input, session)
            return(updated)
        }
        center_print <- function(hh, line){
            hhwid <- sum(nchar(hh)+1)-1
            ttwid <- nchar(line)
            #pad <- truncate(as.integer((hhwid - ttwid) / 2))
            pad <- as.integer((hhwid - ttwid) / 2)
            if (pad < 0) pad = 0;
            #print(str_pad(line, ttwid + pad, "left"), row.names = FALSE, right = FALSE)
            cat(str_pad(line, ttwid + pad, "left"), "\n")
        }
        # Load normal table (one year per row) and call proc_table
        load_data <- function(){
            xls_ext <<- "xls"
            if (as.numeric(input$year1) >= 2019) xls_ext <<- "xlsx"
            xls_ext2 <<- "xls"
            if (as.numeric(input$year2) >= 2019) xls_ext2 <<- "xlsx"
            if (!exists("gdp$DEFLATOR")) load_gdp()
            if (!exists("debt$MediDebt")) load_debt()
            if (!exists("def$MedicSurp")) load_debt()
            if (!exists("ss$SMI_BAL")) load_debt()
            if (!exists("out$Receipts")) load_outlays()
            if (!exists("rec$Outlays")) load_receipts()
            return(gdp)
        }
        load_gdp <- function(){
            #print("========== load_gdp ==========")
            t10 <- load_table(paste0(input$year1,"/hist10z1.",xls_ext), 14, 0)
            gdp <<- create_num_table(t10, c(1:4), c("YEAR","GDP","GDP_CHAINED","DEFLATOR"), 1)
            return(gdp)
        }
        load_debtn <- function(year, ext, suffix){
            #print("========== load_debtn ==========")
            #if (!exists("gdp")) load_gdp()
            yr <- year %% 100
            t1  <- load_table(paste0(year,"/hist01z1.",ext), 3, 41)
            #t1[1:(min_est-2),] <- NA
            def <<- create_num_table(t1, c(1,2,3,4), c("Year","Receipts2","Outlays2","Unified2"), 1000)
            t7  <- load_table(paste0(year,"/hist07z1.",ext), 3, 0) # skip was 5 for csv
            #t7[1:(min_est-2),] <- NA
            debt <<- create_num_table(t7, c(1,2,4,3), c("Year","GrossDebt2","PublicDebt2","GovAccDebt2"), 1000)
            t10 <- load_table(paste0(year,"/hist10z1.",ext), 4, 0) # skip was 14 for csv
            gdp <<- create_num_table(t10, c(1:4), c("YEAR","GDP","GDP_CHAINED","DEFLATOR"), 1)
            t13 <- load_transtable(paste0(year,"/hist13z1.",ext), 2, 4)
            ss  <<- create_num_table(t13, c(1,20,23,44,47,74,78,102,105), c("YEAR","OAS_SURPLUS","OAS_BAL",
                                                                             "DI_SURPLUS","DI_BAL","HI_SURPLUS","HI_BAL","SMI_SURPLUS","SMI_BAL"), 1000)
            debt$OasdiDebt2 <<- ss$OAS_BAL + ss$DI_BAL
            debt$MediDebt2  <<- ss$HI_BAL + ss$SMI_BAL
            debt$WoOasdi2   <<- debt$PublicDebt2 + debt$OasdiDebt2
            debt            <<- debt[,c(1,2,7,3:6)]
            debt$GDP2       <<- gdp$GDP
            
            def$PublicDef2 <<- c(NA, -diff(debt$PublicDebt2))
            def$WoOasdi2   <<- c(NA, -diff(debt$PublicDebt2 + debt$OasdiDebt2))
            def$GrossDef2  <<- c(NA, -diff(debt$GrossDebt2))
            def$OASDISurp2 <<- ss$OAS_SURPLUS + ss$DI_SURPLUS
            def$MedicSurp2 <<- ss$HI_SURPLUS + ss$SMI_SURPLUS
            def            <<- def[,c(1,7:4,8,9,2,3)]
            def$GDP2       <<- gdp$GDP
            colnames(def)  <<- gsub("2", suffix, colnames(def))
            colnames(debt) <<- gsub("2", suffix, colnames(debt))
            return(debt)
        }
        load_debt <- function(){
            if (input$compareyr){
                load_debtn(input$year2, xls_ext2, "2")
                debt2 <<- debt
                def2  <<- def
                gdp2  <<- gdp
                ss2   <<- ss
            }
            load_debtn(input$year1, xls_ext, "")
        }
        load_outlaysn <- function(year, ext, suffix){
            #print("========== load_outlays ==========")
            if (!exists("gdp")) load_gdp()
            if (!exists("def")) load_debt()
            out_names <- c("Year",
                           "Defense2", "HUM_RES2", "Educatn2", "Health2", "Medicare2","Inc_Sec2", "Soc_Sec2", "SS_on2", "SS_off2", "Veterans2",
                           "PHYS_RES2","Energy2",  "Nat_Res2", "Commerce2","Cmrc_on2", "Cmrc_off2","Transprt2","Communty2","Net_Int2", "Int_on2",
                           "Int_off2", "OTH_FUNC2","Interntl2","Science2", "Agricult2","Justice2", "Gen_Govt2","Allownce2","Offs_Rec2","Offs_on2",
                           "Offs_off2","Outlays2", "Outly_on2","Outly_of2")
            t3  <- load_transtable(paste0(year,"/hist03z1.",ext), 1, 0)
            out <<- create_num_table(t3, c(1,3:36), out_names, 1000)
            OtherOut2 <- out$Outlays2 - (out$Defense2 + out$Health2 + out$Medicare2 + out$Inc_Sec2 +
                                           out$Soc_Sec2 + out$Net_Int2 + out$Commerce2 + out$Offs_Rec2)
            out <<- with(out, data.frame(Year, Justice2, Agricult2, Allownce2, Commerce2, Communty2,
                                         Defense2, Educatn2, Energy2, Gen_Govt2, Health2,
                                         Inc_Sec2, Interntl2, Medicare2, Nat_Res2, Net_Int2,
                                         Offs_Rec2, Science2, Soc_Sec2, Transprt2, Veterans2,
                                         OtherOut2, Outlays2))
            if (suffix == "2"){
                out$Receipts2 <<- def2$Receipts
                out$GDP2      <<- gdp2$GDP
                out[1:(min_est-2),] <<- NA
            }
            else{
                out$Receipts2 <<- def$Receipts
                out$GDP2      <<- gdp$GDP
            }
            colnames(out)   <<- gsub("2", suffix, colnames(out))
            return(out)
        }
        load_outlays <- function(){
            if (input$compareyr){
                load_outlaysn(input$year2, xls_ext2, "2")
                out2 <<- out
            }
            load_outlaysn(input$year1, xls_ext, "")
        }
        load_receiptsn <- function(year, ext, suffix){
            #print("========== load_receiptsn ==========")
            if (!exists("gdp")) load_gdp()
            if (!exists("def")) load_debt()
            rec_names <- c("Year",
                           "Individual2", "Corporate2", "SocialIns2", "SocInsOn2",  "SocInsOff2",
                           "Excise2","Other2","Receipts2","ReceiptsOn2","ReceiptsOff2")
            t2  <- load_table(paste0(year,"/hist02z1.",ext), 3, 6) # skip to 1-line header, then skip to 1940
            rec <<- create_num_table(t2, c(1:11), rec_names, 1000)
            if (suffix == "2"){
                rec$Outlays <<- def2$Outlays
            }
            else rec$Outlays <<- def$Outlays
            rates <- read.csv("taxrates.csv", skip = 3, stringsAsFactors = FALSE)
            rates <- rates[rates$Year >= 1940,]
            rates <- rates[1:NROW(rec),]
            rec$TopRate  <<- rates$TopRate
            rec$FicaRate <<- rates$FicaRate
            #rec$TopRate  <<- rates$TopRate[rates$Year >= 1940]
            #rec$FicaRate <<- rates$FicaRate[rates$Year >= 1940]
            if (suffix == "2"){
                rec$GDP     <<- gdp2$GDP
            }
            else rec$GDP     <<- gdp$GDP
            colnames(rec)   <<- gsub("2", suffix, colnames(rec))
            return(rec)
        }
        load_receipts <- function(){
            if (input$compareyr){
                load_receiptsn(input$year2, xls_ext2, "2")
                rec2 <<- rec
            }
            load_receiptsn(input$year1, xls_ext, "")
        }
        # Load normal table (one year per row) and call proc_table
        load_table <- function(file, rskip, cskip){
            #print(paste0("READ ", file))
            if (in_shinyapps){
                tt <- read_excel(file, skip = rskip, fileEncoding="latin1") # implied stringsAsFactors = FALSE 
            }
            else{
                tt <- read_excel(file, skip = rskip) # implied stringsAsFactors = FALSE
            }
            proc_table(tt, cskip)
        }
        # Load transposed table (one year per column), transpose and call proc_table
        load_transtable <- function(file, rskip, cskip){
            #print(paste("READ", file))
            if (in_shinyapps){
                tt <- read_excel(file, skip = rskip, col_names = FALSE, fileEncoding="latin1") # implied stringsAsFactors = FALSE
            }
            else{
                tt <- read_excel(file, skip = rskip, col_names = FALSE) # implied stringsAsFactors = FALSE
            }
            uu <- as.data.frame(t(tt[,-1]), stringsAsFactors = FALSE)
            colnames(uu) <- tt[,1]
            proc_table(uu, cskip)
        }
        # Skip first rskip rows (leaving one header row) and first cskip years
        # Name first column SYEAR and remove TQ row
        # Remove " estimate" from SYEAR and store index of first estimate in est_yr1
        # If there were estimates, remove all rows following estimates
        proc_table <- function(tt, cskip){
            if (cskip > 0){
                tt <- tt[cskip+1:nrow(tt),]
            }
            colnames(tt)[1] <- "SYEAR"
            tt <- tt[tt$SYEAR != "TQ",]
            est_i <- grep(" estimate", tt$SYEAR)
            min_est <<- min(est_i)
            max_est <<- max(est_i)
            if (max_est > 0) tt <- tt[1:max_est,] # remove rows after last estimate
            tt$SYEAR <- sub(" estimate", "", tt$SYEAR)
            min_est_yr <<- as.integer(tt$SYEAR[min_est])
            max_est_yr <<- as.integer(tt$SYEAR[max_est])
            tt$YEAR <- as.integer(tt$SYEAR)
            #print(str(tt))
            return(tt)
        }
        create_num_table <- function(tt, indices, names, div){
            #print("========== RUN create_num_table ==========")
            uu <- data.frame(tt[1])
            for (i in 2:length(indices)){
                indx <- indices[i]
                uu[,names[i]] <- tt[indx]
                uu[,names[i]] <- gsub("..........", "0", uu[,names[i]], fixed = TRUE)
                uu[,names[i]] <- as.numeric(gsub(",", "", uu[,names[i]])) / div
            }
            uu[,1] <- as.numeric(uu[,1])
            colnames(uu)[1] <- names[1]
            return(uu)
        }
        adjust_num_table <- function(tt, num, div, name){
            uu <- tt
            for (i in 2:ncol(tt)){
                uu[,i] <- tt[,i] * num / div
            }
            uu[name] <- div
            return(uu)
        }
        create_str_table <- function(hh, tt, dp, num, div, adj, growth){
            #print("========== RUN create_str_table ==========")
            uu <- tt
            for (i in 2:ncol(tt)){
                uu[,i] <- num * tt[,i] / div
            }
            uu$ADJ <- adj
            if (growth > 0){
                start = 1 + growth
                div <- div[c(start:nrow(uu))]
                adj <- adj[c(start:nrow(uu))]
                min_est <- min_est - growth
                max_est <- max_est - growth
                uu <- calc_growth(uu, growth)
            }
            for (i in 2:ncol(tt)){
                uu[,i] <- format(round(uu[,i], digits = dp[i]), nsmall = dp[i], big.mark = ",")
            }
            i <- ncol(uu)
            uu[,ncol(uu)] <- format(round(adj, digits = dp[i]), nsmall = dp[i], big.mark = ",")
            uu[1:(min_est-1),1] <- paste0(uu[1:(min_est-1),1], " ")
            uu[min_est:max_est,1] <- paste0(uu[min_est:max_est,1], "*")
            colnames(hh) <- colnames(uu) # REMOVE IF POSSIBLE
            uu <- rbind(hh, uu)
            for (i in 1:ncol(uu)){
                colnames(uu)[i] <- " "
            }
            return(uu)
        }
        calc_growth <- function(dd, lg){
            if (lg <= 0){
                return(dd)
            }
            ee <- dd[c((1+lg):nrow(dd)),]
            for (i in 2:ncol(dd)){
                num <- 100 * diff(dd[,i], lag = lg)
                den <- dd[c(1:(nrow(dd)-lg)),i]
                ee[,i] <- num / den
            }
            return(ee)
        }
        check_url <- function(input, session){
            query <- parseQueryString(session$clientData$url_search)
            #for (i in 1:(length(reactiveValuesToList(input)))){
            #    nameval = names(reactiveValuesToList(input)[i])
            #    print(paste(i, nameval, query[[nameval]]))
            #}
            if (input$ignore == FALSE){
                if (!is.null(query[['topic']])){
                    updateSelectInput(session, "topic", selected = query[['topic']])
                }
                if (!is.null(query[['xunits']])){
                    updateSelectInput(session, "xunits", selected = query[['xunits']])
                }
                if (!is.null(query[['print']])){
                    updateCheckboxInput(session, "print", value = query[['print']])
                }
                if (!is.null(query[['xscale']])){
                    updateTextInput(session, "xscale", value = query[['xscale']])
                }
                if (!is.null(query[['yscale']])){
                    updateTextInput(session, "yscale", value = query[['yscale']])
                }
                if (!is.null(query[['growth']])){
                    updateNumericInput(session, "growth", value = as.numeric(query[['growth']]))
                }
                if (!is.null(query[['theme']])){
                    updateSelectInput(session, "theme", selected = query[['theme']])
                }
                if (!is.null(query[['color']])){
                    updateTextInput(session, "color", value = query[['color']])
                }
                if (!is.null(query[['shape']])){
                    updateTextInput(session, "shape", value = query[['shape']])
                }
                if (!is.null(query[['graph']])){
                    updateSelectInput(session, "graph", selected = unlist(strsplit(query[['graph']],",")))
                }
            }
        }
    }
)
