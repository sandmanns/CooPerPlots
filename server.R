library("shiny")
library("shinyjs")
library("shinythemes")
library("shinydashboard")
library("DT")
library("igraph")
#library("stringr")
shinyServer(function(input, output) {
    output$ColorsUI<-renderUI({conditionalPanel(
        condition="input.progredient=='Yes'",
        radioButtons('relativeColor',label="Base coloring on frequency",choices=c("absolute","relative"),selected="absolute",inline=T)
    )})
    
    output$ColorsUI_d<-renderUI({conditionalPanel(
        condition="input.progredient_d=='Yes'",
        radioButtons('relativeColor_d',label="Base coloring on frequency",choices=c("absolute","relative"),selected="absolute",inline=T)
    )})
    
    
    shinyjs::html("text", paste0("<br>Reading input data<br>"), add = FALSE)
    input1<-data.frame(Initial.Symptoms=c("Fatigue, Anosmia","Fever, Cough, Fatigue, Rhinitis",
                                          "Cough, Fatigue, Diarrhea","Fever, Headache, Fatigue, Cough, Diarrhea","Headache, Myalgia, Fatigue, Dyspnea",
                                          "Headache, Myalgia, Diarrhea, Anosmia","Cough, Headache, Anosmia","Cough","Anosmia, Fever","",
                                          "Fever, Anosmia, Cough","Headache, Anosmia","Fever, Cough, Fatigue, Dyspnea","Fatigue, Myalgia, Anosmia, Cough",
                                          "Fever, Cough, Fatigue","Fatigue, Cough, Fever, Headache","Headache, Myalgia, Soar Throat, Fever, Chills, Anosmia",
                                          "Cough, Headache, Soar Throat, Fatigue, Anosmia","Fever, Chills, Cough, Chest Pain, Dyspnea, Hemoptysis",
                                          "Rhinitis, Anosmia","Anosmia, Fatigue, Dyspnea","Headache, Myalgia, Cough, Fever","Anosmia, Cough","Anosmia",
                                          "Cough, Dyspnea","Cough, Dyspnea","Anosmia","","Anosmia, Diarrhea, Fever","Fever, Fatigue, Myalgia",
                                          "Headache, Anosmia","Cough, Myalgia, Fever, Diarrhea, Anosmia","Soar Throat, Diarrhea, Anosmia, Fatigue, Dyspnea",
                                          "Soar Throat, Fatigue, Myalgia","Cough, Hoarseness, Fever, Rhinitis, Dyspnea, Fatigue, Anosmia",
                                          "Fever, Fatigue, Dyspnea","Cough, Soar Throat, Rhinitis","Fever, Cough","Fever, Dyspnea, Hemoptysis",
                                          "Anosmia, Fever, Fatigue, Chills","Chills, Fever, Myalgia","Fever, Cough, Fatigue, Anosmia","Fatigue",
                                          "Fever, Myalgia, Dyspnea","Cough, Fatigue, Anosmia, Dyspnea","Cough, Fever, Headache, Anosmia, Fatigue",
                                          "Soar Throat, Headache, Fatigue, Myalgia, Fever, Dyspnea, Cough","Anosmia","Soar Throat, Cough, Anosmia, Fatigue, Dyspnea, Chest Pain",
                                          "Myalgia, Headache, Anosmia, Dyspnea, Tinnitus","Fever, Fatigue, Chills, Anosmia","Rhinitis, Fatigue, Anosmia",
                                          "Soar Throat, Cough, Anosmia","Fatigue, Diarrhea, Anosmia","Cough, Soar Throat, Fatigue, Fever",
                                          "Rhinitis, Cough, Anosmia","Cough, Fever, Dyspnea","Myalgia","","Soar Throat, Myalgia, Anosmia",
                                          "Cough, Soar Throat","Fever, Headache, Fatigue","Cough, Fatigue, Fever, Dyspnea","Fever, Cough, Fatigue, Anosmia",
                                          "Rhinitis, Cough, Diarrhea, Anosmia","Cough, Anosmia","Cough, Chills, Fever","Dyspnea, Fever, Anosmia, Headache",
                                          "Fever, Rhinitis, Cough","Myalgia, Anosmia","Cough, Fever, Soar Throat","Fever, Fatigue, Anosmia","",
                                          "Anosmia","Fatigue, Chills, Soar Throat, Cough, Dyspnea","Myalgia, Fever, Cough","Fatigue, Cough, Dyspnea, Fever",
                                          "Fatigue, Headache, Myalgia, Fever, Anosmia","Fever, Myalgia, Anosmia","Headache, Soar Throat, Fever, Anosmia",
                                          "Myalgia, Anosmia","Fever, Headache, Myalgia","","Cough, Fatigue","Headache, Anosmia, Fever, Fatigue",
                                          "Soar Throat, Fatigue, Dyspnea, Myalgia, Cough","Headache, Diarrhea, Fatigue, Dyspnea","Cough, Rhinitis",
                                          "Soar Throat, Headache, Fatigue, Dyspnea, Anosmia","Rhinitis, Soar Throat","Chills, Fever, Myalgia",
                                          "Cough, Rhinitis, Fatigue","Rhinitis, Anosmia, Fatigue","Rhinitis, Headache, Cough","Fever, Chills, Myalgia, Cough",
                                          "Fatigue, Cough, Diarrhea","Anosmia, Cough, Fever, Headache, Diarrhea, Dyspnea","Headache, Myalgia, Fatigue",
                                          "Chest Pain, Fever, Cough, Rhinitis","Fatigue, Night Sweats, Anosmia","Anosmia",
                                          "Myalgia, Fatigue, Cough, Fever, Soar Throat, Anosmia","Headache, Soar Throat, Fatigue, Cough",
                                          "Fever, Myalgia,  Cough, Anosmia","Cough, Headache, Fatigue","Anosmia, Fever, Fatigue, Cough",
                                          "Soar Throat, Myalgia, Headache, Fatigue, Fever","Headache, Cough, Myalgia","Chills, Headache, Dizziness",
                                          "Headache, Myalgia, Dyspnea","Headache, Anosmia, Myalgia, Soar Throat, Cough","Anosmia",
                                          "Dyspnea, Diarrhea, Headache, Myalgia, Fatigue","Cough, Headache, Myalgia, Anosmia","Rhinitis, Anosmia",
                                          "Fatigue, Myalgia, Cough"),
                       Progredient.ongoing.Symptoms=c("Anosmia, Dizziness","","","Fatigue","","Anosmia","","",
                                                      "","","","","Dyspnea","","","","","","Fatigue","","Dyspnea","","","","","Fatigue",
                                                      "Fatigue","","","","","Anosmia","","","Fatigue","","","","","Anosmia","","","",
                                                      "Fatigue","","","Fatigue","","Cough, Dyspnea","Fatigue, Tinnitus","","","","","","","","",
                                                      "","","","","","","","","","Fatigue","","","","Fatigue","","","Fatigue, Dyspnea",
                                                      "","","Anosmia","","","","","","","","","","","Dyspnea","","","","Anosmia, Fatigue",
                                                      "","","","Dyspnea, Fatigue","","","","","","","","","","","","","","","","","Myalgia",
                                                      "","Dyspnea"))
    
    #initial<-str_split_fixed(string=input1[,1],pattern=",",n=Inf)
    list_helper<-strsplit(input1[,1],split=",")
    initial<-matrix(data=rep("",100),ncol=100)
    initial<-as.data.frame(initial)
    for(l in 1:length(list_helper)){
        if(length(list_helper[[l]])>0){
            for(m in 1:length(list_helper[[l]])){
                initial[l,m]<-list_helper[[l]][m]
            }
        }
        if(length(list_helper[[l]])==0){
            initial[l,1]<-NA
        }
    }
    initial<-as.matrix(initial)
    initial[is.na(initial)]<-""
    initial<-initial[,colSums(initial!="")>0]
    initial<-gsub(" ","",initial)
    
    #progredient<-str_split_fixed(string=input1[,2],pattern=",",n=Inf)
    list_helper<-strsplit(input1[,2],split=",")
    progredient<-matrix(data=rep("",100),ncol=100)
    progredient<-as.data.frame(progredient)
    for(l in 1:length(list_helper)){
        if(length(list_helper[[l]])>0){
            for(m in 1:length(list_helper[[l]])){
                progredient[l,m]<-list_helper[[l]][m]
            }
        }
        if(length(list_helper[[l]])==0){
            progredient[l,1]<-NA
        }
    }
    progredient<-as.matrix(progredient)
    progredient[is.na(progredient)]<-""
    progredient<-progredient[,colSums(progredient!="")>0]
    progredient<-gsub(" ","",progredient)
    
    temp<-sort(c(initial[,1],initial[,2],initial[,3],initial[,4],initial[,5],initial[,6],initial[,7]))
    temp<-gsub(" ","",temp)
    temp<-unique(temp)
    temp<-temp[temp!=""]
    
    links<-matrix(data = rep(0,length(temp)*length(temp)),ncol=length(temp))
    colnames(links)<-temp
    rownames(links)<-temp
    for(i in 1:length(initial[,1])){
        for(one in 1:length(initial[1,])){
            for(two in 1:length(initial[1,])){
                if(initial[i,one]!=""&&initial[i,two]!=""){
                    links[colnames(links)==initial[i,one],rownames(links)==initial[i,two]]<-links[colnames(links)==initial[i,one],rownames(links)==initial[i,two]]+1
                }
            }
        }
    }
    
    net1 <- graph_from_incidence_matrix(links)
    net2.bp <- bipartite.projection(net1)
    
    point_size<-c()
    for(i in 1:length(links[,1])){
        point_size[i]<-links[i,i]
    }
    
    temp2<-net2.bp$proj1
    temp3<-ends(temp2,es = E(temp2))
    
    edge_size<-c()
    for(i in 1:length(temp3[,1])){
        zeile<-which(colnames(links)==temp3[i,1])
        spalte<-which(colnames(links)==temp3[i,2])
        edge_size<-c(edge_size,links[zeile,spalte])
    }
    
    net.sp <- delete_edges(net2.bp$proj1, E(net2.bp$proj1)[edge_size<1])
    temp3<-ends(net.sp,es = E(net.sp))
    edge_size2<-c()
    for(i in 1:length(temp3[,1])){
        zeile<-which(colnames(links)==temp3[i,1])
        spalte<-which(colnames(links)==temp3[i,2])
        edge_size2<-c(edge_size2,links[zeile,spalte])
    }
    
    test.layout <- layout_(net.sp,with_dh(weight.edge.lengths = edge_density(net.sp)/300))
    point_size2<-point_size/3
    point_size2[point_size2<3]<-3
    
    #progredient<-str_split_fixed(string=input1$Progredient.ongoing.Symptoms,pattern=",",n=Inf)
    list_helper<-strsplit(input1$Progredient.ongoing.Symptoms,split=",")
    progredient<-matrix(data=rep("",100),ncol=100)
    progredient<-as.data.frame(progredient)
    for(l in 1:length(list_helper)){
        if(length(list_helper[[l]])>0){
            for(m in 1:length(list_helper[[l]])){
                progredient[l,m]<-list_helper[[l]][m]
            }
        }
        if(length(list_helper[[l]])==0){
            progredient[l,1]<-NA
        }
    }
    progredient<-as.matrix(progredient)
    progredient[is.na(progredient)]<-""
    progredient<-progredient[,colSums(progredient!="")>0]
    progredient<-gsub(" ","",progredient)
    
    progredient2<-c(progredient[,1],progredient[,2])
    progredient2<-progredient2[progredient2!=""]
    freq_pro<-table(progredient2)
    initial2<-c(initial[,1],initial[,2],initial[,3],initial[,4],initial[,5],initial[,6],initial[,7])
    initial2<-initial2[initial2!=""]
    freq_ini<-table(initial2)
    
    staying<-data.frame(Category=names(freq_ini),initial=as.numeric(freq_ini),progredient=0,rel=0)
    for(i in 1:length(freq_pro)){
        staying[staying[,1]==names(freq_pro)[i],3]<-as.numeric(freq_pro)[i]
        staying[staying[,1]==names(freq_pro)[i],4]<-as.numeric(freq_pro)[i]/staying[staying[,1]==names(freq_pro)[i],2]
    }
    names(staying)<-c("Symptom","Initial","Progredient","Rel [%]")
    staying[,4]<-round(staying[,4],digits = 4)*100
    
    links_p<-matrix(data = rep(0,length(temp)*length(temp)),ncol=length(temp))
    colnames(links_p)<-temp
    rownames(links_p)<-temp
    for(i in 1:length(progredient[,1])){
        for(one in 1:length(progredient[1,])){
            for(two in 1:length(progredient[1,])){
                if(progredient[i,one]!=""&&progredient[i,two]!=""){
                    links_p[colnames(links_p)==progredient[i,one],rownames(links_p)==progredient[i,two]]<-links_p[colnames(links_p)==progredient[i,one],rownames(links_p)==progredient[i,two]]+1
                }
            }
        }
    }
    edge_size2_dp<-c()
    for(i in 1:length(temp3[,1])){
        zeile<-which(colnames(links_p)==temp3[i,1])
        spalte<-which(colnames(links_p)==temp3[i,2])
        edge_size2_dp<-c(edge_size2_dp,links_p[zeile,spalte])
    }
    
    
    output$table_dt<-renderDataTable(expr=staying)
    
    test.layout2_d <- layout_(net.sp,with_dh(weight.edge.lengths = edge_density(net.sp)/400))
    colors<-colorRampPalette(c("gold","darkred"))(max(staying[,3])+1)
    
    edge.color.helper_d<-rep("grey65",length(edge_size2_dp))
    edge.color.helper_d[edge_size2_dp!=0]<-"black"
    
    output$plot1 <- renderPlot({
        plot(net.sp,
             vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
             vertex.size=point_size2,edge.width=edge_size2/1.5,margin=c(0,0,0,0),
             vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[staying[,3]+1],
             vertex.label.color=c(rep("black",7),"white",rep("black",9)),
             vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
             vertex.label.font=2,edge.color="grey65")
        #plot(net.sp,
        #     vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
        #     vertex.size=point_size2,edge.width=edge_size2_dp/1.5,margin=c(0,0,0,0),
        #     vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[staying[,3]+1],
        #     vertex.label.color=c(rep("black",7),"white",rep("black",9)),
        #     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
        #     vertex.label.font=2,add = T,edge.color=edge.color.helper_d)
    }
    ,res=50,width = 1100,height=1100)

    demo_table<-staying
    demo_net<-net.sp
    demo_points<-point_size2
    demo_edge<-edge_size2
    demo_layout<-test.layout2_d

helper<-reactive({FALSE})
    shinyjs::html("text", paste0("<br>Analysis successfull!<br><br>"), add = TRUE)
    shinyjs::html("text", paste0("<center><p style=\"color:#FF0000\";>Anonymous data<br><br></p>"), add = TRUE)
    
    #Analysis
    observeEvent(input$do,{
        shinyjs::html("text", paste0("<br>Reading input data<br>"), add = FALSE)
        input_temp<-input$inputFile
        if(is.null(input_temp)){
            shinyjs::html("text", paste0("No input file defined","<br>"), add = TRUE) 
            output$plot1 <- renderPlot(NULL)
            return()
        }
        input1<-read.table(input_temp$datapath,header=T,quote = "",sep="\t",stringsAsFactors = F)
        if(length(input1[,1])==0){
            shinyjs::html("text", paste0("Symptom information for at least one patient is required","<br>"), add = TRUE) 
            output$plot1 <- renderPlot(NULL)
            return()
        }
        if(length(input1[1,])>2){
            shinyjs::html("text", paste0("More than 2 columns provided as input <br> -> just first 2 columns will be considered","<br>"), add = TRUE) 
        }

        if(length(grep(pattern=",",input1[,1],fixed=T))==0){
            shinyjs::html("text", paste0("Column 1 (initial) only contains max. 1 symptom per patient","<br>"), add = TRUE) 
        }
        if(length(input1[1,])>=2&&length(grep(",",input1[,2],fixed=T))==0){
            shinyjs::html("text", paste0("Column 2 (progredient) only contains max. 1 symptom per patient","<br>"), add = TRUE) 
        }
        
        #initial<-str_split_fixed(string=input1[,1],pattern=",",n=Inf)
        list_helper<-strsplit(input1[,1],split=",")
        initial<-matrix(data=rep("",100),ncol=100)
        initial<-as.data.frame(initial)
        for(l in 1:length(list_helper)){
            if(length(list_helper[[l]])>0){
                for(m in 1:length(list_helper[[l]])){
                    initial[l,m]<-list_helper[[l]][m]
                }
            }
            if(length(list_helper[[l]])==0){
                initial[l,1]<-NA
            }
        }
        initial<-as.matrix(initial)
        initial[is.na(initial)]<-""
        initial<-initial[,colSums(initial!="")>0]
        initial<-gsub(" ","",initial)
        
        #progredient<-str_split_fixed(string=input1[,2],pattern=",",n=Inf)
        list_helper<-strsplit(input1[,2],split=",")
        progredient<-matrix(data=rep("",100),ncol=100)
        progredient<-as.data.frame(progredient)
        for(l in 1:length(list_helper)){
            if(length(list_helper[[l]])>0){
                for(m in 1:length(list_helper[[l]])){
                    progredient[l,m]<-list_helper[[l]][m]
                }
            }
            if(length(list_helper[[l]])==0){
                progredient[l,1]<-NA
            }
        }
        progredient<-as.matrix(progredient)
        progredient[is.na(progredient)]<-""
        progredient<-progredient[,colSums(progredient!="")>0]
        progredient<-gsub(" ","",progredient)
        
        helper<-c()
        for(i in 1:length(initial[1,])){
            helper<-c(helper,initial[,i])
        }
        
        temp<-sort(helper)
        temp<-gsub(" ","",temp)
        temp<-unique(temp)
        temp<-temp[temp!=""]
        
        links<-matrix(data = rep(0,length(temp)*length(temp)),ncol=length(temp))
        colnames(links)<-temp
        rownames(links)<-temp
        for(i in 1:length(initial[,1])){
            for(one in 1:length(initial[1,])){
                for(two in 1:length(initial[1,])){
                    if(initial[i,one]!=""&&initial[i,two]!=""){
                        links[colnames(links)==initial[i,one],rownames(links)==initial[i,two]]<-links[colnames(links)==initial[i,one],rownames(links)==initial[i,two]]+1
                    }
                }
            }
        }
        
        net1 <- graph_from_incidence_matrix(links)
        net2.bp <- bipartite.projection(net1)
        
        point_size<-c()
        for(i in 1:length(links[,1])){
            point_size[i]<-links[i,i]
        }
        
        temp2<-net2.bp$proj1
        temp3<-ends(temp2,es = E(temp2))
        
        edge_size<-c()
        for(i in 1:length(temp3[,1])){
            zeile<-which(colnames(links)==temp3[i,1])
            spalte<-which(colnames(links)==temp3[i,2])
            edge_size<-c(edge_size,links[zeile,spalte])
        }
        
        net.sp <- delete_edges(net2.bp$proj1, E(net2.bp$proj1)[edge_size<1])
        temp3<-ends(net.sp,es = E(net.sp))
        edge_size2<-c()
        for(i in 1:length(temp3[,1])){
            zeile<-which(colnames(links)==temp3[i,1])
            spalte<-which(colnames(links)==temp3[i,2])
            edge_size2<-c(edge_size2,links[zeile,spalte])
        }
        
        test.layout <- layout_(net.sp,with_dh(weight.edge.lengths = edge_density(net.sp)/300))
        point_size2<-point_size/(max(point_size)/20)
        point_size2[point_size2<3]<-3
        
        if(length(input1[1,])==2){
            #progredient<-str_split_fixed(string=input1[,2],pattern=",",n=Inf)
            list_helper<-strsplit(input1[,2],split=",")
            progredient<-matrix(data=rep("",100),ncol=100)
            progredient<-as.data.frame(progredient)
            for(l in 1:length(list_helper)){
                if(length(list_helper[[l]])>0){
                    for(m in 1:length(list_helper[[l]])){
                        progredient[l,m]<-list_helper[[l]][m]
                    }
                }
                if(length(list_helper[[l]])==0){
                    progredient[l,1]<-NA
                }
            }
            progredient<-as.matrix(progredient)
            progredient[is.na(progredient)]<-""
            progredient<-progredient[,colSums(progredient!="")>0]
            progredient<-gsub(" ","",progredient)
            
            helper2<-c()
            for(i in 1:length(progredient[1,])){
                helper2<-c(helper2,progredient[,i])
            }
            
            progredient2<-c(helper2)
            progredient2<-progredient2[progredient2!=""]
            freq_pro<-table(progredient2)
            initial2<-helper
            initial2<-initial2[initial2!=""]
            freq_ini<-table(initial2)
            
            staying<-data.frame(Category=names(freq_ini),initial=as.numeric(freq_ini),progredient=0,rel=0)
            for(i in 1:length(freq_pro)){
                staying[staying[,1]==names(freq_pro)[i],3]<-as.numeric(freq_pro)[i]
                staying[staying[,1]==names(freq_pro)[i],4]<-as.numeric(freq_pro)[i]/staying[staying[,1]==names(freq_pro)[i],2]
            }
            names(staying)<-c("Symptom","Initial","Progredient","Rel [%]")
            staying[,4]<-round(staying[,4],digits = 4)*100
        }
        
        if(length(input1[1,])==1){
            initial2<-helper
            initial2<-initial2[initial2!=""]
            freq_ini<-table(initial2)
            
            staying<-data.frame(Category=names(freq_ini),initial=as.numeric(freq_ini))
            names(staying)<-c("Symptom","Initial")
        }
        
        output$table_dt<-renderDataTable(expr=staying)
        
        test.layout2 <- layout_(net.sp,with_dh(weight.edge.lengths = edge_density(net.sp)/400))
        
        easy<-F
        if(input$progredient=="Yes"&&length(staying[1,])==2){
            shinyjs::html("text", paste0("No progredient symptoms for coloring provided","<br>"), add = TRUE) 
            easy<-T
        }
        if(input$progredient=="No"){
            easy<-T
        }
        
        dist_help<-rep(1,length(point_size2))
        dist_help[point_size2>=8]<-0
        if(easy==T){
            output$plot1 <- renderPlot(expr=plot(net.sp,
                                                 vertex.label.dist=dist_help,
                                                 vertex.size=point_size2,edge.width=edge_size2/(max(edge_size2/20)),margin=c(0,0,0,0),
                                                 vertex.label.cex=2,layout=test.layout2,
                                                 vertex.label.color="black",
                                                 vertex.label=paste0(staying[,1],"\n",staying[,2],sep=""),
                                                 vertex.label.font=2,edge.color="grey65")
                                       ,res=50,width = 1100,height=1100)
        }
        
        if(easy==F){
            links_p<-matrix(data = rep(0,length(temp)*length(temp)),ncol=length(temp))
            colnames(links_p)<-temp
            rownames(links_p)<-temp
            for(i in 1:length(progredient[,1])){
                for(one in 1:length(progredient[1,])){
                    for(two in 1:length(progredient[1,])){
                        if(progredient[i,one]!=""&&progredient[i,two]!=""){
                            links_p[colnames(links_p)==progredient[i,one],rownames(links_p)==progredient[i,two]]<-links_p[colnames(links_p)==progredient[i,one],rownames(links_p)==progredient[i,two]]+1
                        }
                    }
                }
            }
            edge_size2_p<-c()
            for(i in 1:length(temp3[,1])){
                zeile<-which(colnames(links_p)==temp3[i,1])
                spalte<-which(colnames(links_p)==temp3[i,2])
                edge_size2_p<-c(edge_size2_p,links_p[zeile,spalte])
            }
            edge.color.helper<-rep("grey65",length(edge_size2_p))
            edge.color.helper[edge_size2_p!=0]<-"black"
            
            
            if(input$relativeColor=="absolute"){
                colors<-colorRampPalette(c("gold","darkred"))(max(staying[,3])+1)
                col_help<-rep("black",length(point_size2))
                col_help[staying[,3]>=0.8*(max(staying[,3]+1))]<-"white"
                output$plot1 <- renderPlot({plot(net.sp,
                                                     vertex.label.dist=dist_help,
                                                     vertex.size=point_size2,edge.width=edge_size2/(max(edge_size2/20)),margin=c(0,0,0,0),
                                                     vertex.label.cex=2,layout=test.layout2,vertex.color=colors[staying[,3]+1],
                                                     vertex.label.color=col_help,
                                                     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                                                     vertex.label.font=2)
                    #plot(net.sp,
                    #     vertex.label.dist=dist_help,
                    #     vertex.size=point_size2,edge.width=edge_size2_p/(max(edge_size2/20)),margin=c(0,0,0,0),
                    #     vertex.label.cex=2,layout=test.layout2,vertex.color=colors[staying[,3]+1],
                    #     vertex.label.color=col_help,
                    #     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                    #     vertex.label.font=2,add = T,edge.color=edge.color.helper)
                    }
                                           ,res=50,width = 1100,height=1100)
            }
            if(input$relativeColor=="relative"){
                colors<-colorRampPalette(c("gold","darkred"))(max(staying[,4])+1)
                col_help<-rep("black",length(point_size2))
                col_help[staying[,4]>=0.8*(max(staying[,4]+1))&dist_help==0]<-"white"
                output$plot1 <- renderPlot({plot(net.sp,
                                                     vertex.label.dist=dist_help,
                                                     vertex.size=point_size2,edge.width=edge_size2/(max(edge_size2/20)),margin=c(0,0,0,0),
                                                     vertex.label.cex=2,layout=test.layout2,vertex.color=colors[round(staying[,4],digits = 0)+1],
                                                     vertex.label.color=col_help,
                                                     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                                                     vertex.label.font=2,edge.color="grey65")
                    #plot(net.sp,
                    #     vertex.label.dist=dist_help,
                    #     vertex.size=point_size2,edge.width=edge_size2_p/(max(edge_size2/20)),margin=c(0,0,0,0),
                    #     vertex.label.cex=2,layout=test.layout2,vertex.color=colors[round(staying[,4],digits=0)+1],
                    #     vertex.label.color=col_help,
                    #     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                    #     vertex.label.font=2,add = T,edge.color=edge.color.helper)
                    }
                                           ,res=50,width = 1100,height=1100)
            }
        }

        shinyjs::html("text", paste0("<br>Analysis successfull!<br><br>"), add = TRUE)
        shinyjs::html("text", paste0("<center><p style=\"color:#FF0000\";>Anonymous data<br><br></p>"), add = TRUE)
    })
    
    #Analysis
    observeEvent(input$do_demo,{
        shinyjs::html("text", paste0("<br>Reading input data<br>"), add = FALSE)
        staying<-demo_table
        net.sp<-demo_net
        point_size2<-demo_points
        edge_size2<-demo_edge
        
        if(input$shuffle_d=="Yes"){
            test.layout2_d <- layout_(net.sp,with_dh(weight.edge.lengths = edge_density(net.sp)/400))
            helper<-reactive({T})
        }
        message(helper())

        if(input$progredient_d=="Yes"){
            if(input$relativeColor_d=="absolute"){
                colors<-colorRampPalette(c("gold","darkred"))(max(staying[,3])+1)
                output$plot1 <- renderPlot({plot(net.sp,
                                                     vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
                                                     vertex.size=point_size2,edge.width=edge_size2/1.5,margin=c(0,0,0,0),
                                                     vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[staying[,3]+1],
                                                     vertex.label.color=c(rep("black",7),"white",rep("black",9)),
                                                     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                                                     vertex.label.font=2,edge.color="grey65")
                    #plot(net.sp,
                    #     vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
                    #     vertex.size=point_size2,edge.width=edge_size2_dp/1.5,margin=c(0,0,0,0),
                    #     vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[staying[,3]+1],
                    #     vertex.label.color=c(rep("black",7),"white",rep("black",9)),
                    #     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                    #     vertex.label.font=2,add = T,edge.color=edge.color.helper_d)
                    }
                                           ,res=50,width = 1100,height=1100)   
            }
            if(input$relativeColor_d=="relative"){
                colors<-colorRampPalette(c("gold","darkred"))(max(staying[,4])+1)
                output$plot1 <- renderPlot({plot(net.sp,
                                                     vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
                                                     vertex.size=point_size2,edge.width=edge_size2/1.5,margin=c(0,0,0,0),
                                                     vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[round(staying[,4],digits=0)+1],
                                                     vertex.label.color="black",
                                                     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                                                     vertex.label.font=2,edge.color="grey65")
                    #plot(net.sp,
                    #     vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
                    #     vertex.size=point_size2,edge.width=edge_size2_dp/1.5,margin=c(0,0,0,0),
                    #     vertex.label.cex=2,layout=test.layout2_d,vertex.color=colors[round(staying[,4],digits=0)+1],
                    #     vertex.label.color="black",
                    #     vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                    #     vertex.label.font=2,add = T,edge.color=edge.color.helper_d)
                    }
                                           ,res=50,width = 1100,height=1100)   
            }
        }
        if(input$progredient_d=="No"){
            output$plot1 <- renderPlot(expr=plot(net.sp,
                                                 vertex.label.dist=c(0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,1),
                                                 vertex.size=point_size2,edge.width=edge_size2/1.5,margin=c(0,0,0,0),
                                                 vertex.label.cex=2,layout=test.layout2_d,
                                                 vertex.label.color="black",
                                                 vertex.label=paste0(staying[,1],"\n",staying[,2]," (",staying[,3],")",sep=""),
                                                 vertex.label.font=2)
                                       ,res=50,width = 1100,height=1100)   
        }

        shinyjs::html("text", paste0("<br>Analysis successfull!<br><br>"), add = TRUE)
        shinyjs::html("text", paste0("<center><p style=\"color:#FF0000\";>Anonymous data<br><br></p>"), add = TRUE)
    })

})
