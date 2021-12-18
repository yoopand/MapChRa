library(leaflet)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(googledrive)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(rgdal)
library(sp)
library(data.table)
library(formattable)
library(tidyr)
library(treemap)
library(d3treeR)
library(htmltools)

# Load

setwd("C:\\Users\\korawik\\Desktop\\MED-3\\Datviz\\map\\")
## ข้อมูลสุขภาพนักเรียน


dat<-read_excel("data.xlsx")
names(dat)[1]<-"school.name"
names(dat)[6]<-"student.id"
names(dat)[7:9]<-c("eye1","eye2","eye3")
names(dat)[10:12]<-paste("ear",1:3,sep="")
names(dat)[13:15]<-paste("mouth",1:3,sep="")
names(dat)[16:19]<-paste("skin",1:4,sep="")
names(dat)[20:23]<-paste("digest",1:4,sep="")
names(dat)[24:27]<-paste("nutrition",1:4,sep="")
names(dat)[28:31]<-paste("move",1:4,sep="")
names(dat)[32:37]<-paste("emo",1:6,sep="")
names(dat)[38:40]<-paste("hygiene",1:3,sep="")
dat2<-dat%>%mutate(eye=eye1+eye2, eye=ifelse(eye>2,1,0))
dat2<-dat2%>%mutate(ear=ear1+ear2, ear=ifelse(ear>2,1,0))
dat2<-dat2%>%mutate(mouth=mouth1+mouth2, mouth=ifelse(mouth>2,1,0))
dat2<-dat2%>%mutate(skin=skin2+skin3+skin4, skin=ifelse(skin>3,1,0))
dat2<-dat2%>%mutate(digest=digest2+digest3+digest4, digest=ifelse(digest>3,1,0))
dat2<-dat2%>%mutate(nutrition=nutrition2+nutrition3+nutrition4, nutrition=ifelse(nutrition>3,1,0))
dat2<-dat2%>%mutate(move=move2+move3+move4,  move=ifelse(move>3,1,0))

dat2$school.name[dat2$school.name=="โรงเรียนตำรวจตะเวนชายแดนบำรุงที่ 87"]<-"โรงเรียนตชด.ที่ 87"

dat2<-dat2%>%filter(is.na(school.name)==FALSE)%>%
  group_by(school.name)%>%
  summarise(eye=sum(eye),
            ear=sum(ear),
            mouth=sum(mouth),
            skin=sum(skin),
            digest=sum(digest),
            nutrition=sum(nutrition),
            move=sum(move),
            #hygiene=sum(hygiene),
            total=n())
dat2[7,1]<-"โรงเรียนบ้านปางมะหัน"
dat2[8,1]<-"โรงเรียนบ้านพญาไพร"


#dat3<-dat%>%mutate(eye=eye1+eye2, eye=ifelse(eye>2,1,0))
#dat3<-dat3%>%mutate(ear1=ear1, ear=ifelse(ear>1,1,0))
#dat3<-dat3%>%mutate(mouth=mouth1+mouth2, mouth=ifelse(mouth>2,1,0))
#dat3<-dat3%>%mutate(skin=skin2+skin3+skin4, skin=ifelse(skin>3,1,0))
#dat3<-dat3%>%mutate(digest=digest2+digest3+digest4, digest=ifelse(digest>3,1,0))
#dat3<-dat3%>%mutate(nutrition=nutrition2+nutrition3+nutrition4, nutrition=ifelse(nutrition>3,1,0))
#dat3<-dat3%>%mutate(move=move2+move3+move4,  move=ifelse(move>3,1,0))
#dat4<-dat3%>%select(1,7,8,10,11,13,14,17,18,19,21,22,23,25,26,27,29,30,31)
#dat4%>%group_by(school.name)
#dat4<-dat4%>%filter(is.na(school.name)==FALSE)%>%
  #group_by(school.name)%>%
  #summarise(eye=sum(eye),
            #ear=sum(ear),
            #mouth=sum(mouth),
            #skin=sum(skin),
            #digest=sum(digest),
            #nutrition=sum(nutrition),
            #move=sum(move),
            #hygiene=sum(hygiene),
            #total=n())



amphor <- readOGR(dsn = "tha_admbnda_adm2_rtsd_20190221.shp", stringsAsFactors = F)
glimpse(amphor, max.level=2)
amphor@data%>%head()
map<-amphor[amphor$ADM2_TH=="แม่ฟ้าหลวง",]

sch.name<-c("โรงเรียนบ้านห้วยอื้น",
            "โรงเรียนบ้านจะตี",
            "โรงเรียนบ้านผาจี",
            "โรงเรียนบ้านพญาไพร",
            "โรงเรียนพญาไพรไตรมิตร",
            "โรงเรียนสามัคคีพัฒนา",
            "โรงเรียนตชด.ที่ 87",
            "โรงเรียนรัฐราษฎร์วิทยา",
            "โรงเรียนบ้านแม่หม้อ",
            "โรงเรียนบ้านเทอดไทย",
            "โรงเรียนบ้านปางมะหัน")


sch.name2 <- c(rep(sch.name[1],18),
               rep(sch.name[2],18),
               rep(sch.name[3],18),
               rep(sch.name[4],18),
               rep(sch.name[5],18),
               rep(sch.name[6],18),
               rep(sch.name[7],18),
               rep(sch.name[8],18),
               rep(sch.name[9],18),
               rep(sch.name[10],18),
               rep(sch.name[11],18)
)
group <- c(rep(problem[1],2),
           rep(problem[2],2),
           rep(problem[3],2),
           rep(problem[4],3),
           rep(problem[5],3),
           rep(problem[6],3),
           rep(problem[7],3)
)
group<-rep(group,11)

prob<-c("มองกระดานไม่ชัด",
              "ตาแดง ตาอักเสบบ่อย",
              "หูอื้อ/ฟังไม่ชัดเจน",
              "หูอักเสบ/มีหนองไหลจากหู",
              "ปวดฟันบ่อย/ฟันผุ" ,
              "มีแผลในปาก/ร้อนใน" ,
              "มีอาการคัน/มีตุ่ม ผื่น ตกสะเก็ดตามร่างกาย" ,
              "มีแผลเปื่อย หรือมีน้ำเหลือง/หนองตามร่างกาย",
              "มีเหา",
              "ถ่ายลำบาก/พบพยาธิตอนขับถ่าย" ,
              "ปวดท้อง/ท้องอืด/ท้องเฟ้อบ่อย" ,
              "ท้องเสียบ่อย" ,
              "น้ำหนัก/ส่วนสูงต่ำกว่าเกณฑ์" ,
              "ตัวซีด/ตัวเหลือง/ตาเหลือง" ,
              "ผิวหยาบ/ผมร่วง/ผมซีดกว่าปกติ" ,
              "ปวดหลัง/ปวดตัว/ปวดมือบ่อย",
              "เดินกะเปลก" ,
              "ใช้อุปกรณ์ช่วยในการเคลื่อนไหว" )

subgroup<-rep(prob,11)
value<-c(1,	0,	57,	0,	3,	3,	0,	0,	0,	6,	5,	6,	1,	0,  0,	12,	12,	4,
         1,	1,	9,	0,	3,	1,	0,	0,	0,	12,	0,	0,	2,	0,	0,	9,	8,	2,
         2,	0,	45,	0,	1,	0,	0,	2,	4,	27,	4,	1,	1,	2,	3,	8,	2,	10,
         4,	8,	41,	12,	30,	5,	9,	8,	6,	35,	33,	40,	4,	6,	0,	41,	32,	24,
         0,	0,	22,	26,	30,	18,	0,	0,	0,	0,	0,	0,	0,	0,	0,	4,	5,	6,
         2,	0,	30,	25,	7,	9,	1,	0,	0,	15,	9,	13,	4,	2,	0,	12,	18,	19,
         0,	0,	0,	0,	3,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	4,	2,	2,
         1,	1, 28,	13,	5,	2,	2,	1,	0,	6,	9,	9,	2,	3,	0,	12,	9,	8,
         4,	1,	20,	3,	10,	8,	3,	3,	3,	12,	5,	7,	4,	3,	0,	15,	16,	5,
         6,	22,	132,	88,	19,	28,	19,	91,	25,	182,	19,	17,	93,	6,	0,	155,	105,	35,
         0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	12,	5,	4
         )
data <- data.frame(sch.name2,group,subgroup,value)



sch.coord2<-read.csv("school.coord.csv")
#sch.coord2<-merge(sch.coord,sch.name)
names(sch.coord2)[1]<-"lon"


sch.coord2$sch.name<-c("โรงเรียนบ้านห้วยอื้น",
                        "โรงเรียนบ้านจะตี",
                       "โรงเรียนบ้านผาจี",
                        "โรงเรียนบ้านพญาไพร",
                        "โรงเรียนพญาไพรไตรมิตร",
                        "โรงเรียนสามัคคีพัฒนา",
                        "โรงเรียนตชด.ที่ 87",
                        "โรงเรียนรัฐราษฎร์วิทยา",
                        "โรงเรียนบ้านแม่หม้อ",
                        "โรงเรียนบ้านเทอดไทย",
                        "โรงเรียนบ้านปางมะหัน")


sch.location<-SpatialPointsDataFrame(coords=sch.coord2[,2:3], data=sch.coord2)
sch.location@data



problem<-c("การมองเห็น"="eye",
           "การได้ยิน"="ear",
           "ช่องปาก"="mouth",
           "ผิวหนัง"= "skin",
           "ทางเดินอาหาร" = "digest",
           "ทุพโภชนาการ" = "nutrition",
           "การเคลื่อนไหว" = "move")



### ข้อมูล DMC

dmc<-read_excel("stud_basicdat.xlsx")
head(dmc)
glimpse(dmc)
names(dmc)[c(1,2,3,4,5,7,10,11,12)]<-c("school.id","school.name","level","room",
                              "gender","age","nationality","race","religion")

names(dmc)[c(13,14,15,16,17,18,25,26,41)]<-c("old.bro","young.bro","old.sister","young.sister","order",
                                    "fam.status","weight","height","GPAX")

dmc2<-dmc%>%select(1,2,3,4,5,7,10,11,12,13,14,15,16,17,18,25,26,41)

dmc2%>%group_by(school.name)




ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Health-Tracker</a>'), id="nav",
             windowTitle = "MaeFah-Health-Tracker",
             
             #Page1
            
             tabPanel("MF-HealthMap",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")), 
                          leafletOutput("map", width = "100%", height = "100%")),
                      
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 100, left = 55, width = 400, fixed=TRUE,
                                    draggable = TRUE, height = "auto", 
                                    
                                    # span(tags$h5("ผลการสำรวจจากโครงการผลการจัดสรรความช่วยเหลือเพื่อลดปัญหาที่เป็นอุปสรรคต่อการเรียนรู้ของนักเรียน ในโรงเรียนนําร่องจังหวัดเชียงราย"), style="color:#045a8d"),
                                    h4("ผลสำรวจจากโครงการผลการจัดสรรความช่วยเหลือเพื่อลดปัญหาที่เป็นอุปสรรคต่อการเรียนรู้ของนักเรียน ในโรงเรียนนำร่องจังหวัดเชียงราย"),
                                    h5("กองทุนเพื่อความเสมอภาคทางการศึกษาร่วมกับคณะครุศาสตร์จุฬาลงกรณ์มหาวิทยาลัย"),
                                    h5("ตุลาคม พ.ศ.2564"),
                                    h6("โรงเรียนที่เข้าร่วมโครงการ 11 โรงเรียน"),
                                    h6("จำนวนนักเรียนทั้งหมด 4,498 คน")
  
                                    )
                      ,
                      
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    top = 400, left = 55, width = 400, fixed=TRUE,
                                    draggable = TRUE, height = "auto", 
                                    
                                    h3("ปัญหาสุขภาพของนักเรียน"),
                                    
                                    selectInput(inputId="health", 
                                                label="ระบุปัญหาสุขภาพของนักเรียน",
                                                choices = problem),
                                    
                                    plotOutput("healthplot", height="180px", width="100%"),
                                    plotOutput("healthplot2", height="180px", width="100%")
                                    
                                    )
                      
             ),
             
             
             tabPanel("summary table",
                      DT::dataTableOutput('sumtable')
                      
             ),
             
             #Page3
             
             tabPanel("Treemap",
                       sidebarLayout(
                         
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           
                           selectInput(inputId = "sch_selected",
                                       label="เลือกโรงเรียน",
                                       choices = sch.name2,
                                       selected = sch.name2[1])
                         ),      
                         # Main panel for displaying outputs ----
                         mainPanel(
                           
                           # Output:
                           d3tree2Output("tree",height="900px", width="900px")
                           
                         )
                       )
             )
             
  )
)







server <- function(input, output) {
  
  #map 
  student<-data.frame(lon=rnorm(1611,99.65247,0.002), lat=rnorm(1611,20.24846,0.002))
  
    

  output$map <- renderLeaflet({
    
    sch<-sch.location[,c("lon","lat",input$health,"sch.name","total")]
    pal<-c("#FFB99A","#FF6464","#DB3056","#851D41")
    colorpal<-colorBin(palette=pal,sch@data[,3],3)
    
    m = leaflet(map) %>% addProviderTiles("OpenStreetMap",
                                          options = providerTileOptions(minZoom=11, maxZoom=14)) %>% 
      setView(sch.location$lon%>%mean(), sch.location$lat%>%mean(), zoom = 13)%>%
      addPolygons(col="black",weight=2, smoothFactor=0.5, 
                  opacity=0.2, fillOpacity=0.2)%>%
      addCircles(data=sch,lng = ~lon, lat = ~lat, weight = 2,
                 radius = ~sqrt(sch@data[,3])*120, 
                 label = ~sch.name ,
                 #col=~colorpal(sch@data[,3]),
                 labelOptions = labelOptions(noHide=T,textOnly = T,
                                             interactive=T, closeonClick   = T, permanent=TRUE))%>%
      #addCircles(data=student, lng=~lon, lat=~lat,radius=0.005, col="blue", weight=0.01)%>%
      addMarkers(data=sch.location,
                 lng = ~lon, lat = ~lat,
                 popup =  sprintf("<strong>%s</strong> <br/>
                                 จำนวนนักเรียนทั้งหมด %g คน <br/>
                                 มีปัญหา%sจำนวน %g คน",
                                  sch@data[,4],
                                  sch@data[,5],
                                  names(problem[problem==input$health]),
                                  sch@data[,3])%>%lapply(htmltools::HTML))
    
    
    
    
    m %>% setMaxBounds(99.45562, 20.10693, 99.84080, 20.39433)
    
    
  })
  
  output$healthplot<- renderPlot({
    sch<-sch.location[,c("lon","lat",input$health,"sch.name","total")]
    
    health<-data.frame(
      group<-c(input$health, "ไม่มีปัญหา"),
      value<-c(sum(sch@data[,3]), sum(sch@data[,5]-sum(sch@data[,3])))
    ) 
    
    ggplot(health, aes(x="", y=value, fill=group))+
      geom_bar(stat="identity", width=1, color="white")+
      coord_polar("y",start=0)+
      theme_void()+
      ggtitle(sprintf("มีปัญหาจำนวน %g คน", health$value[1])%>%lapply(htmltools::HTML))+
      theme(text=element_text(family='ChulaCharasNew',size=20))+
      labs(fill=sprintf("ปัญหา%s",names(problem[problem==input$health]))%>%lapply(htmltools::HTML))+
      scale_fill_discrete(labels=c("มีปัญหา","ไม่มีปัญหา"))
    
  })
  
  output$healthplot2<-renderPlot({
    
    problem2<-c("มองกระดานไม่ชัด"="eye1",
                "ตาแดง ตาอักเสบบ่อย"="eye2",
                "หูอื้อ/ฟังไม่ชัดเจน"="ear1",
                "หูอักเสบ/มีหนองไหลจากหู"="ear2",
                "ปวดฟันบ่อย/ฟันผุ" = "mouth1",
                "มีแผลในปาก/ร้อนใน" = "mouth2",
                "มีอาการคัน/มีตุ่ม ผื่น ตกสะเก็ดตามร่างกาย" = "skin1",
                "มีแผลเปื่อย หรือมีน้ำเหลือง/หนองตามร่างกาย" = "skin2",
                "มีเหา" = "skin3",
                "ถ่ายลำบาก/พบพยาธิตอนขับถ่าย" = "digest1",
                "ปวดท้อง/ท้องอืด/ท้องเฟ้อบ่อย" = "digest2",
                "ท้องเสียบ่อย" = "digest3",
                "น้ำหนัก/ส่วนสูงต่ำกว่าเกณฑ์" = "nutrition1",
                "ตัวซีด/ตัวเหลือง/ตาเหลือง" = "nutrition2",
                "ผิวหยาบ/ผมร่วง/ผมซีดกว่าปกติ" = "nutrition3",
                "ปวดหลัง/ปวดตัว/ปวดมือบ่อย" = "move1",
                "เดินกะเปลก" = "move2",
                "ใช้อุปกรณ์ช่วยในการเคลื่อนไหว" = "move3",
                "มีกลิ่นตัว/กลิ่นเท้า/กลิ่นปากแรง" = "hygiene1",
                "ประจำเดือนเปรอะเปื้อนตามเสื้อผ้า" = "hygiene2")
    
    
    temp<-dat%>%dplyr::select(contains(input$health))
    # temp<-dat%>%dplyr::select(contains("eye"))
    temp<-ifelse(temp==1,0,1)
    dim<-dim(temp)[2]-1
    freq<-data.frame(group=colnames(temp)[1:dim],
                     value=colSums(temp[,1:dim]))
    ggplot(freq,aes(x=group,y=value))+geom_bar(stat="identity",fill="orange")+
      scale_x_discrete(labels=names(problem2[problem2%>%str_detect(input$health)]))+
      theme_bw()+xlab("")+ylab("จำนวนนักเรียน")+
      theme(text=element_text(family='ChulaCharasNew',size=15))+coord_flip()
    
    
  })
  
  
  #overview Table
  output$sumtable <- DT::renderDataTable(
      DT::datatable(dat2, 
        options = list(pageLength = 11,
        searching = FALSE )
        
        )
  )
  
  
  #Tree map
  output$tree<-renderD3tree2({
    
    
    p<-treemap(data%>%filter(sch.name2==input$sch_selected),
                        force.print.labels = F,
                        border.col = c("black", "white"),
                        border.lwds = c(3,2),
                        index=c("group","subgroup"),
                        vSize="value",
                        type="index",
                        palette = "Set3",    
                        fontsize.labels=c(15,12),
                        title="แผนภาพปัญหาสุขภาพ",
                        fontsize.title=12)
    
    inter <- d3tree2(p,rootname = "แผนภาพปัญหาสุขภาพ")
    
    
    inter
      
    
    
  })
  

  
  
  
  
  
 
}


shinyApp(ui,server)
