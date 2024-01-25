
library(shiny)
library(ggplot2)
library(fontawesome)
library(shinythemes)
library(pacman)
pacman::p_load(tidyverse,DT,DataExplorer)
library(dplyr)
library(gridExtra)
library(scales)


  
  data <- read.csv("双十一剁手数据.csv",header = T,fileEncoding = "GBK",na.strings = "(跳过)")
  
  show <- c("购物与否" = "1",
            "预期花费" = "2",
            "实际花费" = "3",
            "购买品类" = "4",
            "为谁购物" = "5",
            "在哪购物" = "6",
            "是否为折扣参与转发活动" = "7",
            "是否使用花呗、白条或信用卡支付" = "8",
            "对双十一的态度" = "9")
  
  nm <- c("购物与否","预期花费","实际花费","购买品类","为谁购物","在哪购物",
          "参与转发活动","是否提前消费","对双十一的态度","性别","年龄","职业","收入","地区")
  
  t1 <- c("由图可见绝大多数受调查者在双十一期间有过购物行为，体现了购物节的受欢迎程度。",
          "半数受调查者的预期消费在500~2000元之间，三分之一的受调查者预期消费不超过500元，其余绝大多数人群的预期消费在2000至5000元之间，预期消费超过5000元的人群占极少数。",
          "与预期消费作对比，可以看到总体上受调查者的实际花费较预期花费更高，只有四分之一的人群实际消费不超过500元，四成人群实际消费在500~2000元，同时高消费人群比例明显变大。考虑到绝大多数受调查者为没有稳定收入来源的学生，这些消费数额已经十分可观，体现了购物节促销手段的成功。",
          "各有四分之一左右的受调查者选择了服饰和美妆个护作为消费对象，这与受调查者主要为学生和女性的信息相符，剩余绝大多数受调查者选择购买家用电器，食品，图书之类的生活必需品。",
          "大多数受调查者选择为自己购物，这可能是由于调查群体主要是没有稳定收入的学生，其经济来源是其家人。",
          "淘宝是受调查者的主要购物平台，除淘宝之外京东也占有近五分之一的份额，而传统的商城购物模式并不受欢迎，只能与其余的线上平台划分剩余的市场。这可能是由于受调查的主体学生平时较忙无暇去线下购物，同时也能熟练使用线上购物软件。",
          "小部分受调查者为了优惠而参加了转发活动，这一比例约为40%，可能与商家的优惠力度及商家的知名度有关。",
          "大多数受调查者在购物中使用了透支消费手段，这可能与受调查者预期消费与实际消费之间的差距较大，以及主要调查群体收入有限相关。",
          "接近半数的受采访者对购物节持赞成态度，只有极小部分受调查者认为自己有过度消费",
          "调查对象中女性占约七成比例，可能与女性消费热情较高有关。",
          "调查对象的年龄绝大多数处于18~25岁的阶段，可以看出这一年龄段的青年人群是消费者的主体，有较强的消费能力与消费欲望。",
          "绝大多数受调查者为全日制学生，这与受调查者的年龄发布有关，体现了学生群体在购物活动中的高参与度，也有可能是因为问卷主要在校园范围内发布。",
          "大多数受调查者收入不超过3000元，绝大多数不超过5000元。这是由于受调查的大量学生群体没有稳定的收入来源。",
          "接近半数的收调查者来自北京，其余零星分布于全国的近30个省级行政区，这可能是由于问卷主要在北京发放，同时经济发达地区也相对更容易获得更多的数据。
考虑到部分省级行政区的数据太少，误差较大，不具有统计和分析的价值，故对于那些数据量小于10的省级行政区数据不作考察。")
  
  t2 <- c("相比之下，男性的购物率要低于女性，可能与女性更强烈的购物欲望有关。",
          "在考虑自己的预期消费时，更多的男性会选择低消费（500元以下）或者是高消费（5000元以上），而更多的女性则倾向于适中的消费金额（500元~5000元）",
          "相比预期消费的分布，女性的现实消费总体上要更高一些，从消费超过500元的比例到消费超过10000元的比例都有所上升；男性的现实消费在大多数情况下也都超过了预期消费，但消费超过5000元的比例由10.6%降低到了7.9%，是这张图中唯一的反例。现实中由于一些商家的营销策略，人们的消费往往会超出自己的预期，这一点或许对女性更为明显。",
          "男性与女性的购物品类构成大体上相似，其中更多男性会关注电脑、手机数码等电子产品，而更多女性会关注美妆个护、家居用品。",
          "男性与女性在为谁购物这方面的选择也大致相似，男性和女性近六成的购物行为都是为了自己，相比之下男性更倾向于为伴侣购物，而女性则更倾向于为父母购物。这些细微区别可能与文化习俗有关。",
          "淘宝是男性和女性大部分购物行为的渠道，淘宝之外的市场大部分属于京东。相对而言女性受调查者比男性更倾向于用淘宝购物，而相比女性更多的男性受调查者会选择在京东上购物。京东与淘宝的不同特点反映了男女的购物习惯差异。",
          "四分之三的男性和超过半数的女性拒绝为了折扣参加转发活动，但考虑到消费者基数庞大，仍有大量消费者选择了参与转发，体现了这一活动的有效性，相比较而言转发活动对女性来说更具吸引力。",
          "超过半数的男性和女性都在双十一中选择了透支消费，其中女性的比例较高，这也支持了前文中男女性实际消费均超过预期消费，且女性的额外消费现象更显著这一观点。",
          "男性与女性对待双十一的态度都是以支持为主，相比较而言更多男性保留了中立意见（与我无关），而更多的女性表达了对购物节的支持或是过度消费的后悔。")

  t3 <- c("年龄是影响购物与否的重要因素，不超过18岁的受调查者购物率仅60%，远远低于其他年龄段，可能是由于未成年人缺乏稳定收入且学业繁忙；成年群体中18~35岁的青壮年购物率显著超过了36岁以上的中年，可能是年轻人对于购物节这一概念更为追捧，也有更多的购物需求。",
          "由图可见，预期消费与受调查者的年龄息息相关，18岁以下的未成年人由于缺乏收入来源基本不会为购物节准备超过2000的金额；18~35岁的青壮年随着储蓄和收入的增加，可以为购物节准备更多的资金；36至40岁的中年人预期消费较低，可能是购物热情下降，对于购物节的概念并不了解；40岁以上的受调查者整体预期消费较低，但有11%的受调查者会考虑投入10000元以上的资金。",
          "实际消费随年龄段而变化，大体上呈现36岁前消费逐步增加，36岁后消费逐步减少的趋势。与预期消费作对比，可以发现年轻人的消费习惯更激进，而中年人的购物习惯则更加稳妥。",
          "总体上，在年龄较高的受调查者购物记录中，图书文娱等满足精神需求的物品占比较高，而服饰这类日常生活中的物质必需品占比较低。原因可能是成年人随着年龄增长，有了更多的积蓄与更富足的物质生活，便对精神生活有更高的关注。未成年人的购物选择较为平均，可能与学习和学校的要求有关。",
          "总体上随着年龄的增长，人们更倾向于为他人而不是自己购物，这种现象可能与不同年龄段的心态、收入与储蓄有关。由前分析可知年长的受调查者普遍有更好的经济条件，也自然能更多的为他人考虑。",
          "由图中可见，18岁以下未成年人的购物全部通过淘宝进行，可能是未成年人学业繁忙及数据量较小，误差较大导致；随着受调查者年龄的增大，淘宝的占比逐渐降低，而较为传统的渠道占比有上升趋势，这一现象反映了淘宝这一较新渠道对市场格局的影响。可以很明显的看出淘宝更符合年轻人的喜好，在未来有可能会进一步扩大市场占比。",
          "相比之下，年轻人更热衷于通过转发活动来获得商家的优惠，30岁以上的中年人对这类活动热情降低，40岁以上的受调查者则完全不参与此类活动（推测是由于40岁以上受调查者样本量小导致的）。特殊的是36·40岁的中年人对转发活动热情较高，可能是受到了其子女的影响。",
          "18岁以下的未成年人完全没有透支消费的习惯，考虑到他们较低的消费金额与不能办理信用卡这一限制，这一现象是合理的。成年人中18~25岁的群体由于消费金额较低等缘故使用透支消费频率偏低，而40岁以上的受调查者可能是由于老一辈的购物习惯很少使用透支消费，其余26岁至40岁的受调查者透支消费的概率都超过80%。",
          "未成年人和35岁以上的中年人对双十一更多地持积极态度，且没有因过度消费而产生后悔的心理；18~35岁的青壮人群体对购物节的支持度偏低，且有部分认为自己存在过度消费。这一现象可能与年轻人较为激进的购物习惯有关（这一点在预期花费与实际花费的对比中有所体现）。")
  
  t4 <- c("由图可见，不同收入群体的购物率都在90%左右，可见购物率这一数据主要由人们的需求所决定，与收入关系不大。",
          "总体上随着收入的增加，受调查者的预期花费也稳步提升，因为其有了更多的可支配收入与更高的生活追求。这也与我们的直觉相符。",
          "与预期花费相似，受调查者的实际花费也随收入的增加而有所提升，图中反常的点在于月收入20000元的受调查者有近三成选择了不超过500元的消费金额，可能是其生活已经相对富足，希望将钱投入到其它用途。",
          "收入不超过5000元的受调查者中绝大多数都是学生群体，他们相对而言更多地购买服饰、美妆等穿搭、化妆用品，而在收入超过5000元的人群中，收入较高者普遍有更多的可支配收入可以投入到图书文娱等享受资料上，而收入较低者则在购物节中更关注服饰等基础的生存资料。",
          "由图可见，收入越高的受调查者更倾向于为他人购物，收入超过两万的受调查者甚至有相当一部分购物是为了宠物，这是因为他们有更充裕的可支配收入，而收入较低者则不得不先用有限的金钱优先满足自己的需求。",
          "总体上收入越高的受调查者越倾向于在淘宝之外的平台购物，淘宝之外，大多数受调查者都选择了京东作为购物渠道。淘宝和京东在不同收入群体中的占比此消彼长，可能是因为淘宝的微商概念迎合了收入较低的人群而无法满足高收入群体的需求，此时更正规，高端的京东就成了优秀的替代品。",
          "总体上而言随着收入的增加，受调查者对于转发优惠活动的热情度在降低，因为他们有更多的可支配收入而不用在乎少量的优惠。特殊的是收入在5000~10000的中等收入群体对待这类活动热情较高，这可能是低收入群体以未成年人为主，有家人作为经济支撑，而中等收入群体则处于收入有限而生活开支较高，亟需省钱的境地中，故对于优惠活动的热情较高。",
          "由图可见，低收入（5000元以下）的群体由于学生与未成年人较多，使用透支消费的比例较低；而高收入（20000元以上）的受调查者由于储蓄足够也较少选择透支消费；收入介于500元到20000元的受调查者由于并不充裕的储蓄和较高的生活开支，成为了透支消费的主力。",
          "由图可见，收入较高（10000元以上）的人群对于购物节更为支持，而收入较低的受调查者则有不同幅度的对于过度消费的后悔心理。原因可能是更少的可支配收入迫使低收入者省吃俭用，此时双十一期间的过度消费就更容易使人萌生后悔情绪。")
  
  #在数据预处理中将复杂的列名转化为简单的列名，方便接下来的引用，
  #同时用"strsplit()"函数将Q4、Q5、Q6和Q14列数据中的不同元素分割开来，方便之后的统计处理
  colnames(data) <- paste0("Q",1:ncol(data)) #将复杂的列名换成简单的，方便之后引用
  data$Q4 <- strsplit(data$Q4,split = "┋")
  data$Q5 <- strsplit(data$Q5,split = "┋")
  data$Q6 <- strsplit(data$Q6,split = "┋")
  data$Q14 <- gsub("^(.*?)-.*","\\1",data$Q14)
  
  #数据类型转换
  data$Q1 <- as.factor(data$Q1)
  data$Q2 <- factor(data$Q2,levels = c("500元以下","500~2000元","2000~5000元","5000~10000元","10000元以上"))
  data$Q3 <- factor(data$Q3,levels = c("500元以下","500~2000元","2000~5000元","5000~10000元","10000元以上"))
  data$Q7 <- as.factor(data$Q7)
  data$Q8 <- as.factor(data$Q8)
  data$Q10 <- as.factor(data$Q10)
  data$Q11 <- factor(data$Q11,levels = c("18岁以下","18~25岁","26~30岁","31~35岁","36~40岁","40岁以上"))
  data$Q12 <- as.factor(data$Q12)
  data$Q13 <- factor(data$Q13,levels = c("3000元以下","3000~5000元","5000~10000元","10000~20000元","20000元以上"))
  
  
  #筛选出特定分析需要用到的数据
  data2 <- na.omit(data)
  data5 <- data2
  data5$Q4 <- lapply(data2$Q4, function(x) gsub(".*〖.*","其他",x))
  data5$Q5 <- lapply(data2$Q5, function(x) gsub(".*〖.*","其他",x))
  data5$Q6 <- lapply(data2$Q6, function(x) gsub(".*〖.*","其他",x))
  data5$Q9 <- lapply(data2$Q9, function(x) gsub(".*〖.*","其他",x))
  
  #定义需要用到的函数
  pie0 <- function(d,k,i){
    colname <- colnames(d)[i]
    count <- table(d[i])
    count <- data.frame(count)
    colnames(count)[1] <- "Var1"
    count$Freq <- count$Freq/(sum(count$Freq))*100
    ggplot(data = data.frame(count),aes(x = "",y = Freq,fill = Var1)) + geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0) + labs(title = paste("Pie chart",i),x = NULL, y = NULL, fill = as.character(k[i])) 
  }
  pie1 <- function(d,k,i){
    colname <- colnames(d)[i]
    count <- table(d[i])
    count <- data.frame(count)
    colnames(count)[1] <- "Var1"
    count$Freq <- count$Freq/(sum(count$Freq))*100
    ggplot(data = data.frame(count),aes(x = "",y = Freq,fill = Var1)) + geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0) + labs(title = "饼图",x = NULL, y = NULL, fill = as.character(k[i])) + scale_fill_brewer() 
  }
  
  pie2 <- function(d,k,i){
    colname <- colnames(d)[i]
    count <- data.frame(table(unlist(d[,i])))
    combine <- grepl(".*〖.*",count$Var1)
    new_row <- data.frame(Var1 = "其他",Freq = sum(count$Freq[combine]))
    count <- count[!combine,]
    count <- rbind(count,new_row)
    count$Freq <- count$Freq/(sum(count$Freq))*100
    ggplot(data = na.omit(count),aes(x = "",y = Freq,fill = Var1))  + geom_bar(stat = "identity") + coord_polar(theta = "y",start = 0)  + labs(title = "饼图",x = NULL, y = NULL, fill = as.character(k[i]))  + scale_fill_brewer() 
  }
  
  relation1 <- function(d,i,t,k){
    col1 <- as.numeric(str_extract(i, '\\d+'))
    col2 <- as.numeric(str_extract(t, '\\d+'))
    plotdata <- d %>% 
      group_by(!!sym(i), !!sym(t)) %>%
      summarize(n = n(),.groups = 'drop_last') %>% 
      mutate(pct = n/sum(n),
             lbl = scales::percent(pct)) 
    ggplot(plotdata,aes(x = !!sym(i),     
                        y = pct,                                                
                        fill = !!sym(t))) + 
      geom_bar(stat = "identity",                                     
               position = "fill") +                               
      scale_y_continuous(breaks = seq(0, 1, .2), 
                         label = scales::percent) +
      geom_text(aes(label = lbl),                                     
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer() +
      labs(y = "Percent", 
           fill = k[col2],
           x = k[col1],
           title = "比例图") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
    relation2 <- function(d,col1, col2,k) {
      i <- str_extract(col2, '\\d+')
      t <- str_extract(col1, '\\d+')
      plotdata <- d %>%
        tidyr::unnest(!!sym(col1)) %>%
        group_by(!!sym(col2), !!sym(col1)) %>%
        summarise(n = n(),.groups = "drop_last") %>%
        mutate(pct = n/sum(n),
               lbl = scales::percent(pct)) %>%
        ggplot(aes(x = !!sym(col2), y = pct, fill = !!sym(col1))) +
        geom_bar(stat = "identity",                                     
                 position = "fill") +                                   
        scale_y_continuous(breaks = seq(0, 1, .2), 
                           label = scales::percent) +
        geom_text(aes(label = lbl),                                     
                  size = 3, 
                  position = position_stack(vjust = 0.5)) +
        scale_fill_brewer() +
        labs(y = "Percent", 
             fill = k[as.numeric(t)],
             x = k[as.numeric(i)],
             title = "比例图") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
  ui <- navbarPage(
     title = "双十一剁手数据分析",
     theme = shinytheme("lumen"),
     tags$style("
        .nav-tabs {
          display: none;
        }
      "),
     tabsetPanel(
      id = "main",
          tabPanel(
            title = "首页",
            value = "tab",
            fluidPage(
              sidebarLayout( 
                position = "right",
                sidebarPanel(
                  div(class = "small text-muted text-center",p("点击按钮跳转")),
                  div(style = "display: flex; flex-direction: column;align-items:center;line-height: 0.7em;",
                      actionButton("mainbtn0",
                                "数据初看",
                                icon = icon("table"),
                                class = "btn-primary btn-md"
                      ),
                      
                      tags$br(),
                      
                      actionButton("mainbtn1",
                               "基于性别",
                               icon = icon("mars"),
                               class = "btn-primary btn-md"
                               ),
                  
                      tags$br(),
                  
                      actionButton("mainbtn2",
                               "基于年龄",
                               icon = icon("hourglass-half"),
                               class = "btn-primary btn-md"
                               ),
                      
                      tags$br(),
                      
                      actionButton("mainbtn3",
                                "基于收入",
                                icon = icon("dollar-sign"),
                                class = "btn-primary btn-md"   
                                ),
                  )
                ),
                mainPanel(
                  tags$img(src = "bg.jpeg", 
                           style = "width: 100%; height: 100%; object-fit: cover;")
                )
              )
            )
          ),
      
          tabPanel(title = "数据初看", 
                   value = "tab1",
                   fluidPage(sidebarLayout(
                     sidebarPanel(
                       fluidRow(
                         column(4,
                                actionButton("back1",
                                             "返回首页",
                                             icon = icon("arrow-left"),
                                             class = "btn-primary btn-xs"),
                         )
                       ),
                       br(),
                                selectInput("show0","选择数据",c("调查对象信息" = "1","总体调查结果" = "2")),
                                uiOutput("chose"),
                                checkboxGroupInput("check0",
                                                   "选择想要的呈现方式",
                                                   c("图表分析","文字分析"),
                                                   selected = c("图表分析","文字分析"),
                                                   inline = T)
                      
                       
                     ),
                     mainPanel(
                       tabsetPanel(id = "page1",
                                   tabPanel(
                                     title = "调查对象信息",
                                     value = "panel1_1", 
                                     uiOutput("panel1im"),
                                     uiOutput("panel1te")
                                   ),
                                   tabPanel(
                                     title = "总体调查信息", 
                                     value = "panel1_2", 
                                     uiOutput("panel2im"),
                                     uiOutput("panel2te")
                                   )
                                   
                       ),
                       br(),
                       
                     )
                   )
                   )
          ),
      
          tabPanel(title = "基于性别", 
                 value = "tab2",
                 fluidPage(sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(4,
                              actionButton("back2",
                                           "返回首页",
                                           icon = icon("arrow-left"),
                                           class = "btn-primary btn-xs"),
                       )
                     ),
                     br(),
                     selectInput("show1","选择您关心的数据",show),
                     checkboxGroupInput("check1",
                                        "选择想要的呈现方式",
                                        c("图表分析","文字分析"),
                                        selected = c("图表分析","文字分析"),
                                        inline = T)
                   ),
                   mainPanel(
                     tabsetPanel(id = "page2",
                                 tabPanel(
                                   title = "调查对象信息",
                                   value = "panel2", 
                                   uiOutput("panel3im"),
                                   uiOutput("panel3te")
                                 )
                     ),
                     br(),
                     
                   )
                 )
                 )
        ),
      
        tabPanel(title = "基于年龄", 
                 value = "tab3",
                 fluidPage(sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(4,
                              actionButton("back3",
                                           "返回首页",
                                           icon = icon("arrow-left"),
                                           class = "btn-primary btn-xs"),
                       )
                     ),
                     br(),
                     selectInput("show2","选择您关心的数据",show),
                     checkboxGroupInput("check2",
                                        "选择想要的呈现方式",
                                        c("图表分析","文字分析"),
                                        selected = c("图表分析","文字分析"),
                                        inline = T)
                   ),
                   mainPanel(
                     tabsetPanel(id = "page3",
                                 tabPanel(
                                   title = "调查对象信息",
                                   value = "panel3", 
                                   uiOutput("panel4im"),
                                   uiOutput("panel4te")
                                 )
                     ),
                     br(),
                     
                   )
                 )
                 )
        ),
      
      
        tabPanel(title = "基于收入", 
                 value = "tab4",
                 fluidPage(sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(4,
                              actionButton("back4",
                                           "返回首页",
                                           icon = icon("arrow-left"),
                                           class = "btn-primary btn-xs"),
                       )
                     ),
                     br(),
                     selectInput("show3","选择您关心的数据",show),
                     checkboxGroupInput("check3",
                                        "选择想要的呈现方式",
                                        c("图表分析","文字分析"),
                                        selected = c("图表分析","文字分析"),
                                        inline = T)
                   ),
                   mainPanel(
                     tabsetPanel(id = "page4",
                                 tabPanel(
                                   title = "调查对象信息",
                                   value = "panel4", 
                                   uiOutput("panel5im"),
                                   uiOutput("panel5te")
                                 )
                     ),
                     br(),
                     
                   )
                 )
                 )
        ),
      
      )
  )
  
  server <- function(input, output, session) {
    
    
        observeEvent(input$mainbtn0, {
          updateTabsetPanel(session, "main",
                            selected = "tab1")
        })
        observeEvent(input$mainbtn1, {
          updateTabsetPanel(session, "main",
                            selected = "tab2")
          })
        observeEvent(input$mainbtn2, {
          updateTabsetPanel(session, "main",
                            selected = "tab3")
        })
        observeEvent(input$mainbtn3, {
          updateTabsetPanel(session, "main",
                            selected = "tab4")
        })
        
        observeEvent(input$show0, {
          updateTabsetPanel(session, "page1",
                            selected = paste0("panel1_", input$show0)
          )
        })
        
        observeEvent(input$back1, {
          updateTabsetPanel(session, "main",
                            selected = "tab")
        })
        
        observeEvent(input$back2, {
          updateTabsetPanel(session, "main",
                            selected = "tab")
        })
        
        observeEvent(input$back3, {
          updateTabsetPanel(session, "main",
                            selected = "tab")
        })
        
        observeEvent(input$back4, {
          updateTabsetPanel(session, "main",
                            selected = "tab")
        })
        
        
        output$chose <- renderUI({
          if(input$show0 == 1){
            radioButtons("c1","选择您关心的特征",c("性别" = 10,"年龄" = 11,"职业" = 12,"收入" = 13,"地区" = 14))
          }else {
            radioButtons("c2","选择您关心的特征",c("购物与否" = "1",
                                           "预期花费" = "2",
                                           "实际花费" = "3",
                                           "购买品类" = "4",
                                           "为谁购物" = "5",
                                           "在哪购物" = "6",
                                           "是否为折扣参与转发活动" = "7",
                                           "是否使用花呗、白条或信用卡支付" = "8",
                                           "对双十一的态度" = "9"))
          }
          
        })
        output$panel1im <- renderUI({
          if ("图表分析" %in% input$check0) {
            plotOutput("image1_1")
          } else {
            NULL
          }
        })
        output$panel1te <- renderUI({
          if ("文字分析" %in% input$check0) {
            textOutput("text1_1")
          } else {
            NULL
          }
        })
        
        output$image1_1 <- renderPlot({
          pie0(data,nm, as.numeric(input$c1))
        })
        output$text1_1 <- renderText({
          t1[as.numeric(input$c1)]
        })
        
        output$panel2im <- renderUI({
          if ("图表分析" %in% input$check0) {
            plotOutput("image1_2")
          } else {
            NULL
          }
        })
        output$panel2te <- renderUI({
          if ("文字分析" %in% input$check0) {
            textOutput("text1_2")
          } else {
            NULL
          }
        })
        
        output$image1_2 <- renderPlot({
          if(as.numeric(input$c2) == 1 ){
            pie1(data,nm,1)
          }else if(as.numeric(input$c2) %in% c(2,3,7,8)){
            pie1(data2,nm,as.numeric(input$c2))
          }else{
            pie2(data2,nm,as.numeric(input$c2))
          }
        })
        output$text1_2 <- renderText({
          t1[as.numeric(input$c2)]
        })
        
        output$panel3im <- renderUI({
          if ("图表分析" %in% input$check1) {
            plotOutput("image2")
          } else {
            NULL
          }
        })
        output$panel3te <- renderUI({
          if ("文字分析" %in% input$check1) {
            textOutput("text2")
          } else {
            NULL
          }
        })
        
        output$image2 <- renderPlot({
          
            if(as.numeric(input$show1) == 1){
              relation1(data,"Q10","Q1",nm)
            }else if(as.numeric(input$show1) %in% c(2,3,7,8)){
              relation1(data5,"Q10",paste0("Q",as.numeric(input$show1)),nm)
            }else{
              print(relation2(data5,paste0("Q",as.numeric(input$show1)),"Q10",nm))
            }
          
        })
        output$text2 <- renderText({
          t2[as.numeric(input$show1)]
        })
        
      ###
        output$panel4im <- renderUI({
          if ("图表分析" %in% input$check2) {
            plotOutput("image3")
          } else {
            NULL
          }
        })
        output$panel4te <- renderUI({
          if ("文字分析" %in% input$check2) {
            textOutput("text3")
          } else {
            NULL
          }
        })
        
        output$image3 <- renderPlot({
          if(as.numeric(input$show2) == 1){
            relation1(data,"Q11","Q1",nm)
          }else if(as.numeric(input$show2) %in% c(2,3,7,8)){
            relation1(data5,"Q11",paste0("Q",as.numeric(input$show2)),nm)
          }else{
            print(relation2(data5,paste0("Q",as.numeric(input$show2)),"Q11",nm))
          }
          
        })
        output$text3 <- renderText({
          t3[as.numeric(input$show2)]
        })
        ###
        output$panel5im <- renderUI({
          if ("图表分析" %in% input$check3) {
            plotOutput("image4")
          } else {
            NULL
          }
        })
        output$panel5te <- renderUI({
          if ("文字分析" %in% input$check3) {
            textOutput("text4")
          } else {
            NULL
          }
        })
        
        output$image4 <- renderPlot({
          if(as.numeric(input$show3) == 1){
            relation1(data,"Q13","Q1",nm)
          }else if(as.numeric(input$show3) %in% c(2,3,7,8)){
            relation1(data5,"Q13",paste0("Q",as.numeric(input$show3)),nm)
          }else{
            print(relation2(data5,paste0("Q",as.numeric(input$show3)),"Q13",nm))
          }
          
        })
        output$text4 <- renderText({
          t4[as.numeric(input$show3)]
        })
        ###
        
        
  }
  
  shinyApp(ui, server)

