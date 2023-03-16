

library(shiny)
library(RMeCab)
library(tidyverse)
library(dplyr)
library(igraph)
library(DT)
library(magrittr)

tab_memo <- rbind(("読み込みデータ(検索)"),("単語表(頻度・検索)"),("共起ネットワーク"))

shinyServer(function(input, output) {
  
  observeEvent(input$file,{        #ファイルの読込み
    DF <- reactive(read.csv(input$file$datapath,header=T, fileEncoding="SJIS"))
    DFaft <-as.data.frame(DF())
    #DFdf <- as.data.frame(DFaft)#　行番号振りたい。

    colID <- (as.data.frame(as.matrix(t(DFaft))))
    colID <- as.data.frame(colnames(colID))
    IDres <- str_replace_all(colID$'colnames(colID)', "V", "")
    IDres <- as.data.frame(as.integer(IDres))
    #ID <- set_colnames(IDres,"ID")
    col_names <- c("ID","text")
    input_data  <- set_colnames(cbind(IDres,DFaft$txt),col_names)
    input_data
    
    temp <- tempfile()　　　　　
    DF() %>% 
      pull("txt") %>%
      write(file=temp)   
    
    DFaa <- RMeCabFreq(temp)
    DFaa <- DFaa[!(DFaa$Info1=="記号"
                             | DFaa$Info1=="助詞"
                             | DFaa$Info1=="接頭語"
                             | DFaa$Info1=="連体詞"
                             | DFaa$Info1=="接頭詞"
                             | DFaa$Info2=="数"
                             | DFaa$Info2=="接尾"
                             | DFaa$Info2=="*"
                             | DFaa$Info2=="非自立"
                             | DFaa$Term==":"
                             | DFaa$Term=="("
                             | DFaa$Term=="（"
                             | DFaa$Term=="/"
                   　　　　　| DFaa$Term=="）"
                             | DFaa$Term==")"
                             | DFaa$Term=="-"
                             | DFaa$Term==","
                             | DFaa$Term=="~"
                             | DFaa$Term=="?"
                             | DFaa$Term=="."
                             | DFaa$Term=="～"
                             | DFaa$Term==","
                             | DFaa$Term=="<"
                             | DFaa$Term==">"
                             | DFaa$Term=="+"
                             | DFaa$Term=="!"
                             | DFaa$Term=="/"
                             | DFaa$Term=="~"
    ),]
    
    DFaa <- arrange(DFaa,-DFaa$Freq)

    # 多変量解析用行列の作成
    DFT <- as.data.frame(t(DFaft))
    rest  <- (docMatrixDF(DFT
                          #,pos = c("動詞","形容詞","形容動詞","副詞","名詞"),
    ))
    
    DFmatrix <- as.data.frame(t(rest))
    
  
    output$DFmatrix <- DT::renderDataTable(DFaa)
    
    
    
    ngram <- NgramDF(temp,
                     type = 1,#単語の共起語
                     N=2,#2単語を繋げる
                     pos=c("名詞","動詞","形容詞","形容動詞"))
    
    ngram <- ngram[!(ngram$Ngram1=="。"
                         | ngram$Ngram2=="。"
                         | ngram$Ngram1=="、"
                         | ngram$Ngram2=="、"
                         | ngram$Ngram1==","
                         | ngram$Ngram2==","
                         | ngram$Ngram1=="「"
                         | ngram$Ngram2=="「"
                         | ngram$Ngram1=="」"
                         | ngram$Ngram2=="」"
                         | ngram$Ngram1=="《"
                         | ngram$Ngram2=="《"
                         | ngram$Ngram1=="》"
                         | ngram$Ngram2=="》"
                         | ngram$Ngram1=="/"
                         | ngram$Ngram2=="/"
                         | ngram$Ngram1=="~"
                         | ngram$Ngram2=="~"
                     　　| ngram$Ngram1=="（"
                     　　| ngram$Ngram2=="（"
                     　　| ngram$Ngram1=="）"
                     　　| ngram$Ngram2=="）"
    ),]
    
    
    Nmin_size <-(input$Nmin)
    
 #=====================================================clustA START
    
    ngram_clust <- filter(ngram,ngram$Freq>=Nmin_size)
    clustOBJ <- cluster_fast_greedy(as.undirected(graph.data.frame(ngram_clust)))
    
    output$dendroA <- renderPlot({
    
    dendPlot(clustOBJ,mode = "hclust")
    
    })
 #=====================================================clustA END
    
 # ===================================================clustB START
    
#    DFT <- as.data.frame(t(DFaft))
#    rest  <- (docMatrixDF(DFT
#                          #,pos = c("動詞","形容詞","形容動詞","副詞","名詞"),
#    ))
#    
#    DFmatrix <- as.data.frame(t(rest))
#    
#    DFmatrix
#    
#    
#    
#    filter(DFmatrix,)
#    
#    C <- ncol(DFmatrix)
#    
#    Freq  <- as.data.frame(colSums(DFmatrix))
#    
#    Freq  <- as.data.frame(t(set_colnames(Freq,"Freq")))
#    
#    Freq   
#    
#    DFmat <- rbind(DFmatrix,Freq)
#    DFmat <- as.data.frame(t(DFmat)) 
#    
#    min_size <- 5
    
#    DFmat <- filter(DFmat,Freq>=min_size)
    
    
#    #"ward.D" method
#    Xd <- dist(DFmat, method="euclidean")
#    h <- hclust(Xd, method = "ward.D")
    
#    #図示
#    par(family="systemfonts", 
#        mgp=c(2.5, 1, 0), xpd=T,
#        mar=c(2, 5, 5, 1))
#    #  plot(h, hang=-1, cex=0.1, xlab = "", sub = "")
#  output$dendro  <- renderPlot({ plot(h, hang=-1, cex=1, xlab = "", sub = "")
#   })
    #### ============================================　clustB END
    
    ngram_clust <- leading.eigenvector.community(graph.data.frame(ngram))
    pal1 <- rainbow(10, alpha=.3)
    pal2 <- rainbow(10, alpha=.6)
    
    
    output$input_data <- DT::renderDataTable(input_data)
    
    #output$table <- renderTable({
    #  input_data
    #})
    
    
    cfg <- cluster_fast_greedy(as.undirected(graph.data.frame(filter(ngram,Freq>=Nmin_size))))
    net <- graph.data.frame(filter(ngram,Freq>=Nmin_size),directed = F)
    V(net)$community <- cfg$membership
    
    
    
    output$downloadData = downloadHandler(
      filename = "Sentence × Word-Vector.csv",
      content = function(file) {
        write.csv(DFmatrix,file,fileEncoding = "cp932")
      })
    
    
    
    output$Coocnet <- renderPlot({
      
#      plot(graph.data.frame(filter(ngram,Freq >= Nmin_size ),directed = F),
#           vertex.size=(((DFaa$Freq)^(1/2))*(mean((DFaa$Freq)^(1/2)))*2),               #ノードのサイズ
#           vertex.label.font=1, 　　　　 #ラベルのフォントの調整
#           vertex.label.color="black",　 #ラベルの色の調整
#           vertex.label.cex=(mean((DFaa$Freq)^(1/2))),　　　   #ラベルの大きさの調整
#           vertex.frame.color=pal2[V(net)$community], #ノードの枠の色  "#00ced1"
#           vertex.color= pal1[V(net)$community],　　　 #ノードの色  "#afeeee"  or ngram_clust$membership
#           family="HiraKakuPro-W3",      #日本語を表示させる
#           vertex.label.dist=0 ,          #文字とノードをズラす
#           edge.curved = FALSE,
#           edge.color = "#a9a9a9",        #edgeの色を調整 a9a9a9 c0c0c0
#          # vertex.label.font = 5   # 1 プレーン, 2 ボールド, 3, イタリック, 4 ボールドイタリック, 5 シンボル
#          # vertex.label.family = "Times"    # "Times", "Helvetica"
#          frame=TRUE  ,                  # プロットに枠を付けるかどうか。
#　　　　　#edge.width=E(g)$weight,       # object g not found
#　　　　　layout=layout.fruchterman.reingold
#                           )
      
            plot(graph.data.frame(filter(ngram,Freq >= Nmin_size ),directed = F),
                 vertex.size=(((sqrt(DFaa$Freq)))/((sqrt(max(DFaa$Freq)))))*30,               #ノードのサイズ
                 vertex.label.font=1, 　　　　 #ラベルのフォントの調整
                 vertex.label.color="black",　 #ラベルの色の調整
                 #vertex.label.cex=,　　　   #ラベルの大きさの調整
                 vertex.frame.color="#a9a9a9", #ノードの枠の色  "#00ced1"
                 vertex.color= pal1[V(net)$community],　　　 #ノードの色  "#afeeee"  or ngram_clust$membership
                 family="systemfonts",      #日本語を表示させる
                 vertex.label.dist=0 ,          #文字とノードをズラす
                 edge.curved = FALSE,
                 edge.color = "#a9a9a9",        #edgeの色を調整 a9a9a9 c0c0c0
                # vertex.label.font = 5   # 1 プレーン, 2 ボールド, 3, イタリック, 4 ボールドイタリック, 5 シンボル
                # vertex.label.family = "Times"    # "Times", "Helvetica"
                frame=TRUE  ,                  # プロットに枠を付けるかどうか。
      　　　　　#edge.width=E(g)$weight,       # object g not found
      　　　　　layout=layout.fruchterman.reingold
                                 )
      
      
    })
  })
})




