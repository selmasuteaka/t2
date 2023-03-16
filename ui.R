
# TF-IDF

library(shiny)
library(RMeCab)
library(tidyverse)
library(dplyr)
library(igraph)
library(DT)



shinyUI(fluidPage(
  
  titlePanel("オリジナルアプリ　KTcoder"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", label = h3("Fileを選択(csv)"),         
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      
      numericInput("Nmin",
                   
                   label = "最小出現数(0~10000)",
                   
                   value = 100,
                   
                   min = 0,
                   
                   max = 10000
                   ),
  
      downloadButton('downloadData', '文章×単語Download')
    ),
    
    
    
    
    
    
    
    mainPanel(
      tabsetPanel(#type = "tabs",
        
        tabPanel("■　はじめに～",
        "使用可能タブ：【読み込みデータ】【単語表】【共起ネットワーク】"
                ),
        
        tabPanel("読み込みデータ",
                dataTableOutput ('input_data')
                 ),
        
        tabPanel("単語表(頻度・検索)",
                dataTableOutput ("DFmatrix")
                 
                 ),
        
        tabPanel( "共起ネットワーク", #共起ネット
          div(("何も出力されない時は最小出現数を下げてからもう一度ファイルを読み込んで下さい。
              出力のたびに位置が変わったりしますが、位置関係＝単語間の関係性の計算結果は変わっていません。
              "),
            plotOutput('Coocnet',height = "100%"),
            style = "height: calc(100vh  - 100px)")
          ),                

        tabPanel( "中心性解析",
        "開発中"
        ),
        
        tabPanel( "Corr_Mat",
                  "開発中。形態素解析したデータに対して無制限に相関係数行列を出力しようとすると、
                  メモリが足りなくなったり、処理に時間がかかりすぎたり、アプリがフリーズしたりする。
                  何らかの外生変数に対して上位ｎ個分までの相関係数列ベクトル出力するような仕様にする予定"
        ),
        
        tabPanel( "デンドログラムA", #共起ネット
                  div(("何も出力されない時は最小出現数を下げてからもう一度ファイルを読み込んで下さい。
              出力のたびに位置が変わったりしますが、位置関係＝単語間の関係性の計算結果は変わっていません。
              "),
                      plotOutput('dendroA',height = "100%"),
                      style = "height: calc(100vh  - 100px)")
                  ),
        
        tabPanel( "PCA(dim2)",
                  "開発中"
        ),
        
        tabPanel("デンドログラムB", 
        #div((
        "動作が遅くなるので解決策を思い付くまでは非公開
              "
        #)
         #,
#                       plotOutput('dendro',height = "100%"),
#                       style = "height: calc(100vh  - 100px)")
        ),
        
        tabPanel( "wordcloud",
                  "開発中"
        ),
        
        tabPanel( "TF-IDF",
                  "開発中"
        ),
        
        
        tabPanel( "Word2-Vec",
                  "開発中。実装は大分先の予定。重くなりそうなので別アプリにするかも。"
        )
        
        
                      )
    )
  )
)
)




