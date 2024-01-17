library(readxl)
library(DescTools)
library(nonpar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(VIM)
library(MASS)
library(corrplot)
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(DT)
library(profvis)
library(htmltools)
library(shinythemes)
library(reshape2)
library(robustbase)
library(car)
library(lmtest)
library(rsconnect)

# HALAMAN UI 
loginUI <- fluidPage(
  tags$head(
    tags$script('
      $(document).ready(function() {
        $("#loginButton").click(function() {
            Shiny.setInputValue("login", true);
        });
      });
    ')
  ),
  tags$style(
    HTML(
      ".login-container { 
        display: flex; 
        justify-content: center; 
        align-items: center;
        text-align: center; 
        background-image: url('https://github.com/arknsa/image/blob/main/cover_rshiny_desktop.png?raw=true');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-color : white;
        height: 100vh;
      }
      .login-container button {
        font-size: 22px;
      }"
    )
  ),
  div(
    class = "login-container",
    actionButton("loginButton", "Klik untuk melihat Project Kelompok 9")
  )
)

cover <- fluidPage(
  useShinyjs(),
  uiOutput("page")
)

apk <- fluidRow(
  dashboardPage(
    dashboardHeader(title = "Menu", tags$li(
      class = "dropdown user user-menu",
      tags$style(
        HTML(
          ".main-header{ 
              color: pink; 
          }
          .main-header .navbar { 
            background-color: pink; 
          }
          .main-header .navbar .sidebar-toggle:hover { 
            background-color: purple; 
          }
          .main-header .navbar .sidebar-toggle { 
            background-color: black; 
          }
          .main-header .navbar .sidebar-toggle:focus { 
            background-color: black; 
          }
          .main-header .navbar .sidebar-toggle { 
            color: pink;
          }"
        )
      ),
    ) 
    ),
    dashboardSidebar(width = 270,
                     sidebarMenu(
                       menuItem("Pendahuluan", tabName ="pendahuluanTab",icon = icon("book-open")),
                       menuItem("Preprocessing Data", tabName = "dataTab", icon = icon("database"),
                                menuSubItem("Dataset Awal", tabName = "dataset_awal"),
                                menuSubItem("Checking & Handling Missing Value", tabName = "misval"),
                                menuSubItem("Checking & Handling Outlier", tabName = "outlier"),
                                menuSubItem("Transformasi Data", tabName = "transformasi"),
                                menuSubItem("Reduksi Dimensi", tabName = "reduksi"),
                                menuSubItem("Dataset Akhir", tabName = "dataset_akhir")
                       ),
                       menuItem("Visualisasi Data", tabName = "visualisasiTab", icon = icon("line-chart"),
                                menuSubItem("Heatmap Correlation", tabName = "heatmap"),
                                menuSubItem("Scatter Plot", tabName = "scatter"),
                                menuSubItem("Pie Chart", tabName = "pie"),
                                menuSubItem("Histogram + Density Plot", tabName = "histdensity"),
                                menuSubItem("Box Plot", tabName = "box")
                       ),
                       menuItem("Analisis Data", tabName = "interpretasiTab", icon = icon("search"),
                                menuSubItem("Regresi Linier Berganda", tabName = "ols"),
                                menuSubItem("Robust Regression", tabName = "robust"),
                                menuSubItem("Uji Asumsi", tabName = "uji")
                       ),
                       menuItem("Penutup", tabName ="penutupTab",icon = icon("book")
                       )
                     ),
                     tags$style(
                       HTML(
                         ".main-sidebar {
            background-color: black;
          }
          .main-sidebar .sidebar-menu .treeview-menu { 
          background-color: black; /* Ganti dengan warna latar belakang submenu yang diinginkan */
        }
        .main-sidebar .sidebar-menu a { 
          color: pink; /* Ganti dengan warna teks menu yang diinginkan */
        }
        .main-sidebar .sidebar-menu a:hover { 
          background-color: purple; /* Ganti dengan warna saat hover pada menu yang diinginkan */
        }
        .main-sidebar .sidebar-menu .active { 
          background-color: black; /* Ganti dengan warna latar belakang menu aktif yang diinginkan */
        }"
                       )
                     )
    ),
    
    dashboardBody(
      tabItems(
        # Tab Pendahuluan
        tabItem(tabName = "pendahuluanTab",
                h2(strong("Analisis Pengaruh Keterlibatan Mahasiswa Teknologi Sains Data Universitas Airlangga dalam  Masa Pemilihan Umum Tahun 2024"), align = "center"),
                tags$hr(),
                h3(strong("Latar Belakang"), align = "center"),
                h5(HTML("Pemilihan umum adalah wujud sistem politik yang demokratis dan ajang bagi masyarakat untuk menentukan wakil-wakil di pemerintahan. 
      Momentum pemilihan umum seperti pemilihan presiden dan wakil presiden selalu menarik untuk dibahas. 
      Hal ini memicu masyarakat untuk turut ramai memperbincangkan dan terlibat di dalamnya, termasuk memperbincangkan persoalan-persoalan politik, aktor-aktor politik, dan tantangan-tantangan politik.<br><br>
      Generasi milenial diproyeksi menjadi kelompok pemilih dengan proporsi terbesar mencapai 50% di Pemilu 2024. 
      Oleh karena itu, perhatian terhadap keterlibatan politik rakyat dalam pemilu menjadi penting, karena tinggi rendahnya partisipasi ialah tanda serta indikasi penting berfungsinya demokrasi dan perwujudan kedaulatan rakyat. 
      Selain itu, generasi milenial tidak hanya menjadi kelompok pemilih terbesar, tetapi juga menjadi kelompok yang paling aktif dalam menggunakan media sosial. 
      Oleh karena itu, media sosial menjadi platform yang penting dalam kampanye politik, baik untuk menyebarkan informasi, mempengaruhi opini publik, maupun untuk mendengarkan aspirasi masyarakat.<br><br>
      Dalam konteks mahasiswa Teknologi Sains Data Universitas Airlangga, penelitian ini bertujuan untuk menganalisis pengaruh masa pemilihan umum tahun 2024 terhadap keterlibatan mereka. 
      Penelitian ini dilakukan dengan metode kuisioner online yang mencakup pertanyaan tentang Umur, Jenis Kelamin, Angkatan, Pemahaman, Akademis, Diskusi, Kepercayaan, Hubungan, Aktivitas, Sumber Informasi, dan Keterlibatan. 
                   Hasil dari penelitian ini diharapkan dapat memberikan gambaran tentang sejauh mana mahasiswa terlibat dalam pemilihan umum dan faktor-faktor apa saja yang mempengaruhinya."), align = "justify"),
                tags$hr(),
                h3(strong("Rumusan Masalah"), align = "center"),
                h5(HTML("1. Bagaimana pengaruh keterlibatan mahasiswa teknologi sains data Universitas Airlangga berdasarkan faktor-faktor seperti umur, jenis kelamin, angkatan, pemahaman, akademis, diskusi, kepercayaan, hubungan, aktivitas, dan sumber informasi dalam konteks Pemilihan Umum 2024?<br><br>
                        2. Apakah terdapat perbedaan signifikan dalam keterlibatan mahasiswa teknologi sains data Universitas Airlangga berdasarkan faktor-faktor seperti umur, jenis kelamin, angkatan, pemahaman, akademis, diskusi, kepercayaan, hubungan, aktivitas, dan sumber informasi dalam konteks Pemilihan Umum 2024?"), align="justify"),
                tags$hr(),
                h3(strong("Tujuan Penelitian"), align = "center"),
                h5(HTML("1. Untuk mengetahui pengaruh keterlibatan mahasiswa teknologi sains data Universitas Airlangga berdasarkan faktor-faktor seperti umur, jenis kelamin, angkatan, pemahaman, akademis, diskusi, kepercayaan, hubungan, aktivitas, dan sumber informasi dalam konteks Pemilihan Umum 2024.<br><br>
                        2. Untuk mengetahui apakah terdapat perbedaan signifikan dalam keterlibatan mahasiswa teknologi sains data Universitas Airlangga berdasarkan faktor-faktor seperti umur, jenis kelamin, angkatan, pemahaman, akademis, diskusi, kepercayaan, hubungan, aktivitas, dan sumber informasi dalam konteks Pemilihan Umum 2024."), align="justify")
        ),
        
        
        # Tab Data
        tabItem(tabName = "dataset_awal",
                h3(strong("Dataset Awal"), align="center"),
                h4(strong("Unduh Data Dibawah Ini :"), align="justify"),
                h5(tags$a(href = "https://docs.google.com/spreadsheets/d/1m0bccEFecACPFuXrjINGP7Zhtpq8m6jL/edit?usp=sharing&ouid=112286042528992115933&rtpof=true&sd=true", 
                          "Dataset Kelompok 9", target = "_blank")),
                fileInput("uploadData", "Upload Data (Excel)"),
                DT::dataTableOutput("dataPreview1"),
                tags$hr(),
                h4("Deskripsi Data", align="center"),
                DT::dataTableOutput("dataSummary"),
                tags$hr(),
                h4("Statistika Deskriptif", align="center"),
                DT::dataTableOutput("descStats1"),
        ),
        tabItem(tabName = "misval",
                h3(strong("Checking Missing Value"), align="center"),
                plotOutput("missingValue"),
        ),
        tabItem(tabName = "outlier",
                h3(strong("Checking Outlier"), align="center"),
                tags$hr(),
                h4("Visualisasi Box Plot", align="center"),
                plotlyOutput("outlierBox"),
                tags$hr(),
                h4("Grubbs' Test", align="center"),
                DT::dataTableOutput("outlierGrubbs"),
                tags$hr(),
                h4("Leverage Test", align="center"),
                DT::dataTableOutput("outlierLeverage"),
                tags$hr(),
                h4("Cook's Distance Test", align="center"),
                DT::dataTableOutput("outlierCooks"),
                tags$hr(),
                h3(strong("Handling Outlier"), align="center"),
                h4("Dataset tanpa Outlier", align="center"),
                DT::dataTableOutput("dataClean"),
                h4("Visualisasi Box Plot", align="center"),
                plotlyOutput("cleanBox"),
                tags$hr(),
        ),
        tabItem(tabName = "transformasi",
                h3(strong("Transformasi Data"), align="center"),
                h4("One-Hot Encoding Data", align="center"),
                DT::dataTableOutput("dataTransformation"),
                tags$hr(),
        ),
        tabItem(tabName = "reduksi",
                h3(strong("Reduksi Dimensi"), align="center"),
                h4("Backward Elimination", align="center"),
                verbatimTextOutput("backwardElimination"),
                tags$hr(),
        ),
        tabItem(tabName = "dataset_akhir",
                h3(strong("Dataset Akhir"), align="center"),
                DT::dataTableOutput("dataPreview2"),
                tags$hr(),
                h4("Statistika Deskriptif", align="center"),
                DT::dataTableOutput("descStats2"),
        ),
        
        # Tab Visualisasi
        tabItem(tabName = "heatmap",
                h3(strong("Correlation Heatmap"), align="center"),
                plotOutput("CorrelationHeatmap"),
                tags$hr(),
                h4("Interpretasi Correlation Heatmap :"),
                h5(HTML("Berdasarkan hasil visualisasi Heatmap di atas menggambarkan nilai korelasi antar variabel, dengan intensitas warna mencerminkan kekuatan korelasi. Semakin gelap warnanya, semakin kuat korelasinya.<br><br> 
                        Korelasi terkuat antara variabel prediktor dan respon dari heatmap diatas adalah variabel 'Hubungan' dan 'Keterlibatan', yaitu dengan nilai korelasi sebesar 0.53.<br><br> 
                        Selanjutnya, Korelasi terkuat kedua antara variabel prediktor dan respon dari heatmap diatas adalah variabel 'Kepercayaan' dan 'Keterlibatan', yaitu dengan nilai korelasi sebesar 0.51.<br><br>
                        Terakhir, Korelasi terkuat ketiga antara variabel prediktor dan respon dari heatmap diatas adalah variabel 'Akademis' dan 'Keterlibatan', yaitu dengan nilai korelasi sebesar 0.49."), align="justify"),
        ),
        tabItem(tabName = "scatter",
                h3(strong("Scatter Plot"), align="center"),
                selectInput("scatter_x", "Pilih Variabel X untuk Scatter Plot", choices = c("Pemahaman", 
                                                                                            "Akademis", 
                                                                                            "Kepercayaan", 
                                                                                            "Hubungan", 
                                                                                            "Keterlibatan")),
                selectInput("scatter_y", "Pilih Variabel Y untuk Scatter Plot", choices = c("Pemahaman", 
                                                                                            "Akademis", 
                                                                                            "Kepercayaan", 
                                                                                            "Hubungan", 
                                                                                            "Keterlibatan")),
                plotlyOutput("scatterPlot"),
                tags$hr(),
                h4("Interpretasi Scatter Plot :"),
                h5(HTML("Berdasarkan hasil visualisasi Scatter Plot di atas menunjukkan bahwa pada variabel ‘Pemahaman’,  ‘Akademis’,  ‘Kepercayaan’,  dan  ‘Hubungan’ terdapat hubungan positif dengan variabel ‘Keterlibatan’. <br><br>
      Interpretasi dari scatter plot tersebut adalah bahwa semakin tinggi tingkat pemahaman, akademis, kepercayaan, dan hubungan yang dimiliki mahasiswa/i TSD, semakin tinggi juga tingkat keterlibatannya dalam suatu aktivitas atau situasi, seperti dalam pemilu. <br><br>
      Korelasi positif ini menandakan bahwa variabel-variabel tersebut secara signifikan berkontribusi terhadap tingkat keterlibatan mahasiswa/i TSD  dalam pemilu tahun 2024. <br><br>
      Oleh karena itu, dapat disimpulkan bahwa pemahaman, akademik, kepercayaan, dan hubungan memiliki pengaruh yang penting dalam meningkatkan keterlibatan mahasiswa/i TSD dalam pemilu tahun 2024."), align="justify"),
        ),
        tabItem(tabName = "pie",
                h3(strong("Pie Chart"), align="center"),
                selectInput("piechart_variable", "Pilih Variabel untuk Pie Chart", choices = c("Jenis_KelaminLaki.Laki", 
                                                                                               "AngkatanDevaskara",     
                                                                                               "AngkatanVidyadatum",    
                                                                                               "Pemahaman",              
                                                                                               "Akademis",              
                                                                                               "Kepercayaan",            
                                                                                               "Hubungan" ,              
                                                                                               "Sumber_InformasiTV",     
                                                                                               "Keterlibatan")),
                plotlyOutput("pieChart"),
                tags$hr(),
                h4("Interpretasi Pie Chart :"),
                h5(HTML("Berdasarkan hasil visualisasi Pie Chart di atas, kita dapat melihat sebaran karakteristik dari sembilan variabel yang diamati dalam konteks pemilu. Setiap bagian dari pie merepresentasikan proporsi atau persentase kontribusi masing-masing variabel terhadap total keseluruhan. <br><br>
      Dalam visualisasi pie chart untuk variabel 'Jenis_KelaminLaki.Laki', terlihat bahwa responden kami dari mahasiswa/i TSD sebanyak 61.2% merupakan 'Perempuan' (0), sedangkan 'Laki-Laki' (1) hanya menyumbang sebanyak 38.8%. Ini mengindikasikan bahwa jumlah responden yang menjawab formulir Google kami lebih banyak berasal dari kalangan perempuan.<br><br>
      Dalam visualisasi pie chart untuk variabel 'AngkatanDevaskara', terlihat bahwa responden kami dari mahasiswa/i TSD sebanyak 53.7% merupakan 'Angkatan Devaskara' (1), sedangkan 'Selain Angkatan Devaskara' (0) hanya menyumbang sebanyak 46.3%. Ini mengindikasikan bahwa jumlah responden yang menjawab formulir Google kami lebih banyak berasal dari angkatan Devaskara (2022).<br><br>
      Dalam visualisasi pie chart untuk variabel 'AngkatanVidyadatum', terlihat bahwa responden kami dari mahasiswa/i TSD sebanyak 73.1% merupakan 'Selain Angkatan Vidyadatum' (0), sedangkan 'Angkatan Vidyadatum' (1) hanya menyumbang sebanyak 26.9%. Ini mengindikasikan bahwa jumlah responden yang menjawab formulir Google kami lebih banyak berasal dari angkatan selain Vidyadatum (2023).<br><br>
      Dalam visualisasi pie chart untuk variabel 'Pemahaman', terlihat bahwa sebanyak 25.4% mahasiswa/i TSD memilih nilai 6, 23.9% memilih nilai 8, dan 22.4% memilih nilai 7. Dari data ini, dapat disimpulkan bahwa mayoritas mahasiswa/i TSD memiliki pemahaman yang cukup mengenai pemilu. Angka-angka tersebut menunjukkan bahwa sebagian besar responden memberikan penilaian yang cukup baik terhadap pemahaman mereka tentang pemilu.<br><br>
      Dalam visualisasi pie chart untuk variabel 'Akademis', terlihat bahwa sebanyak 19.4% mahasiswa memilih nilai 1, 17.9% memilih nilai 3, dan 14.9% memilih nilai 4 dan 5. Dari data ini, dapat disimpulkan bahwa pemilu tidak mempengaruhi secara signifikan akademis sebagian mahasiswa/i TSD.<br><br>
      Dalam visualisasi pie chart untuk variabel 'Kepercayaan', terlihat bahwa sebanyak 22.4% memilih nilai 7, 20.9% memilih nilai 5, dan 13.4% memilih nilai 4, 6, dan 8. Dari data ini, dapat disimpulkan bahwa sebanyak 20.9% mahasiswa/i TSD memiliki cukup kepercayaan terhadap informasi mengenai pemilu tahun 2024.<br><br>
      Dalam visualisasi pie chart untuk variabel 'Hubungan', terlihat bahwa sebanyak 47.8% responden memilih nilai 1, 14.9% memilih nilai 3, dan 10.4% memilih nilai 2. Dari hasil tiga teratas respon terbanyak ini, dapat disimpulkan bahwa pemilu tidak memiliki pengaruh yang signifikan terhadap hubungan pertemanan mahasiswa/i TSD.<br><br>
      Dalam visualisasi pie chart untuk variabel 'Keterlibatan', terlihat bahwa sebanyak 20.9% mahasiswa memilih nilai 5, 14.9% memilih nilai 3, dan 13.4% memilih nilai 4. Dari data ini, dapat disimpulkan bahwa sebanyak 20.9% mahasiswa/i TSD memiliki cukup kepercayaan terhadap informasi mengenai pemilu tahun 2024.<br><br>
      Dalam visualisasi pie chart untuk variabel 'Sumber_InformasiTV', terlihat bahwa sebanyak 92.5% responden memilih 'Selain Sumber Informasi TV' (0), sedangkan 'Sumber Informasi TV' (1) hanya menyumbang sebanyak 7.5%. Hal ini menunjukkan bahwa mayoritas mahasiswa/i TSD lebih mengandalkan sumber informasi selain TV dalam memperoleh informasi mengenai pemilu."), align="justify"),
        ),        
        tabItem(tabName = "histdensity",
                h3(strong("Histogram + Density Plot"), align="center"),
                selectInput("histdensity_variable", "Pilih Variabel untuk Histogram + Density Plot", choices = c("Pemahaman", 
                                                                                                                 "Akademis", 
                                                                                                                 "Kepercayaan", 
                                                                                                                 "Hubungan", 
                                                                                                                 "Keterlibatan")),
                plotlyOutput("histdensityPlot"),
                tags$hr(),
                h4("Interpretasi Histogram :"),
                h5(HTML("Berdasarkan hasil visualisasi Histogram di atas, kita dapat melihat karakteristik distribusi dari lima variabel yang diamati dalam konteks pemilu. <br><br>
      Pertama, variabel 'Pemahaman' menunjukkan distribusi yang miring ke kiri, mengindikasikan skewness negatif. Pada rentang nilai 5-6 atau lebih dari 0.25 mahasiswa/i TSD menunjukkan pemahaman yang cukup mengenai pemilu, sementara nilai di bawah itu mungkin mencerminkan pengetahuan dasar atau sekadar informasi yang terbatas tentang pemilu. <br><br>
      Kedua, variabel 'Akademis' menunjukkan distribusi yang miring ke kanan, menandakan skewness positif. Pengaruh pemilu terhadap akademik mahasiswa/i TSD terbanyak terjadi pada rentang nilai 1-2. Dari sini, dapat disimpulkan bahwa pemilu tidak memiliki pengaruh yang signifikan terhadap akademik mahasiswa/i TSD. <br><br>
      Ketiga, variabel 'Kepercayaan' menunjukkan distribusi yang juga miring ke kiri dengan skewness negatif. Frekuensi tertinggi terjadi pada rentang nilai 6-7, menandakan bahwa mahasiswa/i TSD memiliki tingkat kepercayaan yang tinggi terhadap informasi politik yang diperoleh selama pemilu tahun 2024. <br><br>
      Keempat, variabel 'Hubungan' menunjukkan distribusi yang miring ke kanan, menandakan skewness positif. Frekuensi tertinggi terjadi pada rentang nilai 1-2, mengindikasikan bahwa pemilu tidak memiliki pengaruh yang signifikan terhadap hubungan pertemanan mahasiswa/i TSD.<br><br>
      Terakhir, variabel 'Keterlibatan' menunjukkan distribusi yang berbentuk seperti lonceng, menandakan berdistribusi normal. Frekuensi tertinggi terjadi pada nilai 5, mengindikasikan bahwa keterlibatan mahasiswa/i TSD di pemilu cukup."), align="justify"),
                tags$hr(),
                h4("Interpretasi Density Plot :"),
                h5(HTML("Berdasarkan hasil visualisasi Density Plot di atas, kita dapat mengamati distribusi kepadatan probabilitas dari data. Density Plot memberikan gambaran visual tentang sebaran nilai-nilai dan membantu kita memahami pola distribusi yang mendasarinya. Puncak pada plot menunjukkan daerah dengan kepadatan tertinggi, sementara ekstensi dan bentuk distribusi memberikan wawasan lebih lanjut tentang karakteristik variabel.<br><br>
      Yang pertama, sebaran pemahaman mahasiswa/i TSD terhadap pemilu memiliki puncak tertinggi di rentang nilai 6-8. Hal ini menunjukkan bahwa sebagian besar mahasiswa/i TSD memiliki tingkat pemahaman yang cukup baik terkait pemilu. <br><br> 
      Sementara itu, pengaruh pemilu terhadap prestasi akademik mahasiswa/i TSD memiliki puncak tertinggi di rentang nilai 2.5-5. Hal ini mengindikasikan bahwa pemilu tidak berpengaruh secara signifikan terhadap prestasi akademik mahasiswa/i TSD TSD. <br><br>
      Selanjutnya, kepercayaan informasi politik mahasiswa/i TSD terhadap pemilu tahun 2024 terlihat berada di rentang nilai 5-7.5. Hal ini menunjukkan bahwa sebagian besar mahasiswa/i TSD memiliki tingkat kepercayaan yang cukup tinggi terhadap informasi politik yang berkaitan dengan pemilu tahun 2024. <br><br> 
      Selanjutnya, pengaruh pemilu terhadap hubungan pertemanan mahasiswa/i TSD terlihat berada di rentang nilai 1-2. Hal ini menunjukkan bahwa pengaruh pemilu terhadap hubungan pertemanan mahasiswa/i TSD cenderung rendah, sehingga pemilu tidak secara signifikan mempengaruhi hubungan pertemanan mahasiswa/i TSD TSD.<br><br>
      Terakhir, pengaruh keterlibatan mahasiswa/i TSD terhadap pemilu berada di rentang nilai 3-6. Hal ini menunjukkan bahwa keterlibatan mahasiswa/i TSD di pemilu cenderung cukup."), align="justify"),
        ),
        tabItem(tabName = "box",
                h3(strong("Box Plot"), align="center"),
                selectInput("boxplot_variable", "Pilih Variabel untuk Box Plot", choices = c("Pemahaman", 
                                                                                             "Akademis", 
                                                                                             "Kepercayaan", 
                                                                                             "Hubungan", 
                                                                                             "Keterlibatan")),
                plotlyOutput("boxPlot"),
                tags$hr(),
                h4("Interpretasi Box Plot :"),
                h5(HTML("Berdasarkan hasil visualisasi Box Plot di atas, untuk lima variabel dalam konteks pemilu, kita dapat mengamati sejumlah karakteristik distribusi data. Setiap box merepresentasikan interquartile range (IQR) yang mencakup sebagian besar data, sementara garis tengah dalam box menunjukkan median. <br><br>
                Hasil visualisasi boxplot menunjukkan bahwa pada variabel 'Pemahaman', nilai minimal sebesar 3, nilai maksimal sebesar 10, dan nilai mediannya adalah 7 tanpa adanya outlier. 
      Persebaran data menunjukkan bahwa sebagian besar pemahaman mahasiswa/i TSD berada di rentang antara 6 hingga 8, dengan quartile pertama (Q1) pada 6, median (Q2) pada 7, dan quartile ketiga (Q3) pada 8.<br>
      Dari hasil ini, dapat disimpulkan bahwa pemahaman mahasiswa/i TSD terhadap pemilu cenderung baik, terkonsentrasi di sekitar nilai median.<br><br>
      Hasil visualisasi boxplot pada variabel 'Akademis' menunjukkan nilai minimal sebesar 1, nilai maksimal sebesar 9, dan nilai median sebesar 4 tanpa adanya outlier. 
      Persebaran data menunjukkan bahwa sebagian besar data tersebar di rentang antara 2 hingga 5, dengan quartile pertama (Q1) pada 2, median (Q2) pada 4, dan quartile ketiga (Q3) pada 5.<br>
      Dari hasil ini, dapat disimpulkan bahwa pemilu tidak memiliki pengaruh signifikan terhadap prestasi akademis mahasiswa/i TSD.<br><br>
      Hasil visualisasi boxplot pada variabel 'Kepercayaan' menunjukkan nilai minimal sebesar 1, nilai maksimal sebesar 9, dan nilai median sebesar 6 tanpa adanya outlier. 
      Persebaran data menunjukkan bahwa data lebih banyak terdistribusi di rentang antara 4 hingga 7, dengan quartile pertama (Q1) pada 4, median (Q2) pada 6, dan quartile ketiga (Q3) pada 7.<br>
      Hal ini mengindikasikan bahwa mahasiswa/i TSD memiliki tingkat kepercayaan yang cukup terhadap informasi politik terkait pemilu 2024, dengan sebagian besar data berkumpul di sekitar nilai median.<br><br>
      Hasil visualisasi boxplot pada variabel 'Hubungan' menunjukkan nilai minimal sebesar 1, nilai maksimal sebesar 8, dan nilai median sebesar 2 tanpa adanya outlier. 
      Persebaran data menunjukkan bahwa sebagian besar data terkumpul di rentang antara 1 hingga 4, dengan quartile pertama (Q1) pada 1, median (Q2) pada 2, dan quartile ketiga (Q3) pada 4.<br>
      Dari hasil ini, dapat disimpulkan bahwa pemilu sama sekali tidak mempengaruhi hubungan pertemanan mahasiswa/i TSD, karena sebagian besar data berkumpul di sekitar nilai median yang rendah.<br><br>
      Hasil visualisasi boxplot pada variabel 'Keterlibatan' menunjukkan nilai minimal sebesar 1, nilai maksimal sebesar 9, dan nilai median sebesar 5 tanpa adanya outlier. 
      Persebaran data menunjukkan bahwa sebagian besar data tersebar di rentang antara 3 hingga 6, dengan quartile pertama (Q1) pada 3, median (Q2) pada 5, dan quartile ketiga (Q3) pada 6.<br>
      Dari hasil ini, dapat disimpulkan bahwa keterlibatan mahasiswa/i TSD di pemilu cenderung cukup, terkonsentrasi di sekitar nilai median.<br><br>"), align="justify"),
        ),
        
        # Tab Analisis
        tabItem(tabName = "ols",
                h3(strong("Analisis Regresi Linier Berganda"),align="center"),
                verbatimTextOutput("OLS"),
                h4("Persamaan Analisis Regresi Berganda :"),
                h5(HTML("keterlibatan = -1.98 + 0.69 (Jenis_KelaminLaki.Laki) + 1.48 (AngkatanDevaskara) + 1.25 (AngkatanVidyadatum) + 0.28 (Pemahaman) + 0.22 (Akademis) + 0.34 (Kepercayaan) + 0.17 (Hubungan) + 1.20 (Sumber_InformasiTV)."), align="justify"),
                tags$hr(),
                h4("Interpretasi Analisis Regresi Berganda :"),
                h5(HTML("H0 : Tidak Terdapat perbedaan secara signifikan.<br>
    H1 : Terdapat perbedaan secara signifikan<br><br>
    Dikarenakan nilai p-value < α (0.05), kita memiliki cukup bukti statistik untuk Tolak H0.<br>
    Oleh karena itu, terdapat perbedaan yang signifikan antara variabel prediktor 'Jenis_KelaminLaki.Laki', 'Pemahaman', 'Akademis', 'Kepercayaan', 'Hubungan', dan 'Sumber_InformasiTV' terhadap variabel respons 'Keterlibatan'.<br>
    Namun, untuk variabel prediktor 'AngkatanDevaskara' dan 'AngkatanVidyadatum', nilai p-value > α (0.05) menunjukkan bahwa kita tidak memiliki cukup bukti statistik untuk Gagal Tolak H0.<br> 
    Oleh karena itu, tidak terdapat perbedaan yang signifikan antara 'AngkatanDevaskara' dan 'AngkatanVidyadatum' terhadap variabel respons 'Keterlibatan'.<br><br>
    Model Analisis Regresi Linier Berganda memiliki R-Square sebesar 0.578, yang menunjukkan bahwa model mampu menjelaskan sekitar 57,8% dari variasi dalam variabel target (keterlibatan).<br>
    Model Analisis Regresi Linier Berganda memiliki Adjusted R-Square sebesar 0.52, yang menunjukkan bahwa model mampu menjelaskan sekitar 52% dari variasi dalam variabel target setelah mempertimbangkan jumlah variabel prediktor yang digunakan dalam model.<br><br>
    Secara keseluruhan, kedua model menunjukkan kinerja yang cukup baik dalam menjelaskan hubungan antara variabel prediktor dengan variabel respon."), align="justify")
        ),
        tabItem(tabName = "robust",
                h3(strong("Robust Regression"), align="center"),
                verbatimTextOutput("Robust")
        ),
        tabItem(tabName = "uji",
                h3(strong("Uji Asumsi"), align="center"),
                tags$hr(),
                h4("Uji Normalitas", align="center"),
                verbatimTextOutput("normalitas"),
                tags$hr(),
                h4("Uji Heteroskedastisitas", align="center"),
                DT::dataTableOutput("hetero"),
                h4("Uji Autokorelasi", align="center"),
                verbatimTextOutput("autokorelasi"),
                tags$hr(),
                h4("Uji Multikolinearitas", align="center"),
                verbatimTextOutput("multikolinearitas")
        ),
        # Tab Penutup
        tabItem(tabName = "penutupTab",
                h3(strong("Kesimpulan"), align = "center"),
                h4(HTML("Berdasarkan hasil Analisis Pengaruh Keterlibatan Mahasiswa Teknologi Sains Data Universitas Airlangga dalam  Masa Pemilihan Umum Tahun 2024 dengan mengambil responden dari 80 mahasiswa/i, dapat disimpulkan bahwa :<br>"), align = "justify"),
                h4(HTML("1. Visualisasi Data<br>")),
                h5(HTML("Dari hasil analisis eksplorasi data yang dilakukan dengan menyajikan hubungan antar variabel didapatkan bahwa variabel respon berdistribusi normal. Berdasarkan nilai korelasi, dapat diketahui bahwa variabel prediktor yang berkorelasi atau berpengaruh terhadap variabel respons (keterlibatan) adalah hubungan (0.53), kepercayaan (0.51), dan akademis (0.49).<br><br>"), align="justify"),
                h4(HTML("2. Analisis Data<br>")),
                h5(HTML("a. Signifikansi<br> Secara keseluruhan, kita dapat menyimpulkan bahwa beberapa variabel prediktor, yaitu 'Jenis_KelaminLaki.Laki', 'Pemahaman', 'Akademis', 'Kepercayaan', 'Hubungan', dan 'Sumber_InformasiTV', berkontribusi secara signifikan terhadap perbedaan dalam variabel respons 'Keterlibatan', sementara 'AngkatanDevaskara' dan 'AngkatanVidyadatum' tidak memberikan kontribusi yang signifikan.<br>"), align = "justify"),
                h5(HTML("b. Model<br> Secara keseluruhan, Kebaikan Model Analisis Regresi Linier Berganda memiliki R-Square dan Adjusted R-Square sebesar 0.578 dan 0.520, yang menunjukkan bahwa model mampu menjelaskan (memberikan pengaruh) sekitar 57.8%, sedangkan 42.2% dijelaskan oleh variabel lain yang tidak termasuk dalam perhitungan dari variasi dalam variabel target (keterlibatan).<br>"), align = "justify"),
                tags$hr(),
                h3(strong("Saran"), align = "center"),
                h5(HTML("Berdasarkan hasil analisis dan kesimpulan diatas, peneliti menyarankan ketika data yang dianalisis memiliki outlier, penggunaan metode regresi linier konvensional seperti Ordinary Least Squares (OLS) dapat menjadi rentan terhadap dampak outlier tersebut. 
                        Dalam hal ini, penggunaan metode regresi yang lebih tahan terhadap outlier, seperti Robust Regression, bisa menjadi pilihan yang lebih baik. 
                        Disamping itu, untuk meningkatkan nilai R-squared, disarankan untuk menambahkan variabel yang memiliki validitas yang lebih tinggi dalam model regresi. 
                        Selain itu bisa dengan melakukan uji asumsi, peneliti dapat memastikan bahwa analisis regresi linier dilakukan dengan benar dan hasilnya dapat diinterpretasikan dengan kepercayaan yang tinggi. "), align="justify")
        )
      ),
      tags$style(
        HTML(
          ".content-wrapper {background-color: #EDF0F7;}"
        )
      )
    )
  )
)

#Server
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    shinyjs::reset("login")
  })
  
  
  output$page <- renderUI({
    if (!is.null(input$login) && input$login == TRUE) {
      apk
    } else {
      loginUI
    }
  })
  
  #Subset Data
  data_awal  <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Periksa keberadaan file
    if (file.exists(file_path)) {
      data1 <- read_excel(file_path)
      
    } else {
      # Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      
      req(input$uploadData)
      data1 <- read_excel(input$uploadData$datapath)
    }
  })
  
  data_angka <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Periksa keberadaan file
    if (file.exists(file_path)) {
      data1 <- read_excel(file_path)
      data_awal_num <- data1[, !(names(data1) %in% c("Angkatan", "Sumber_Informasi", "Jenis_Kelamin"))]
      
      
    } else {
      # Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data1 <- read_excel(input$uploadData$datapath)
      data_awal_num <- data1[, !(names(data1) %in% c("Angkatan", "Sumber_Informasi", "Jenis_Kelamin"))]
    }
  })
  
  data_clean  <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Periksa keberadaan file
    if (file.exists(file_path)) {
      data2 <- read_excel(file_path)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data2, variables_to_check)
      
    } else {
      # Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data2 <- read_excel(input$uploadData$datapath)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data2, variables_to_check)
    }
  })
  data_robust <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Data
    ## Periksa keberadaan file
    if (file.exists(file_path)) {
      data9 <- read_excel(file_path)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = data9))
      data_en <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan"))]
      
    } else {
      ### Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data9 <- read_excel(file_path)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = data9))
      data_en <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan"))]
    }
  })
  
  data_encoded <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Data
    ## Periksa keberadaan file
    if (file.exists(file_path)) {
      data5 <- read_excel(file_path)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data5, variables_to_check)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = datafix))
      data_en <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan"))]
      
    } else {
      ### Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data5 <- read_excel(input$uploadData$datapath)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data5, variables_to_check)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = datafix))
      data_en <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan"))]
    }
  })
  
  data <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Data
    ## Periksa keberadaan file
    if (file.exists(file_path)) {
      data3 <- read_excel(file_path)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data3, variables_to_check)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = datafix))
      data_akhir <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan", "Umur", "AngkatanEuclid", "Diskusi", "Aktivitas", "Sumber_InformasiMedia.Sosial"))]
      
    } else {
      ### Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data3 <- read_excel(input$uploadData$datapath)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data3, variables_to_check)
      datafix_encoded <- data.frame(model.matrix(~ . -1, data = datafix))
      data_akhir <- datafix_encoded[, !(names(datafix_encoded) %in% c("Jenis_KelaminPerempuan", "Umur", "AngkatanEuclid", "Diskusi", "Aktivitas", "Sumber_InformasiMedia.Sosial"))]
    }
  })
  
  data_num  <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Periksa keberadaan file
    if (file.exists(file_path)) {
      data4 <- read_excel(file_path)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data4, variables_to_check)
      data_akhir_num <- datafix[, !(names(datafix) %in% c("Umur", "Angkatan", "Diskusi", "Aktivitas", "Sumber_Informasi", "Jenis_Kelamin"))]
      
    } else {
      # Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data4 <- read_excel(input$uploadData$datapath)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data4, variables_to_check)
      data_akhir_num <- datafix[, !(names(datafix) %in% c("Umur", "Angkatan", "Diskusi", "Aktivitas", "Sumber_Informasi", "Jenis_Kelamin"))]
    }
  })
  
  data_respon  <- reactive({
    file_path <- "pemilu_evd.xlsx"
    
    # Periksa keberadaan file
    if (file.exists(file_path)) {
      data6 <- read_excel(file_path)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data6, variables_to_check)
      data_akhir_num <- datafix[, !(names(datafix) %in% c("Umur", "Angkatan", "Diskusi", "Aktivitas", "Sumber_Informasi", "Jenis_Kelamin"))]
      
    } else {
      # Jika file tidak ditemukan, kembalikan NULL atau lakukan tindakan lain
      req(input$uploadData)
      data6 <- read_excel(input$uploadData$datapath)
      variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
      remove_outliers_iqr <- function(data, variables, threshold = 1.5) {
        for (variable in variables) {
          q1 <- quantile(data[[variable]], 0.25)
          q3 <- quantile(data[[variable]], 0.75)
          iqr <- q3 - q1
          lower_bound <- q1 - threshold * iqr
          upper_bound <- q3 + threshold * iqr
          data <- data[data[[variable]] >= lower_bound & data[[variable]] <= upper_bound, ]
        }
        return(data)
      }
      datafix <- remove_outliers_iqr(data6, variables_to_check)
      data_akhir_num1 <- datafix[, !(names(datafix) %in% c("Umur", "Angkatan", "Diskusi", "Aktivitas", "Sumber_Informasi", "Jenis_Kelamin", "Pemahaman", "Akademis", "Kepercayaan", "Hubungan"))]
    }
  })
  
  # Update choices for selectInput based on uploaded data
  observe({
    req(data_num())
    var_names <- colnames(data_num())
    updateSelectInput(session, "histdensity_variable", choices = var_names)
    updateSelectInput(session, "boxplot_variable", choices = var_names)
  })
  
  observe({
    req(data())
    var_names2 <- colnames(data())
    updateSelectInput(session, "scatter_x", choices = var_names2)
    updateSelectInput(session, "piechart_variable", choices = var_names2)
  })
  
  observe({
    req(data_respon())
    var_names3 <- colnames(data_respon())
    updateSelectInput(session, "scatter_y", choices = var_names3)
  })
  
  ## Data Preview
  output$dataPreview1 <- DT::renderDataTable({
    req(data_awal())
    DT::datatable(data_awal(), options = list(scrollX = TRUE))
  })
  
  ## Rangkuman Data
  output$dataSummary <- DT::renderDataTable({
    # Membuat Dataframe
    data_penelitian <- data.frame(
      Variabel = c("Umur", "Jenis_Kelamin", "Angkatan", "Pemahaman", "Akademis", "Diskusi", "Kepercayaan", "Hubungan", "Aktivitas", "Sumber_Informasi", "Keterlibatan"),
      Tipe.Data = c("Numerik", "Kategorik", "Kategorik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Kategorik", "Numerik"),
      Keterangan = c("Usia responden (dalam tahun)", "Jenis kelamin responden (Laki-Laki dan Perempuan)", "Nama angkatan responden (Euclid, Archimedes, Devaskara, dan Vidyadatum)", "Seberapa besar tingkat pemahaman responden terhadap pemilu  (1-10)", "Seberapa besar pengaruh masa pemilu terhadap keseharian akademis mahasiswa TSD (1-10)", "Seberapa sering responden terlibat diskusi politik dengan teman-teman  (1-10)", "Seberapa besar tingkat kepercayaan informasi politik yang responden peroleh selama masa pemilu 2024 (1-10)", "Seberapa besar pengaruh masa pemilu terhadap hubungan pertemanan responden  (1-10)", "Sejauh mana responden merasa perlu melibatkan diri dalam aktivitas politik sebagai mahasiswa TSD (1-10)", "Dari mana responden mendapatkan informasi tentang pemilu (TV, Berita Digital, Media Sosial, dan Koran)", "Sejauh mana responden terlibat dalam proses Pemilu 2024 (1-10)")
    )
    DT::datatable(data_penelitian, options = list(scrollX = TRUE))
  })
  
  ## Statistika Deskriptif
  output$descStats1 <- DT::renderDataTable({
    statistika_deskriptif <- function(data) {
      # Mendapatkan hanya kolom-kolom numerik
      kolom_numerik <- data[, sapply(data, is.numeric)]
      # Inisialisasi vektor kosong untuk menyimpan hasil perhitungan
      variables <- names(kolom_numerik)
      means <- medians <- q1s <- q3s <- variances <- sds <- ranges <- sums <- mins <- maxs <- numeric(length(variables))
      # Loop untuk setiap variabel numerik
      for (i in seq_along(variables)) {
        variable <- variables[i]
        values <- kolom_numerik[[variable]]
        # Hitung statistika deskriptif
        means[i] <- mean(values)
        medians[i] <- median(values)
        q1s[i] <- quantile(values, 0.25)
        q3s[i] <- quantile(values, 0.75)
        variances[i] <- var(values)
        sds[i] <- sd(values)
        ranges[i] <- max(values) - min(values)
        sums[i] <- sum(values)
        mins[i] <- min(values)
        maxs[i] <- max(values)
      }
      
      hasil <- data.frame(Variabel = variables, Mean = means, Median = medians, Q1 = q1s, Q3 = q3s, Variance = variances, SD = sds, Range = ranges, Sum = sums, Min = mins, Max = maxs, stringsAsFactors = FALSE)
      
      return(hasil)
    }
    # Tampilkan data.frame sebagai tabel interaktif
    DT::datatable(statistika_deskriptif(data_angka()), options = list(scrollX = TRUE))
  })
  
  ## Check Missing Values
  output$missingValue <- renderPlot({
    req(data_awal())
    aggr_plot <- aggr(data_awal(), col=c('navyblue','red'), numbers=TRUE,
                      sortVars=TRUE, labels=names(data_awal()), cex.axis=.7,
                      gap=3, ylab=c("Histogram of missing data","Pattern"))
  })
  
  ## Check Outlier
  ### Visualisasi Box Plot
  output$outlierBox <- renderPlotly({
    req(data_awal())
    
    # Mengambil hanya variabel numerik
    numeric_vars <- sapply(data_awal(), is.numeric)
    
    # Mendapatkan nama kolom variabel numerik
    numeric_columns <- names(data_awal())[numeric_vars]
    
    # Membuat boxplot untuk setiap variabel numerik
    plots <- lapply(numeric_columns, function(col) {
      plot_ly(data_awal(), y = ~get(col), type = "box", name = col) %>%
        layout(showlegend = FALSE)
    })
    
    # Menggabungkan boxplot menjadi satu tampilan
    subplot(plots, nrows = 1)
    
  })
  
  
  ### Grubbs' Test
  output$outlierGrubbs <- DT::renderDataTable({
    variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
    grubbs_test_multiple_with_pvalue_summary <- function(data, variables, alpha = 0.05) {
      results <- data.frame()
      
      for (variable in variables) {
        n <- length(data[[variable]])
        mean_val <- mean(data[[variable]])
        sd_val <- sd(data[[variable]])
        z_max <- (max(data[[variable]]) - mean_val) / sd_val
        z_min <- (min(data[[variable]]) - mean_val) / sd_val
        critical_value <- qt(1 - alpha / (2 * n), df = n - 2)
        G_max <- z_max / sqrt(1 - (1 / (2 * n)))
        G_min <- z_min / sqrt(1 - (1 / (2 * n)))
        p_value_max <- 2 * pt(-abs(G_max), df = n - 2)
        p_value_min <- 2 * pt(-abs(G_min), df = n - 2)
        max_outlier <- ifelse(p_value_max < alpha, max(data[[variable]]), NA)
        min_outlier <- ifelse(p_value_min < alpha, min(data[[variable]]), NA)
        
        # Construct a data frame with the results
        result <- data.frame(
          Variable = variable,
          "Min_outlier" = min_outlier,
          "Max_outlier" = max_outlier,
          "Max_p-value" = p_value_max,
          "Min_p_value" = p_value_min,
          "alpha" = 0.05,
          "Kesimpulan_nilai_Max" = ifelse(p_value_max < alpha, "Tolak H0", "Gagal Tolak H0"),
          "Kesimpulan_nilai_Min" = ifelse(p_value_min < alpha, "Tolak H0", "Gagal Tolak H0")
        )
        
        results <- rbind(results, result)
      }
      
      return(results)
    }
    
    # Tampilkan data.frame sebagai tabel interaktif
    DT::datatable(grubbs_test_multiple_with_pvalue_summary(data_awal(), variables_to_check), options = list(scrollX = TRUE))
  })
  
  ### Leverage Test
  output$outlierLeverage <- DT::renderDataTable({
    variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
    leverage_test_multiple_with_summary <- function(data, variables, alpha = 0.05) {
      results <- data.frame()
      
      for (variable in variables) {
        n <- length(data[[variable]])
        mean_val <- mean(data[[variable]])
        sd_val <- sd(data[[variable]])
        leverage_stat <- (data[[variable]] - mean_val) / sd_val
        critical_value <- qt(1 - alpha / (2 * n), df = n - 2)
        critical_leverage <- sqrt((n - 1) / (n - critical_value^2))
        outlier_values <- ifelse(abs(leverage_stat) > critical_leverage, data[[variable]], NA)
        
        # Construct a data frame with the results
        result <- data.frame(
          Variable = variable,
          "Nilai_Statistik" = leverage_stat,
          "Nilai_Kritis" = critical_leverage,
          "Nilai_Outlier" = outlier_values,
          "Kesimpulan" = ifelse(any(abs(leverage_stat) > critical_leverage), "Tolak H0, Terdapat Setidaknya Satu Nilai yang Merupakan Pencilan pada Variabel Ini.", "Gagal Tolak H0, Tidak Terdapat Pencilan pada Variabel Ini.")
        )
        
        results <- rbind(results, result)
      }
      
      return(results)
    }
    
    # Tampilkan data.frame sebagai tabel interaktif
    DT::datatable(leverage_test_multiple_with_summary(data_awal(), variables_to_check), options = list(scrollX = TRUE))
  })
  
  ### Cook's Distance Test
  output$outlierCooks <- DT::renderDataTable({
    variables_to_check <- c("Umur", "Pemahaman", "Akademis","Diskusi","Keterlibatan", "Kepercayaan", "Hubungan", "Aktivitas")
    # Fungsi Cook's Distance Test untuk Multiple Variables dengan Kesimpulan
    cooks_distance_test_multiple_with_summary <- function(data, variables, alpha = 0.05) {
      results <- data.frame()
      
      for (variable in variables) {
        # Mendapatkan panjang sampel
        n <- length(data[[variable]])
        
        # Menghitung mean dan standard deviation
        mean_val <- mean(data[[variable]])
        sd_val <- sd(data[[variable]])
        
        # Menghitung Cook's Distance
        model <- lm(data[[variable]] ~ 1)
        cooks_distance <- cooks.distance(model)
        
        # Menghitung critical value dari tabel distribusi F
        critical_value <- qf(1 - alpha, df1 = 1, df2 = n - 2)
        
        # Menentukan apakah nilai Cook's Distance melebihi nilai kritis
        outlier_values <- ifelse(cooks_distance > critical_value, data[[variable]], NA)
        
        # Menyimpan hasil Cook's Distance Test untuk variabel tertentu
        result <- data.frame(
          Variable = variable,
          "Nilai_Statistik" = cooks_distance,
          "Nilai_Kritis" = critical_value,
          "Nilai_Outlier" = outlier_values,
          "Kesimpulan" = ifelse(any(abs(cooks_distance) > critical_value),
                                "Tolak H0, Terdapat Setidaknya Satu Nilai yang Merupakan Pencilan pada Variabel Ini.",
                                "Gagal Tolak H0, Tidak Terdapat Pencilan pada Variabel Ini.")
        )
        
        results <- rbind(results, result)
      }
      
      return(results)
    }
    
    # Contoh penggunaan Cook's Distance Test untuk Multiple Variables dengan Kesimpulan
    DT::datatable(cooks_distance_test_multiple_with_summary(data_awal(), variables_to_check), options = list(scrollX = TRUE))
  })
  
  ## Handling Outlier
  ### Menghapus Outlier
  output$dataClean <- DT::renderDataTable({
    req(data_clean())
    DT::datatable(data_clean(), options = list(scrollX = TRUE))
  })
  
  ### Visualisasi Box Plot
  output$cleanBox <- renderPlotly({
    req(data_clean())
    
    # Mengambil hanya variabel numerik
    numeric_vars <- sapply(data_clean(), is.numeric)
    
    # Mendapatkan nama kolom variabel numerik
    numeric_columns <- names(data_clean())[numeric_vars]
    
    # Membuat boxplot untuk setiap variabel numerik
    plots <- lapply(numeric_columns, function(col) {
      plot_ly(data_clean(), y = ~get(col), type = "box", name = col) %>%
        layout(showlegend = FALSE)
    })
    
    # Menggabungkan boxplot menjadi satu tampilan
    subplot(plots, nrows = 1)
    
  })
  
  ## Transformasi Data
  ### Menggunakan One-Hot Encoding
  output$dataTransformation <- DT::renderDataTable({
    req(data_encoded())
    DT::datatable(data_encoded(), options = list(scrollX = TRUE))
  })
  
  ## Reduksi Dimensi
  ### Backward Elimination
  output$backwardElimination <- renderPrint({
    # Memisahkan variabel independent dan dependent
    req(data_encoded())
    X <- data_encoded()[, !(names(data_encoded()) %in% c("Keterlibatan"))]
    y <- data_encoded()$Keterlibatan
    
    # Menambahkan konstanta pada X
    X <- cbind(1, X)
    
    # Membuat model OLS awal
    initial_model <- lm(y ~ ., data = data.frame(cbind(y, X)))
    
    # Fungsi untuk melakukan backward elimination
    backward_elimination <- function(model) {
      while (TRUE) {
        # Mencari variabel dengan p-value tertinggi
        p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]
        
        # Jika tidak ada variabel dengan p-value lebih besar dari 0.05, maka keluar dari loop
        if (all(p_values <= 0.05)) {
          break
        }
        
        # Jika variabel dengan p-value tertinggi memiliki p-value lebih besar dari 0.05, maka hapus variabel tersebut
        feature_with_max_pvalue <- names(p_values)[which.max(p_values)]
        X <- X[, !(names(X) %in% c(feature_with_max_pvalue))]
        model <- lm(y ~ ., data = data.frame(cbind(y, X)))
        print(paste('Variables: ', names(X), '| Adj R-Squared: ', summary(model)$adj.r.squared))
      }
      return(model)
    }
    
    # Melakukan backward elimination
    final_model <- backward_elimination(initial_model)
  })
  
  ## Data Preview
  output$dataPreview2 <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(scrollX = TRUE))
  })
  
  ## Statistika Deskriptif
  output$descStats2 <- DT::renderDataTable({
    statistika_deskriptif <- function(data) {
      # Mendapatkan hanya kolom-kolom numerik
      kolom_numerik <- data[, sapply(data, is.numeric)]
      # Inisialisasi vektor kosong untuk menyimpan hasil perhitungan
      variables <- names(kolom_numerik)
      means <- medians <- q1s <- q3s <- variances <- sds <- ranges <- sums <- mins <- maxs <- numeric(length(variables))
      # Loop untuk setiap variabel numerik
      for (i in seq_along(variables)) {
        variable <- variables[i]
        values <- kolom_numerik[[variable]]
        # Hitung statistika deskriptif
        means[i] <- mean(values)
        medians[i] <- median(values)
        q1s[i] <- quantile(values, 0.25)
        q3s[i] <- quantile(values, 0.75)
        variances[i] <- var(values)
        sds[i] <- sd(values)
        ranges[i] <- max(values) - min(values)
        sums[i] <- sum(values)
        mins[i] <- min(values)
        maxs[i] <- max(values)
      }
      
      hasil <- data.frame(Variabel = variables, Mean = means, Median = medians, Q1 = q1s, Q3 = q3s, Variance = variances, SD = sds, Range = ranges, Sum = sums, Min = mins, Max = maxs, stringsAsFactors = FALSE)
      
      return(hasil)
    }
    # Tampilkan data.frame sebagai tabel interaktif
    DT::datatable(statistika_deskriptif(data_num()), options = list(scrollX = TRUE))
  })
  
  # Grafik 1 (Heat Map Correlation)
  output$CorrelationHeatmap <- renderPlot({
    req(data())
    # Menghitung matriks korelasi
    correlation_matrix <- cor(data())
    
    # Membuat plot heatmap korelasi dengan bentuk segitiga
    ggplot(data = melt(correlation_matrix, na.rm = TRUE), aes(Var2, Var1, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
      scale_fill_distiller(palette = "Blues") +
      labs(title = "Heatmap Korelasi Antar Variabel") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  # Grafik 2 (Scatter Plot)
  output$scatterPlot <- renderPlotly({
    req(data_num(), input$scatter_x, input$scatter_y)
    
    # Mendapatkan nama variabel
    x_variable_scatter <- input$scatter_x
    y_variable_scatter <- input$scatter_y
    
    # Menggunakan plot_ly untuk membuat scatter plot plotly
    plot_ly(data_num(), x = ~get(x_variable_scatter), y = ~get(y_variable_scatter), marker = list(color = ~get(x_variable_scatter), colorscale = "Blues")) %>%
      layout(title = paste("Scatter Plot of", x_variable_scatter, "and", y_variable_scatter), xaxis = list(title = x_variable_scatter), yaxis = list(title = y_variable_scatter))
  })
  
  
  # Grafik 3 (Pie Chart)
  # Menampilkan pie chart
  output$pieChart <- renderPlotly({
    req(data(), input$piechart_variable)
    
    # Menghitung frekuensi masing-masing nilai dalam kolom terpilih
    value_counts <- table(data()[[input$piechart_variable]])
    
    # Menghitung persentase
    percentages <- round((value_counts / sum(value_counts)) * 100, 1)
    
    # Membuat label dengan persentase
    labels <- paste(names(value_counts), ": ", percentages, "%")
    
    # Membuat pie chart dengan Plotly
    pie_chart <- plot_ly(labels = ~labels,
                         values = ~value_counts,
                         type = "pie",
                         marker = list(colors = colors),
                         textposition = 'inside',
                         textinfo = 'percent',
                         insidetextfont = list(color = 'Blues'),
                         hoverinfo = 'text',
                         text = ~paste('Berjumlah', value_counts))
    
    # Menampilkan pie chart
    pie_chart
  })
  
  # Grafik 4 (Histogram + Density Plot)
  output$histdensityPlot <- renderPlotly({
    req(data_num(), input$histdensity_variable)
    
    density_values <- density(data_num()[[input$histdensity_variable]])
    
    # Buat histogram dengan garis densitas
    hist_plot <- plot_ly() %>%
      add_trace(x = data_num()[[input$histdensity_variable]], 
                type = "histogram", 
                histnorm = "probability", 
                nbinsy = 30,  # Jumlah bins untuk sumbu y
                marker = list(color = "lightblue", line = list(color = "black"))) %>%
      add_trace(x = density_values$x, 
                y = density_values$y,  # Tukar x dan y untuk orientasi horizontal
                type = "scatter", 
                mode = "lines", 
                line = list(color = "black", width = 2)) %>%
      layout(title = paste("Histogram with Density Line of", input$histdensity_variable),
             yaxis = list(title = "Frekuensi/Density"),  # Tukar sumbu x dan y
             xaxis = list(title = input$histdensity_variable))
    
    hist_plot
  })
  
  # Grafik 5 (Box Plot)
  output$boxPlot <- renderPlotly({
    req(data_num(), input$boxplot_variable)
    
    plot_ly(data_num(), y = ~get(input$boxplot_variable), type = "box", jitter = 0.3, pointpos = -1.8, box = list(marker = list(color = "black")),
            point = list(marker = list(color = "black")), line = list(color = "black"), colors = "lightblue") %>%
      layout(title = paste("Boxplot of", input$boxplot_variable), yaxis = list(title = input$boxplot_variable))
  })
  
  # Analisis Data
  ## Analisis Regresi Linier Berganda
  output$OLS <- renderPrint({
    req(data())
    data_akhir <- data()
    # Menghapus kolom 'Keterlibatan'
    x <- data_akhir[, !(names(data_akhir) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y <- data_akhir$Keterlibatan
    
    # OLS Model
    model <- lm(y ~ ., data = data.frame(cbind(y, x)))
    
    # Menampilkan ringkasan hasil regresi
    summary(model)  
  })
  output$Robust <- renderPrint({
    req(data_robust())
    x1 <- data_robust() [, !(names(data_robust()) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y1 <- data_robust()$Keterlibatan
    
    # OLS Model
    model1 <- lm(y1 ~ ., data = data.frame(cbind(y1, x1)))
    # Fit robust regression model
    robust_model1 <- lmrob(y1 ~ ., data = data.frame(cbind(y1, x1)))
    
    # Print summary
    summary(robust_model1)
  })
  
  ## Normalitas
  output$normalitas <- renderPrint({
    req(data())
    data_akhir <- data()
    # Menghapus kolom 'Keterlibatan'
    x <- data_akhir[, !(names(data_akhir) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y <- data_akhir$Keterlibatan
    
    # Menambahkan konstanta ke variabel x
    X <- cbind(1, x)
    
    # OLS Model
    model <- lm(y ~ ., data = data.frame(cbind(y, X)))
    residuals <- residuals(model)
    
    # Uji normalitas dengan uji Kolmogorov-Smirnov
    ks_test_result <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
    
    # Menampilkan hasil uji
    print(paste("Statistic KS:", ks_test_result$statistic))
    print(paste("P-value:", ks_test_result$p.value))
    
    # Interpretasi hasil uji
    alpha <- 0.05
    if (ks_test_result$p.value > alpha) {
      print("Data Berdistribusi Normal (Gagal Tolak H0)")
    } else {
      print("Data Tidak Berdistribusi Normal (Tolak H0)")
    }
  })
  
  
  ## Heteroskedastisitas
  output$hetero <- DT::renderDataTable({
    req(data())
    data_akhir <- data()
    # Menghapus kolom 'Keterlibatan'
    x <- data_akhir[, !(names(data_akhir) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y <- data_akhir$Keterlibatan
    
    # OLS Model
    model <- lm(y ~ ., data = data.frame(cbind(y,x)))
    pred_y <- predict(model, data = data.frame(cbind(y, x)))
    
    # Simpan residu
    resid <- y - pred_y
    
    # Absolut residu
    y_abs_resid <- abs(resid)
    
    data_combined <- cbind(y_abs_resid, x)
    
    # Membuat model regresi untuk residu absolut
    result_glejser <- lm(y_abs_resid ~., data = data_combined)
    
    # Uji statistik
    glejser_test_statistic <- summary(result_glejser)$fstatistic[1]
    glejser_p_values <- summary(result_glejser)$coefficients[, "Pr(>|t|)"]
    
    # Menampilkan hasil uji
    print(paste("Test Statistic Glejser:", glejser_test_statistic))
    print(paste("P-values Glejser:", glejser_p_values))
    
    # Interpretasi hasil uji
    alpha <- 0.05
    reject_h0 <- glejser_p_values < alpha
    
    # Buat data frame untuk hasil uji tiap variabel
    df_glejser <- data.frame(
      Variabel = names(glejser_p_values),
      P_Value = glejser_p_values,
      Keputusan = ifelse(reject_h0, 'Tolak H0', 'Gagal Tolak H0')
    )
    
    # Print data frame
    DT::datatable(df_glejser, options = list(scrollX = TRUE))
  })
  
  ## Autokorelasi
  output$autokorelasi <- renderPrint({
    req(data())
    data_akhir <- data()
    # Menghapus kolom 'Keterlibatan'
    x <- data_akhir[, !(names(data_akhir) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y <- data_akhir$Keterlibatan
    
    # OLS Model
    model <- lm(y ~ ., data = data.frame(cbind(y,x)))
    # Menghitung uji Durbin-Watson
    dw_test_result <- dwtest(model)
    
    # Menampilkan hasil uji
    print(paste("Statistic DW:", dw_test_result$statistic))
    print(paste("P-value:", dw_test_result$p.value))
    
    # Interpretasi hasil uji
    alpha <- 0.05
    if (dw_test_result$p.value > alpha) {
      print("Tidak ada Autokorelasi (Gagal Tolak H0)")
    } else {
      print("Ada Autokorelasi (Tolak H0)")
    }
  })
  
  ## Multikolinearitas
  output$multikolinearitas <- renderPrint({
    req(data())
    data_akhir <- data()
    # Menghapus kolom 'Keterlibatan'
    x <- data_akhir[, !(names(data_akhir) %in% c('Keterlibatan'))]
    
    # Menetapkan variabel y sebagai kolom 'Keterlibatan'
    y <- data_akhir$Keterlibatan
    
    # OLS Model
    model <- lm(y ~ ., data = data.frame(cbind(y,x)))
    # Menghitung VIF
    vif_result <- car::vif(model)
    
    # Menampilkan hasil VIF
    print(vif_result)
    
    # Interpretasi hasil VIF
    high_vif <- vif_result[vif_result > 5]
    if (length(high_vif) > 0) {
      print("Ada Multikolinearitas (Tolak H0)")
    } else {
      print("Tidak ada Multikolinearitas (Gagal Tolak H0)")
    }
  })
}

shinyApp(ui = cover, server=server)
