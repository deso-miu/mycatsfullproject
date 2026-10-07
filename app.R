# ============================================
# 🌸 КОТИКО-ПРОЕКТ v2: с вводом данных 🌸
# ============================================
library(shiny)
library(ggplot2)
library(bslib)

# --------------------------------------------
# 🌷 ТЕМА
# --------------------------------------------
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#FF69B4",
  secondary = "#B0E0E6",
  success = "#98FB98",
  base_font = font_google("Comfortaa"),
  heading_font = font_google("Pacifico")
)

# --------------------------------------------
# UI
# --------------------------------------------
ui <- fluidPage(
  theme = my_theme,
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #FFE4E1 0%, #E0F7FA 50%, #F0FFF0 100%);
        background-attachment: fixed;
      }
      .cat-header {
        text-align: center;
        padding: 15px;
        background: linear-gradient(90deg, #FFB6C1, #B0E0E6, #98FB98);
        border-radius: 20px;
        box-shadow: 0 4px 15px rgba(255,105,180,0.3);
        margin-bottom: 20px;
      }
      .cat-header h1 {
        color: white;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
        margin: 0;
        font-size: 2.2em;
      }
      .well {
        background: rgba(255,255,255,0.9) !important;
        border: 2px dashed #FFB6C1 !important;
        border-radius: 20px !important;
        box-shadow: 0 4px 12px rgba(176,224,230,0.4) !important;
      }
      .btn-primary {
        background: linear-gradient(90deg, #FF69B4, #FFB6C1) !important;
        border: none !important;
        border-radius: 25px !important;
        font-weight: bold;
        padding: 10px 25px !important;
        box-shadow: 0 4px 10px rgba(255,105,180,0.4);
      }
      .btn-primary:hover {
        transform: scale(1.05);
        transition: 0.3s;
      }
      #result {
        background: #FFF8DC;
        padding: 15px;
        border-radius: 15px;
        border-left: 5px solid #FFB6C1;
        font-size: 15px;
        min-height: 80px;
      }
      .cat-footer {
        text-align: center;
        margin-top: 30px;
        font-size: 14px;
        color: #888;
      }
      /* Стилизуем поля ввода */
      .form-control, .form-select {
        border-radius: 15px !important;
        border: 2px solid #FFB6C1 !important;
        background: #FFF8FA !important;
      }
      .form-control:focus, .form-select:focus {
        border-color: #FF69B4 !important;
        box-shadow: 0 0 8px rgba(255,105,180,0.4) !important;
      }
      label {
        color: #FF69B4 !important;
        font-weight: bold;
      }
    "))
  ),
  
  div(class = "cat-header",
      h1("🐱Котики считают задачи🌸")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("🌷 Выбери раздел:"),
      selectInput("main_menu", NULL,
                  choices = c("🧮 Математические задачи",
                              "📊 Анализ данных / Статистика",
                              "💰 Финансовые задачи")),
      
      uiOutput("sub_menu"),
      hr(),
      
      # 🌟 ЗДЕСЬ БУДУТ ПОЛЯ ВВОДА (динамические)
      uiOutput("inputs"),
      
      br(),
      actionButton("run", "🐾 Выполнить!",
                   class = "btn-primary",
                   width = "100%"),
      
      br(), br(),
      div(style = "text-align:center; font-size:40px;", "🐱")
    ),
    
    mainPanel(
      h3(textOutput("header"), style = "color: #FF69B4;"),
      hr(),
      verbatimTextOutput("result"),
      plotOutput("plot")
    )
  ),
  
  div(class = "cat-footer",
      "Сделано с 💖 и 🐱 на R + Shiny"
  )
)

# --------------------------------------------
# SERVER
# --------------------------------------------
server <- function(input, output) {
  
  # --- Подменю ---
  output$sub_menu <- renderUI({
    if (grepl("Матем", input$main_menu)) {
      selectInput("sub", "🌸 Подраздел:",
                  choices = c("Квадратное уравнение",
                              "Факториал",
                              "Простые числа"))
    } else if (grepl("Анализ", input$main_menu)) {
      selectInput("sub", "🌸 Подраздел:",
                  choices = c("Описательные статистики",
                              "Гистограмма",
                              "Корреляция"))
    } else {
      selectInput("sub", "🌸 Подраздел:",
                  choices = c("Сложный процент",
                              "Ипотека (аннуитет)",
                              "Дисконтирование"))
    }
  })
  
  # --- ДИНАМИЧЕСКИЕ ПОЛЯ ВВОДА ---
  output$inputs <- renderUI({
    req(input$sub)
    
    # 🧮 Квадратное уравнение
    if (input$sub == "Квадратное уравнение") {
      tagList(
        numericInput("a", "🐱 Коэффициент a:", value = 1),
        numericInput("b", "🐱 Коэффициент b:", value = -3),
        numericInput("c", "🐱 Коэффициент c:", value = 2)
      )
    }
    # 🧮 Факториал
    else if (input$sub == "Факториал") {
      numericInput("n_fact", "🐱 Число n (целое, ≤ 170):",
                   value = 5, min = 0, max = 170, step = 1)
    }
    # 🧮 Простые числа
    else if (input$sub == "Простые числа") {
      numericInput("n_prime", "🐱 Искать простые до:",
                   value = 30, min = 2, step = 1)
    }
    # 📊 Описательные статистики
    else if (input$sub == "Описательные статистики") {
      textAreaInput("data_desc",
                    "🌸 Введи числа через запятую:",
                    value = "12, 15, 18, 22, 25, 30, 35, 40, 45, 50",
                    rows = 3)
    }
    # 📊 Гистограмма
    else if (input$sub == "Гистограмма") {
      tagList(
        numericInput("n_hist", "🐱 Кол-во точек:", value = 200, min = 10),
        numericInput("mean_hist", "🌸 Среднее:", value = 50),
        numericInput("sd_hist", "🌸 Ст. отклонение:", value = 10, min = 0.1)
      )
    }
    # 📊 Корреляция
    else if (input$sub == "Корреляция") {
      tagList(
        textAreaInput("x_cor", "🌸 Набор X (через запятую):",
                      value = "1, 2, 3, 4, 5, 6, 7, 8", rows = 2),
        textAreaInput("y_cor", "🌸 Набор Y (через запятую):",
                      value = "2, 4, 5, 4, 5, 7, 8, 9", rows = 2)
      )
    }
    # 💰 Сложный процент
    else if (input$sub == "Сложный процент") {
      tagList(
        numericInput("P", "💰 Начальная сумма:", value = 100000, min = 0),
        numericInput("r_sp", "🌸 Ставка (% годовых):", value = 10, min = 0),
        numericInput("n_sp", "🌸 Срок (лет):", value = 5, min = 1)
      )
    }
    # 💰 Ипотека
    else if (input$sub == "Ипотека (аннуитет)") {
      tagList(
        numericInput("S_mort", "💰 Сумма кредита:", value = 3000000, min = 0),
        numericInput("r_mort", "🌸 Годовая ставка (%):", value = 12, min = 0),
        numericInput("n_mort", "🌸 Срок (лет):", value = 20, min = 1)
      )
    }
    # 💰 Дисконтирование
    else if (input$sub == "Дисконтирование") {
      tagList(
        numericInput("FV", "💰 Будущая стоимость:", value = 150000, min = 0),
        numericInput("r_disc", "🌸 Ставка (%):", value = 10, min = 0),
        numericInput("n_disc", "🌸 Через сколько лет:", value = 3, min = 1)
      )
    }
  })
  
  # --- Заголовок ---
  output$header <- renderText({
    paste("🐾", input$main_menu, "→", input$sub)
  })
  
  # --- Парсим числа из строки ---
  parse_numbers <- function(txt) {
    nums <- suppressWarnings(
      as.numeric(trimws(unlist(strsplit(txt, ","))))
    )
    nums[!is.na(nums)]
  }
  
  # --- ЛОГИКА (реагирует на кнопку) ---
  output$result <- renderPrint({
    req(input$run > 0)
    isolate({
      
      # 🧮 МАТЕМАТИКА
      if (input$sub == "Квадратное уравнение") {
        a <- input$a; b <- input$b; c <- input$c
        cat("🐱 Уравнение:", a, "x² +", b, "x +", c, "= 0\n")
        D <- b^2 - 4*a*c
        cat("🌸 Дискриминант D =", D, "\n")
        if (a == 0) {
          cat("⚠ Это не квадратное уравнение (a = 0)\n")
        } else if (D > 0) {
          x1 <- (-b + sqrt(D)) / (2*a)
          x2 <- (-b - sqrt(D)) / (2*a)
          cat("🌷 Два корня:\n   x1 =", round(x1, 4), "\n   x2 =", round(x2, 4), "\n")
        } else if (D == 0) {
          x <- -b / (2*a)
          cat("🌷 Один корень: x =", round(x, 4), "\n")
        } else {
          cat("😿 Корней нет (D < 0)\n")
        }
      }
      
      else if (input$sub == "Факториал") {
        n <- input$n_fact
        if (n < 0 || n != round(n)) {
          cat("😿 Нужно целое неотрицательное число!\n")
        } else {
          cat("🐱 Факториал", n, "! =", factorial(n), "\n")
        }
      }
      
      else if (input$sub == "Простые числа") {
        n <- input$n_prime
        if (n < 2) {
          cat("😿 Нужно число ≥ 2\n")
        } else {
          primes <- c()
          for (i in 2:n) {
            is_prime <- TRUE
            if (i > 2) {
              for (j in 2:floor(sqrt(i))) {
                if (i %% j == 0) { is_prime <- FALSE; break }
              }
            }
            if (is_prime) primes <- c(primes, i)
          }
          cat("🌸 Простые числа до", n, ":\n", primes, "\n")
          cat("🐱 Всего найдено:", length(primes), "\n")
        }
      }
      
      # 📊 СТАТИСТИКА
      else if (input$sub == "Описательные статистики") {
        x <- parse_numbers(input$data_desc)
        if (length(x) < 2) {
          cat("😿 Введи хотя бы 2 числа через запятую\n")
        } else {
          cat("🐱 Данные (", length(x), "чисел):", x, "\n\n")
          cat("🌸 Среднее:       ", round(mean(x), 4), "\n")
          cat("🌸 Медиана:       ", round(median(x), 4), "\n")
          cat("🌸 Ст.отклонение: ", round(sd(x), 4), "\n")
          cat("🌸 Дисперсия:     ", round(var(x), 4), "\n")
          cat("🌸 Мин/Макс:      ", min(x), "/", max(x), "\n")
          cat("🌸 Размах:        ", max(x) - min(x), "\n")
        }
      }
      
      else if (input$sub == "Гистограмма") {
        cat("🐱 Смотри график справа →\n")
        cat("🌸 Параметры: n =", input$n_hist,
            ", среднее =", input$mean_hist,
            ", sd =", input$sd_hist, "\n")
      }
      
      else if (input$sub == "Корреляция") {
        x <- parse_numbers(input$x_cor)
        y <- parse_numbers(input$y_cor)
        if (length(x) != length(y) || length(x) < 2) {
          cat("😿 Наборы X и Y должны быть одинаковой длины (≥ 2)\n")
          cat("X:", length(x), "элементов | Y:", length(y), "элементов\n")
        } else {
          r <- cor(x, y)
          cat("🐱 X:", x, "\n")
          cat("🐱 Y:", y, "\n\n")
          cat("🌸 Корреляция Пирсона:", round(r, 4), "\n")
          if (abs(r) > 0.7) cat("🌷 Сильная связь!\n")
          else if (abs(r) > 0.3) cat("🌷 Умеренная связь\n")
          else cat("🌷 Слабая связь\n")
        }
      }
      
      # 💰 ФИНАНСЫ
      else if (input$sub == "Сложный процент") {
        P <- input$P; r <- input$r_sp / 100; n <- input$n_sp
        S <- P * (1 + r)^n
        profit <- S - P
        cat("💰 Начальная сумма:", P, "\n")
        cat("🌸 Ставка:", input$r_sp, "% годовых\n")
        cat("🌸 Срок:", n, "лет\n\n")
        cat("🌷 Итог:", round(S, 2), "\n")
        cat("🌷 Прибыль:", round(profit, 2), "\n")
      }
      
      else if (input$sub == "Ипотека (ануитет)") {
        S <- input$S_mort
        r <- input$r_mort / 100 / 12
        n <- input$n_mort * 12
        if (r == 0) {
          payment <- S / n
        } else {
          payment <- S * r / (1 - (1 + r)^(-n))
        }
        total <- payment * n
        overpay <- total - S
        cat("💰 Сумма кредита:", S, "\n")
        cat("🌸 Годовая ставка:", input$r_mort, "%\n")
        cat("🌸 Срок:", input$n_mort, "лет (", n, "месяцев)\n\n")
        cat("🌷 Ежемесячный платёж:", round(payment, 2), "\n")
        cat("🌷 Всего выплатишь:", round(total, 2), "\n")
        cat("🌷 Переплата:", round(overpay, 2), "\n")
      }
      
      else if (input$sub == "Дисконтирование") {
        FV <- input$FV
        r <- input$r_disc / 100
        n <- input$n_disc
        PV <- FV / (1 + r)^n
        cat("💰 Будущая стоимость:", FV, "\n")
        cat("🌸 Ставка:", input$r_disc, "%\n")
        cat("🌸 Через", n, "лет\n\n")
        cat("🌷 Текущая стоимость:", round(PV, 2), "\n")
        cat("🌷 Дисконт:", round(FV - PV, 2), "\n")
      }
    })
  })
  
  # --- График ---
  output$plot <- renderPlot({
    req(input$run > 0)
    if (input$sub == "Гистограмма") {
      data <- rnorm(input$n_hist, mean = input$mean_hist, sd = input$sd_hist)
      par(bg = "#FFF8DC")
      hist(data,
           col = "#FFB6C1",
           border = "white",
           main = "🌸 Гистограмма 🌸",
           xlab = "Значения",
           ylab = "Частота",
           col.main = "#FF69B4",
           col.lab = "#888")
      abline(v = mean(data), col = "#FF69B4", lwd = 2, lty = 2)
      legend("topright",
             legend = c("Среднее"),
             col = "#FF69B4", lty = 2, lwd = 2,
             bg = "white", box.col = "#FFB6C1")
    }
  })
}

# --------------------------------------------
# ЗАПУСК
# --------------------------------------------
shinyApp(ui, server)