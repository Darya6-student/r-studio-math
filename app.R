library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Пользовательский интерфейс
ui <- page_sidebar(
  title = "📚 Обучение математике в R Studio — 30 задач",
  
  sidebar = sidebar(
    navset_card_tab(
      id = "tabs",
      
      # ===== ВКЛАДКА 1: ПРОИЗВОДНЫЕ (10 задач) =====
      nav_panel("📐 Производные", icon = bs_icon("calculator"),
                selectInput("deriv_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: f(x) = x² + 3x - 5" = "d1",
                              "Задача 2: f(x) = sin(x)" = "d2",
                              "Задача 3: f(x) = e^x" = "d3",
                              "Задача 4: f(x) = ln(x)" = "d4",
                              "Задача 5: f(x) = x³ - 2x² + x" = "d5",
                              "Задача 6: f(x) = cos(x) · e^x" = "d6",
                              "Задача 7: f(x) = √x" = "d7",
                              "Задача 8: f(x) = 1/x" = "d8",
                              "Задача 9: f(x) = x · ln(x)" = "d9",
                              "Задача 10: f(x) = sin(x²)" = "d10"
                            )
                ),
                numericInput("deriv_x", "Точка вычисления (x):", value = 2, min = 0.1, max = 10, step = 0.1),
                actionButton("calc_deriv", "Вычислить производную", class = "btn-primary")
      ),
      
      # ===== ВКЛАДКА 2: СТАТИСТИКА (10 задач) =====
      nav_panel("📊 Статистика", icon = bs_icon("graph-up"),
                selectInput("stat_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: Описательные статистики" = "s1",
                              "Задача 2: Гистограмма" = "s2",
                              "Задача 3: Boxplot" = "s3",
                              "Задача 4: Корреляция Пирсона" = "s4",
                              "Задача 5: T-тест" = "s5",
                              "Задача 6: Квартили" = "s6",
                              "Задача 7: Дисперсия" = "s7",
                              "Задача 8: Scatter plot" = "s8",
                              "Задача 9: Анализ выбросов" = "s9",
                              "Задача 10: Сравнение групп" = "s10"
                            )
                ),
                textAreaInput("stat_data", "Данные через запятую:", 
                              value = "12, 15, 18, 20, 22, 25, 28", height = "80px"),
                textAreaInput("stat_data2", "Вторая группа (если нужна):", 
                              value = "10, 14, 16, 19, 21", height = "80px"),
                actionButton("calc_stats", "Рассчитать", class = "btn-success")
      ),
      
      # ===== ВКЛАДКА 3: ФИНАНСЫ (10 задач) =====
      nav_panel("💰 Финансы", icon = bs_icon("currency-dollar"),
                selectInput("fin_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: Сложный процент" = "f1",
                              "Задача 2: Аннуитетный платёж" = "f2",
                              "Задача 3: NPV проекта" = "f3",
                              "Задача 4: Простой процент" = "f4",
                              "Задача 5: Будущая стоимость аннуитета" = "f5",
                              "Задача 6: Приведённая стоимость" = "f6",
                              "Задача 7: Срок вклада" = "f7",
                              "Задача 8: Эффективная ставка" = "f8",
                              "Задача 9: IRR" = "f9",
                              "Задача 10: Дифференцированный платёж" = "f10"
                            )
                ),
                
                # Поля для задачи 1
                conditionalPanel(condition = "input.fin_task == 'f1'",
                                 numericInput("f1_init", "Начальная сумма:", value = 10000),
                                 numericInput("f1_rate", "Ставка (%):", value = 8),
                                 numericInput("f1_years", "Срок (лет):", value = 5)
                ),
                
                # Поля для задачи 2
                conditionalPanel(condition = "input.fin_task == 'f2'",
                                 numericInput("f2_loan", "Сумма кредита:", value = 500000),
                                 numericInput("f2_rate", "Ставка (%):", value = 12),
                                 numericInput("f2_years", "Срок (лет):", value = 3)
                ),
                
                # Поля для задачи 3
                conditionalPanel(condition = "input.fin_task == 'f3'",
                                 textAreaInput("f3_flows", "Денежные потоки:", 
                                               value = "-100000, 30000, 35000, 40000, 45000", height = "80px"),
                                 numericInput("f3_rate", "Ставка дисконтирования (%):", value = 10)
                ),
                
                # Поля для задачи 4
                conditionalPanel(condition = "input.fin_task == 'f4'",
                                 numericInput("f4_init", "Начальная сумма:", value = 50000),
                                 numericInput("f4_rate", "Ставка (%):", value = 10),
                                 numericInput("f4_years", "Срок (лет):", value = 3)
                ),
                
                # Поля для задачи 5
                conditionalPanel(condition = "input.fin_task == 'f5'",
                                 numericInput("f5_pay", "Ежемесячный взнос:", value = 5000),
                                 numericInput("f5_rate", "Ставка (%):", value = 7),
                                 numericInput("f5_years", "Срок (лет):", value = 10)
                ),
                
                # Поля для задачи 6
                conditionalPanel(condition = "input.fin_task == 'f6'",
                                 numericInput("f6_future", "Будущая сумма:", value = 100000),
                                 numericInput("f6_rate", "Ставка (%):", value = 8),
                                 numericInput("f6_years", "Через сколько лет:", value = 5)
                ),
                
                # Поля для задачи 7
                conditionalPanel(condition = "input.fin_task == 'f7'",
                                 numericInput("f7_init", "Начальная сумма:", value = 10000),
                                 numericInput("f7_final", "Желаемая сумма:", value = 20000),
                                 numericInput("f7_rate", "Ставка (%):", value = 10)
                ),
                
                # Поля для задачи 8
                conditionalPanel(condition = "input.fin_task == 'f8'",
                                 numericInput("f8_nom", "Номинальная ставка (%):", value = 12),
                                 numericInput("f8_per", "Начислений в год:", value = 12)
                ),
                
                # Поля для задачи 9
                conditionalPanel(condition = "input.fin_task == 'f9'",
                                 textAreaInput("f9_flows", "Денежные потоки:", 
                                               value = "-100000, 25000, 30000, 35000, 40000, 45000", height = "80px")
                ),
                
                # Поля для задачи 10
                conditionalPanel(condition = "input.fin_task == 'f10'",
                                 numericInput("f10_loan", "Сумма кредита:", value = 600000),
                                 numericInput("f10_rate", "Ставка (%):", value = 15),
                                 numericInput("f10_months", "Срок (месяцев):", value = 24)
                ),
                
                actionButton("calc_finance", "Рассчитать", class = "btn-warning")
      )
    )
  ),
  
  card(
    card_header(h3("📋 Результаты")),
    uiOutput("result_output")
  )
)

# Серверная логика
server <- function(input, output, session) {
  
  output$result_output <- renderUI({
    HTML("<div style='padding:15px; background:#f5f5f5; border-radius:8px; text-align:center;'>
          👈 Выберите задачу и нажмите кнопку расчёта</div>")
  })
  
  # ===== ПРОИЗВОДНЫЕ =====
  observeEvent(input$calc_deriv, {
    x <- input$deriv_x
    result <- switch(input$deriv_task,
                     "d1" = paste0("f(x) = x² + 3x - 5<br>f'(x) = 2x + 3<br>f'(", x, ") = <b>", 2*x+3, "</b>"),
                     "d2" = paste0("f(x) = sin(x)<br>f'(x) = cos(x)<br>f'(", round(x,2), ") = <b>", round(cos(x),4), "</b>"),
                     "d3" = paste0("f(x) = e^x<br>f'(x) = e^x<br>f'(", round(x,2), ") = <b>", round(exp(x),4), "</b>"),
                     "d4" = paste0("f(x) = ln(x)<br>f'(x) = 1/x<br>f'(", round(x,2), ") = <b>", round(1/x,4), "</b>"),
                     "d5" = paste0("f(x) = x³ - 2x² + x<br>f'(x) = 3x² - 4x + 1<br>f'(", x, ") = <b>", 3*x^2-4*x+1, "</b>"),
                     "d6" = paste0("f(x) = cos(x)·e^x<br>f'(x) = e^x(cos(x)-sin(x))<br>f'(", round(x,2), ") = <b>", round(exp(x)*(cos(x)-sin(x)),4), "</b>"),
                     "d7" = paste0("f(x) = √x<br>f'(x) = 1/(2√x)<br>f'(", round(x,2), ") = <b>", round(1/(2*sqrt(x)),4), "</b>"),
                     "d8" = paste0("f(x) = 1/x<br>f'(x) = -1/x²<br>f'(", round(x,2), ") = <b>", round(-1/x^2,4), "</b>"),
                     "d9" = paste0("f(x) = x·ln(x)<br>f'(x) = ln(x) + 1<br>f'(", round(x,2), ") = <b>", round(log(x)+1,4), "</b>"),
                     "d10" = paste0("f(x) = sin(x²)<br>f'(x) = 2x·cos(x²)<br>f'(", round(x,2), ") = <b>", round(2*x*cos(x^2),4), "</b>")
    )
    output$result_output <- renderUI({
      HTML(paste0("<div style='padding:15px; background:#e3f2fd; border-radius:8px;'><h4>📐 Производная</h4>", result, "</div>"))
    })
  })
  
  # ===== СТАТИСТИКА =====
  observeEvent(input$calc_stats, {
    d1 <- na.omit(as.numeric(unlist(strsplit(input$stat_data, ","))))
    d2 <- na.omit(as.numeric(unlist(strsplit(input$stat_data2, ","))))
    
    if (length(d1) < 2) {
      output$result_output <- renderUI({
        HTML("<div style='padding:15px; background:#ffebee; color:red;'>❌ Минимум 2 числа</div>")
      })
      return()
    }
    
    result <- switch(input$stat_task,
                     "s1" = paste0("<b>Описательные статистики:</b><br>",
                                   "Среднее: ", round(mean(d1),2), "<br>Медиана: ", round(median(d1),2),
                                   "<br>Ст.откл: ", round(sd(d1),2), "<br>Дисперсия: ", round(var(d1),2),
                                   "<br>Мин: ", min(d1), ", Макс: ", max(d1), "<br>N = ", length(d1)),
                     
                     "s2" = "Смотрите гистограмму ниже 👇",
                     "s3" = "Смотрите boxplot ниже 👇",
                     
                     "s4" = {
                       cor_val <- cor(d1, d2)
                       paste0("<b>Корреляция Пирсона:</b> ", round(cor_val, 4), "<br>",
                              if(abs(cor_val)>0.7) "Сильная связь" else if(abs(cor_val)>0.4) "Средняя связь" else "Слабая связь")
                     },
                     
                     "s5" = {
                       tt <- t.test(d1, d2)
                       paste0("<b>T-тест:</b><br>t = ", round(tt$statistic,4),
                              "<br>p-value = ", round(tt$p.value,4),
                              "<br>", if(tt$p.value<0.05) "✅ Различия значимы" else "⚠️ Различия незначимы")
                     },
                     
                     "s6" = paste0("<b>Квартили:</b><br>Q1 = ", round(quantile(d1,0.25),2),
                                   "<br>Q2 (медиана) = ", round(median(d1),2),
                                   "<br>Q3 = ", round(quantile(d1,0.75),2),
                                   "<br>IQR = ", round(IQR(d1),2)),
                     
                     "s7" = paste0("<b>Дисперсия:</b> ", round(var(d1),4),
                                   "<br><b>Ст.отклонение:</b> ", round(sd(d1),4)),
                     
                     "s8" = "Смотрите scatter plot ниже 👇",
                     
                     "s9" = {
                       q1 <- quantile(d1, 0.25); q3 <- quantile(d1, 0.75)
                       iqr <- q3 - q1
                       out <- d1[d1 < q1-1.5*iqr | d1 > q3+1.5*iqr]
                       paste0("<b>Выбросы:</b> ", if(length(out)==0) "не найдены" else paste(out, collapse=", "))
                     },
                     
                     "s10" = paste0("<b>Сравнение групп:</b><br>Группа 1: среднее = ", round(mean(d1),2),
                                    "<br>Группа 2: среднее = ", round(mean(d2),2),
                                    "<br>Разница: ", round(mean(d1)-mean(d2),2))
    )
    
    # График
    df <- data.frame(values = d1)
    p <- switch(input$stat_task,
                "s1" = ggplot(df, aes(values)) + geom_histogram(fill="#667eea", bins=10) + theme_minimal() + labs(title="Распределение"),
                "s2" = ggplot(df, aes(values)) + geom_histogram(fill="#764ba2", bins=15) + theme_minimal() + labs(title="Гистограмма"),
                "s3" = ggplot(df, aes(y=values)) + geom_boxplot(fill="#667eea") + theme_minimal() + labs(title="Boxplot"),
                "s4" = {df2 <- data.frame(x=d1, y=d2); ggplot(df2, aes(x,y)) + geom_point(color="#764ba2", size=3) + theme_minimal() + labs(title="Корреляция")},
                "s5" = {df_c <- data.frame(v=c(d1,d2), g=rep(c("Гр1","Гр2"), c(length(d1),length(d2)))); ggplot(df_c, aes(g,v,fill=g)) + geom_boxplot() + theme_minimal()},
                "s6" = ggplot(df, aes(y=values)) + geom_boxplot(fill="#667eea") + theme_minimal() + labs(title="Квартили"),
                "s7" = ggplot(df, aes(values)) + geom_histogram(fill="#764ba2", bins=10) + theme_minimal(),
                "s8" = {df2 <- data.frame(x=d1, y=d2); ggplot(df2, aes(x,y)) + geom_point(color="#667eea", size=3) + theme_minimal() + labs(title="Scatter")},
                "s9" = ggplot(df, aes(y=values)) + geom_boxplot(fill="#764ba2") + theme_minimal() + labs(title="Выбросы"),
                "s10" = {df_c <- data.frame(v=c(d1,d2), g=rep(c("Гр1","Гр2"), c(length(d1),length(d2)))); ggplot(df_c, aes(g,v,fill=g)) + geom_boxplot() + theme_minimal()}
    )
    
    output$result_output <- renderUI({
      tagList(
        div(style="padding:15px; background:#e8f5e9; border-radius:8px;", HTML(result)),
        div(style="margin-top:20px;", plotOutput("stat_plot", height="400px"))
      )
    })
    output$stat_plot <- renderPlot({ print(p) })
  })
  
  # ===== ФИНАНСЫ =====
  observeEvent(input$calc_finance, {
    result <- switch(input$fin_task,
                     "f1" = {
                       fv <- input$f1_init * (1 + input$f1_rate/100)^input$f1_years
                       paste0("<b>Сложный процент:</b><br>Будущая стоимость: <b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b><br>Прибыль: ", format(round(fv-input$f1_init,2), big.mark=" "), " руб.")
                     },
                     "f2" = {
                       mr <- input$f2_rate/100/12; m <- input$f2_years*12
                       pay <- input$f2_loan * (mr*(1+mr)^m) / ((1+mr)^m - 1)
                       paste0("<b>Аннуитет:</b><br>Ежемесячный платёж: <b style='color:blue;'>", format(round(pay,2), big.mark=" "), " руб.</b><br>Переплата: ", format(round(pay*m - input$f2_loan,2), big.mark=" "), " руб.")
                     },
                     "f3" = {
                       cf <- as.numeric(unlist(strsplit(input$f3_flows, ","))); r <- input$f3_rate/100
                       npv <- sum(cf / (1+r)^(0:(length(cf)-1)))
                       paste0("<b>NPV:</b> ", format(round(npv,2), big.mark=" "), " руб.<br>", if(npv>0) "✅ Выгодно" else "❌ Не выгодно")
                     },
                     "f4" = {
                       fv <- input$f4_init * (1 + input$f4_rate/100 * input$f4_years)
                       paste0("<b>Простой процент:</b><br>Итого: <b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b>")
                     },
                     "f5" = {
                       mr <- input$f5_rate/100/12; m <- input$f5_years*12
                       fv <- input$f5_pay * (((1+mr)^m - 1)/mr)
                       paste0("<b>Будущая стоимость аннуитета:</b><br><b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b>")
                     },
                     "f6" = {
                       pv <- input$f6_future / (1 + input$f6_rate/100)^input$f6_years
                       paste0("<b>Приведённая стоимость:</b><br><b style='color:blue;'>", format(round(pv,2), big.mark=" "), " руб.</b>")
                     },
                     "f7" = {
                       y <- log(input$f7_final / input$f7_init) / log(1 + input$f7_rate/100)
                       paste0("<b>Срок вклада:</b><br><b style='color:green;'>", round(y,2), " лет</b>")
                     },
                     "f8" = {
                       eff <- (1 + input$f8_nom/100/input$f8_per)^input$f8_per - 1
                       paste0("<b>Эффективная ставка:</b><br><b style='color:blue;'>", round(eff*100,2), "%</b>")
                     },
                     "f9" = {
                       cf <- as.numeric(unlist(strsplit(input$f9_flows, ",")))
                       irr <- 0.1
                       for(i in 1:100) { npv <- sum(cf/(1+irr)^(0:(length(cf)-1))); if(abs(npv)<1) break; irr <- irr - npv/10000 }
                       paste0("<b>IRR:</b><br><b style='color:green;'>", round(irr*100,2), "%</b>")
                     },
                     "f10" = {
                       mr <- input$f10_rate/100/12; m <- input$f10_months
                       pp <- input$f10_loan/m
                       first <- pp + input$f10_loan*mr
                       last <- pp + pp*mr
                       paste0("<b>Дифференцированный платёж:</b><br>Первый: ", format(round(first,2), big.mark=" "), " руб.<br>Последний: ", format(round(last,2), big.mark=" "), " руб.")
                     }
    )
    output$result_output <- renderUI({
      HTML(paste0("<div style='padding:15px; background:#fff3e0; border-radius:8px;'><h4>💰 Финансы</h4>", result, "</div>"))
    })
  })
}

shinyApp(ui, server)
install.packages(c("shiny", "ggplot2", "bslib", "bsicons"))
