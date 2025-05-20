# Завантажуємо наш датасет
my_data <- read.csv("C://analis//Odata2024File.csv", encoding = "UTF-8", header = TRUE, sep = ";")

# Перевірка структури даних, чи датасет нормально завантажився
head(my_data, 10)
View(my_data[1:10, ])

#----- ОПИС ДАТАСЕТУ -----
#Кількість рядків в нашому датасеті 
cat("В нашому датасеті", nrow(my_data), "рядків\n")

# кількість стовпців в нашому датасеті 
cat("В нашому датасеті", ncol(my_data), "стовпців\n")

# базові статистичні показники для кожної змінної
summary(my_data)

# структура даних - типи змінних і приклади значень
str(my_data)

# альтернативний перегляд структури з dplyr
library(dplyr)
glimpse(my_data)

#----- ОБРОБКА ДАНИХ -----
#під час перегляду структури даних було помічено що в датасеті замість NA 
# використовується null, що заважає коректно працювати з аналітичними функціями, 
# тому що null буде сприйматися як звичайні текстові дані, а не як пропущені 
# значення, що призведе до помилок у розрахунках та аналізі 

# перетворення null на NA
my_data[my_data == "null"] <- NA

# повторний аналіз даних після зміни null на NA
# базові статистичні показники для кожної змінної
summary(my_data)

# структура даних - типи змінних і приклади значень
str(my_data)

# альтернативний перегляд структури з dplyr
library(dplyr)
glimpse(my_data)

# заміна пройшла успішно, тепер можемо разувати кількість пропущених значень в датасеті

# перевірка пропущених значень
colSums(is.na(my_data))

# також під час перевірки структури датасету було помічено, що стовпець TestDate 
# має Class та Mode character, що не зовсім правильно, тому переведемо його в Date

str(my_data$TestDate)

my_data$TestDate <- as.Date(my_data$TestDate, format = "%d.%m.%Y")

str(my_data$TestDate)

# також під час перевірки структури датасету було помічено, що стовпці 
# UkrBlockBall100, UkrBlockBall, HistBlockBall100, HistBlockBall, MathBlockBall100, 
# MathBlockBall, PhysBlockBall100, PhysBlockBall, ChemBlockBall100, ChemBlockBall, 
# BioBlockBall100, BioBlockBall, GeoBlockBall100, GeoBlockBall, EngBlockBall100, 
# EngBlockBall, FraBlockBall100, FraBlockBall, DeuBlockBall100, DeuBlockBall, 
# SpaBlockBall100, SpaBlockBall, UkrLitBlockBall100, UkrLitBlockBall мають Class 
# та Mode character, що не зовсім правильно, тому переведемо його в integer

# Створюємо список стовпців, які потрібно конвертувати
cols_to_convert <- c("UkrBlockBall100", "UkrBlockBall", "HistBlockBall100", "HistBlockBall", 
                     "MathBlockBall100", "MathBlockBall", "PhysBlockBall100", "PhysBlockBall", 
                     "ChemBlockBall100", "ChemBlockBall", "BioBlockBall100", "BioBlockBall", 
                     "GeoBlockBall100", "GeoBlockBall", "EngBlockBall100", "EngBlockBall", 
                     "FraBlockBall100", "FraBlockBall", "DeuBlockBall100", "DeuBlockBall", 
                     "SpaBlockBall100", "SpaBlockBall", "UkrLitBlockBall100", "UkrLitBlockBall")

for (col in cols_to_convert) {
  print(str(my_data[[col]]))
}

# Застосовуємо конвертацію
my_data[, cols_to_convert] <- lapply(my_data[, cols_to_convert], function(x) {
  # оскільки деякі цифри в нас з комою, то замінюємо її на крапку
  x_fixed <- gsub(",", ".", x)
  # тепер можемо пеертворити character спочатку в numeric, а потім в integer
  as.integer(as.numeric(x_fixed))
})

for (col in cols_to_convert) {
  print(str(my_data[[col]]))
}

# Перевіряємо зміни
str(my_data)
summary(my_data)

#----- ВІЗУАЛІЗАЦІЯ РОЗПОДІЛУ ЗМІННИХ -----

library(ggplot2)

# Ми побудуємо два варіанти графіків — з урахуванням нулів і без них, щоб краще 
# зрозуміти наші дані з різних боків. Коли ми включаємо нульові значення, бачимо 
# повну картину, зокрема тих, хто не набрав жодного бала (не подолав поріг). Це 
# допомагає оцінити, який відсоток таких учасників і наскільки вони відрізняються 
# від решти.
# Якщо ж ми прибираємо всі нулі, то можемо детальніше побачити, як розподіляються 
# результати лише серед тих, хто подолав поріг. У такому випадку не буде сильного 
# «зсуву» чи окремого «шипа» на графіку, і ми чіткіше побачимо, де саме 
# зосереджені бали (скажімо, переважно в діапазоні від 140 до 150).

# Гістограма без нулів (тільки ті, хто подолав поріг)
# Створюємо підмножину даних без нульових значень
data_filtered <- my_data[my_data$UkrBlockBall100 > 0, ]

not_passed <- sum(my_data$UkrBlockBall100 == 0, na.rm = TRUE)
total <- sum(!is.na(my_data$UkrBlockBall100))
percent_not_passed <- round(not_passed / total * 100, 1)

ggplot(data_filtered, aes(x = UkrBlockBall100)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Розподіл балів UkrBlockBall100 (без нулів)",
       subtitle = paste(not_passed, "учасників (", percent_not_passed, "%) не подолали поріг"),
       x = "Бали", y = "Частота") +
  theme_minimal()

# На цій гістограмі ми бачимо розподіл балів тих учасників, які подолали поріг, 
# тобто отримали більше 0. Найвищий стовпчик розташований у районі 140–150 
# балів, і можна сказати, що найбільша частка учасників отримала оцінки саме в 
# цьому діапазоні. Загалом результати досить щільно згруповані приблизно від 110 
# до 180 балів, що вказує на відносно «центрований» розподіл без яскраво 
# вираженого зміщення
# При цьому зверху на графіку зазначено, що лише 0,4% (тобто 1218 учасників) 
# від загальної кількості отримали 0. Таким чином, більшість учасників 
# успішно подолали мінімальний пороговий бал, а розподіл результатів переважно 
# зосереджений у середньому та високому діапазоні.

# Коробкова діаграма без нулів
ggplot(data_filtered, aes(y = UkrBlockBall100)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Коробкова діаграма UkrBlockBall100 (без нулів)",
       subtitle = paste(not_passed, "учасників (", percent_not_passed, "%) не подолали поріг"),
       y = "Бали") +
  theme_minimal()

# На цій коробковій діаграмі ми бачимо розподіл балів серед тих, хто подолав поріг, 
# тобто хто має бали,  вищі за 0 . Коробка розташована приблизно в межах від  
# 130 до 150 , а медіана знаходиться близько 140–145  балів.  
# Важливо, що на цій діаграмі вже немає нульових балів, оскільки ми відсіяли 
# учасників, які не подолали поріг (за легендою, їх було 0,4% від загальної кількості).  
# Таким чином, цей графік дозволяє побачити, як розподілилися бали в учасників, 
# які набрали принаймні 1 бал, і наочно показує, де знаходяться типовий діапазон 
# оцінок (коробка) та невелика кількість екстремальних значень.

# Графік розсіювання

# Для побудови графіка розсіювання візьмемо змінні MathBlockBall100 та UkrBlockBall100

ggplot(my_data, aes(x = UkrBlockBall100, y = MathBlockBall100)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Графік розсіювання: UkrBlockBall100 vs MathBlockBall100 (включаючи нулі)",
       x = "UkrBlockBall100",
       y = "MathBlockBall100") +
  theme_minimal()

# На осі X відкладені бали з української, а на осі Y — бали з математики.
# Бачимо, що частина учасників отримала 0 (не подолали поріг) з обох предметів 
# (точки, які примикають до осей).
# Основна «хмара» точок зосереджена в діапазоні 100–200 як по UkrBlockBall100, 
# так і по MathBlockBall100. Тут можна розгледіти, що чим вищі бали з одного 
# предмета, тим більша схильність до вищих балів з іншого (є певна позитивна 
# залежність).
# Є також невелика кількість точок, що сильно виділяються (наприклад, хтось 
# має 0 з одного предмета, але понад 150 чи 200 з іншого). Такі точки свідчать, 
# що учасник міг добре скласти один предмет, але повністю «провалити» інший.
# Загалом ми бачимо скупчення балів у верхньому правому куті (близько 150–200), 
# що вказує на чималу кількість учасників з високими балами з обох предметів.

#----- ПОБУДОВА КОРЕЛЯЦІЙНОЇ МАТРИЦІ -----
library(corrplot)

# Перетворення змінних стовпця 'TerTypeName' у числові
my_data <- my_data %>%
  mutate(city = ifelse(TerTypeName == "місто", 1, 0),
         village_or_town = ifelse(TerTypeName == "селище, село", 1, 0),
         other_country = ifelse(TerTypeName == "інша країна", 1, 0))

# Перетворення змінних стовпця 'SexTypeName' у числові
my_data <- my_data %>%
  mutate(male = ifelse(SexTypeName == "чоловіча", 1, 0),
         female = ifelse(SexTypeName == "жіноча", 1, 0))

# Видалення оригінальних категоріальних стовпців для уникнення перевантаження.
#my_data <- my_data %>% select(-TerTypeName, -SexTypeName)

# Фільтрування та обрання лише числових стовпців.
numeric_data <- my_data %>%
  select(where(is.numeric))

# Прибирання стовпців із великою кількістю пропущених значень для збільшення
#точності кореляції.
threshold <- 0.7  # залишаємо лише ті, де понад 70% значень не NA
valid_cols <- names(numeric_data)[colMeans(!is.na(numeric_data)) >= threshold]
cor_data <- numeric_data[, valid_cols]

# Видалення рядків із NA
cor_data <- na.omit(cor_data)

# Розрахунок кореляційної матриці
cor_matrix <- cor(cor_data, use = "complete.obs")

# Побудова кореляційної матриці
corrplot(cor_matrix, 
         method = "color",        
         type = "full",          
         addCoef.col = "black",   
         tl.col = "grey",        
         number.cex = 0.7,        
         col = colorRampPalette(c("purple", "white", "orange"))(200)  
)

#Побудова кореляційної матриці дозволила виявити певний звʼязок між ознаками.
#Прослідкована позитивна кореляція між місцевістю, в якій відбувалося складання
#НМТ учасником, та балами за складені предмети. Так можна припустити, що ті 
#учасники, що проходили НМТ в місті, отримували більшу кількість балів. 
#Також можна прослідкувати позитивну кореляцію між статтю учасника НМТ та результатами
#зі складених предметів. Можливим є припущення, що учасники жіночого роду в середньому
#мають трохи вищі бали за українську мові, порівняно з учасниками чоловічого роду.
#Також можна прослідкувати позитивну кореляцію між датою народження та результатами
#проходження НМТ, але цей звʼязок є досить слабким. Серед прикладів негативної кореляції
#можна виділити звʼязок між селищем як територією складання НМТ та результатами з математики,
#проте цей звʼязок є настільки слабким, що навряд чи впливає на результат.

#----- ЧИСТКА ТА ПІДГОТОВКА ДАНИХ -----

# ВИЯВЛЯЄМО ПРОПУЩЕНІ ЗНАЧЕННЯ

# Спочатку подивимось, скільки пропущених значень у нашому датасеті
sum(is.na(my_data))

# Аналіз відсотку пропущених значень
na_percentage <- round(colMeans(is.na(my_data)) * 100, 2)
na_df <- data.frame(Column = names(na_percentage), NA_Percentage = na_percentage)
na_df <- na_df[order(-na_df$NA_Percentage), ]
print(na_df[na_df$NA_Percentage > 0, ])

# Ми вирішили не підставляти жодних вигаданих оцінок замість пропусків, бо в нашому 
# випадку NA означає реальну відсутність учня на іспиті або вибір предмету, і 
# будь-яка спроба «заповнити» ці порожні клітинки середнім чи медіаною лише 
# спотворить справжню картину результатів. Якщо ми підсунемо штучні бали, то 
# втратимо інформацію про тих, хто не з’явився, і ризикуємо отримати хибні 
# висновки про успішність учнів. Краще залишити пропуски як вони є, щоб у 
# подальшому аналізі чітко розуміти, що ці дані відсутні з об’єктивних причин, 
# а не через технічну помилку. Це дозволяє зберегти достовірність дослідження 
# та уникнути непотрібних перекручувань у статистичних показниках.

#----- АНАЛІЗ ТА ОБРОБКА ВИКИДІВ -----

# Підготуємо лише числові дані
numeric_data <- my_data[, sapply(my_data, is.numeric)]

# Список максимальних балів для кожного BlockBall-стовпця
max_scores <- c(
  UkrBlockBall      = 45,
  UkrLitBlockBall   = 45,
  MathBlockBall     = 32,
  HistBlockBall     = 54,
  EngBlockBall      = 32,
  FraBlockBall      = 32,
  DeuBlockBall      = 32,
  SpaBlockBall      = 32,
  BioBlockBall      = 46,
  ChemBlockBall     = 40,
  PhysBlockBall     = 32,
  GeoBlockBall      = 46
)

# Шукаємо всі стовпці з “BlockBall” у назві
score_cols <- grep("BlockBall", names(numeric_data), value = TRUE)

# Функція виявлення викидів за доменними межами
detect_outliers <- function(x, col_name) {
  n <- length(x)
  outliers <- rep(FALSE, n)
  valid    <- !is.na(x)
  
  # BlockBall100: 0 – легітимно, ненульові мають бути в [100,200]
  if (grepl("BlockBall100$", col_name)) {
    outliers[ valid & x != 0 & (x < 100 | x > 200) ] <- TRUE
    return(list(outliers = outliers, lower = 100, upper = 200))
  }
  
  # Інші *BlockBall: доменні межі [0, max_scores[col_name]]
  if (col_name %in% names(max_scores)) {
    upper <- max_scores[col_name]
    # всі значення <0 або >upper вважаємо викидами
    outliers[ valid & (x < 0 | x > upper) ] <- TRUE
    return(list(outliers = outliers, lower = 0, upper = upper))
  }
  
  # всі інші – без аналізу
  list(outliers = outliers, lower = NA, upper = NA)
}

# Підготуємо таблицю для статистики нульових
zero_scores <- data.frame(
  Subject      = character(),
  Zero_Count   = integer(),
  Zero_Percent = numeric(),
  stringsAsFactors = FALSE
)

# Основний цикл по кожному предмету
for (col in score_cols) {
  vec <- numeric_data[[col]]
  
  # Підрахунок нулів
  zcount <- sum(vec == 0, na.rm = TRUE)
  total  <- sum(!is.na(vec))
  zpct   <- if (total > 0) round(zcount/total*100, 2) else NA
  zero_scores <- rbind(zero_scores, data.frame(
    Subject      = col,
    Zero_Count   = zcount,
    Zero_Percent = zpct,
    stringsAsFactors = FALSE
  ))
  
  # Виявлення викидів
  info      <- detect_outliers(vec, col)
  valid_cnt <- sum(!is.na(vec))
  out_cnt   <- sum(info$outliers, na.rm = TRUE)
  
  # Вивід результату
  if (grepl("BlockBall100$", col)) {
    cat(sprintf("%s: %d викидів (ненульові поза [100,200])\n",
                col, out_cnt))
  } else if (col %in% names(max_scores)) {
    cat(sprintf("%s: %d викидів (поза [0,%d])\n",
                col, out_cnt, max_scores[col]))
  } else {
    cat(sprintf("%s: аналіз доменних меж не застосовано\n", col))
  }
  
  # Winsorizing (якщо викидів <5% від валідних)
  if (col %in% names(max_scores) && valid_cnt > 0 &&
      out_cnt > 0 && out_cnt/valid_cnt < 0.05) {
    tmp <- my_data[[col]]
    idx <- which(info$outliers)
    for (i in idx) {
      if (!is.na(tmp[i])) {
        tmp[i] <- min(max(tmp[i], info$lower), info$upper)
      }
    }
    my_data[[col]] <- tmp
    cat(sprintf("  -> значення в %s обмежені до [%.1f, %.1f]\n",
                col, info$lower, info$upper))
  }
  
  cat("\n")
}

# Виводимо таблицю нульових балів
print(zero_scores)

# Ми провели перевірку всіх оцінок за двома групами предметів - ті, що мають шкалу 
# від 100 до 200, і ті, де максимальною межею є різні значення від 32 до 54. Для 
# предметів із «100-бальною» шкалою нулі вважаються звичайним результатом 
# (не подолав поріг), а всі оцінки поза інтервалом [100;200] розглядаються як 
# викиди. Як видно, жодних викидів немає, тому можна сказати, що всі записи в межах 
# шкали 100-200 вірні. Аналогічно, для предметів із меншою шкалою (UkrBlockBall – до 45, 
# MathBlockBall – до 32, HistBlockBall – до 54 тощо) усі бали перевіряються в діапазоні 
# від 0 до відповідного максимуму, і теж викидів не виявилося. При цьому нульові 
# бали (тобто учні, які не набрали жодного балу) трапляються нечасто – максимальна 
# частка таких спостережень становить близько 12,8 % у MathBlockBall100 та 10,2 % 
# у PhysBlockBall100, а в інших предметах цей показник не перевищує кількох відсотків. 

#----- ПЕРЕКОДУВАННЯ ЗМІННИХ: SexTypeName, TestDate, TerTypeName -----

# SexTypeName: character → factor → integer
my_data$SexTypeName <- as.factor(my_data$SexTypeName)
my_data$SexTypeName <- as.integer(my_data$SexTypeName)

# TestDate: character → Date
my_data$TestDate <- as.Date(my_data$TestDate)

# TerTypeName: character → factor
unique_tertypes <- unique(my_data$TerTypeName)
cat("Унікальні типи територій (TerTypeName):\n")
print(unique_tertypes)

# Перетворення TerTypeName у фактор і перекодування в числа
my_data$TerTypeName <- as.factor(my_data$TerTypeName)
my_data$TerTypeName <- as.integer(my_data$TerTypeName)

View(my_data[1:10, ])

# Ми перекодували деякі зі змінних в integer для зручної роботи з ними під час досліджень та 
# наступного аналізу. 
# SexTypeName має значення:
# 1 - "жіноча",
# 2 - "чоловіча".
# TerTypeName має значення:
# 1 - "інша країна",
# 2 - "місто",
# 3 - "селище, село".
# Для TestDate змінено формат з dd.mm.yyyy на yyyy-mm-dd для зручності роботи з датами
# Додаткова трансформація змінних не потрібна, оскільки більшість числових змінних — це бали за шкалою,
# вже інтерпретуються однозначно і не потребують додаткового масштабування чи перетворення. 
# Вони мають чітке смислове значення, яке втратиться після логарифмування або нормалізації.
# Логарифмічна трансформація — не підходить через нулі. 
# У змінних типу BlockBall або BlockBall100 можуть бути нулячі значення, що означають:
# або учень не подолав поріг,
# або не з’явився на іспит (пропущені значення).
# Якщо планується описова статистика або побудова частот/розподілів, то стандартизація 
# (переведення в Z-оцінки) або нормалізація (від 0 до 1) спотворює сенс шкал, що також нам не підходить.

