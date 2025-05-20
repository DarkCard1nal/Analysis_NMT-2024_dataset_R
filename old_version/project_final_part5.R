# Завантажуємо наш датасет
my_data <- read.csv("/Users/artem/Documents/R/Odata2024File.csv", encoding = "UTF-8", header = TRUE, sep = ";")

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

#----- ПЕРЕВІРКА ГІПОТЕЗ -----

# Гіпотеза 1: «Якщо у людини високі оцінки з одного філологічного предмету 
# (наприклад, з англійської мови), то з іншого філологічного предмету 
# (наприклад, з української мови) в неї, скоріш за все, також високі оцінки.»

# Видаляємо пропущені значення
data_lang <- na.omit(my_data[, c("EngBlockBall100", "UkrBlockBall100")])

# --- 1. T-тест ---
# Створимо групу: висока оцінка з англійської > 180
data_lang$high_eng <- ifelse(data_lang$EngBlockBall100 >= 180, "high", "low")
t_test_1 <- t.test(UkrBlockBall100 ~ high_eng, data = data_lang)
print(t_test_1)

# p-value < 2.2e-16 - різниця між середніми оцінками значуща.

# mean high_eng ≈ 172.6, mean low_eng ≈ 152.5 - отже, в тих, хто добре знає англійську, 
# в середньому на 20 балів вище з української.

# --- 2. ANOVA ---
anova_1 <- aov(UkrBlockBall100 ~ high_eng, data = data_lang)
summary(anova_1)

# Аналогічно: значуща різниця між групами (p < 2e-16).
# Підтверджує T-тест: фактор "high_eng" впливає на оцінку з української.

# --- 3. x-квадрат тест ---
# Категоризація обох предметів
data_lang$eng_cat <- ifelse(data_lang$EngBlockBall100 >= 180, "high", "low")
data_lang$ukr_cat <- ifelse(data_lang$UkrBlockBall100 >= 180, "high", "low")
table_lang <- table(data_lang$eng_cat, data_lang$ukr_cat)
chisq_test_1 <- chisq.test(table_lang)
print(chisq_test_1)

# Обидва предмети поділено на "високі/низькі".
# X^2 = 9446, p < 2.2e-16 - є залежність між високими оцінками з англійської та української.

# --- 4. Кореляція Пірсона ---
cor_test_1 <- cor.test(data_lang$EngBlockBall100, data_lang$UkrBlockBall100, method = "pearson")
print(cor_test_1)

# Кореляція: r = 0.475 - помірний прямий зв'язок.
# Значущість підтверджена (p < 2.2e-16).

# Висновок: Успішність в англійській пов’язана з успішністю в українській. Усі методи це підтвердили.

# Гіпотеза 2: «Якщо у людини високі оцінки з одного технічного предмету (наприклад, з фізики), 
# то з іншого технічного предмету (наприклад, з математики) в неї, скоріш за все, також високі оцінки.»

# Видаляємо пропущення
data_tech <- na.omit(my_data[, c("PhysBlockBall100", "MathBlockBall100")])

# --- 1. T-тест ---
data_tech$high_phys <- ifelse(data_tech$PhysBlockBall100 >= 180, "high", "low")
t_test_2 <- t.test(MathBlockBall100 ~ high_phys, data = data_tech)
print(t_test_2)

# p-value < 2.2e-16
# Середні:
# high_phys ≈ 189.0
# low_phys ≈ 123.6
# Різниця в середньому балів: ~65 балів!

# --- 2. ANOVA ---
anova_2 <- aov(MathBlockBall100 ~ high_phys, data = data_tech)
summary(anova_2)

# F = 495.8, p < 2e-16 - значуща різниця.
# Потужний вплив "high_phys" на оцінку з математики.

# --- 3. x-квадрат тест ---
data_tech$phys_cat <- ifelse(data_tech$PhysBlockBall100 >= 180, "high", "low")
data_tech$math_cat <- ifelse(data_tech$MathBlockBall100 >= 180, "high", "low")
table_tech <- table(data_tech$phys_cat, data_tech$math_cat)
chisq_test_2 <- chisq.test(table_tech)
print(chisq_test_2)

# X^2 = 1783.6, p < 2.2e-16 → існує залежність між категоріями "високо/низько".

# --- 4. Кореляція Спірмена ---
cor_test_2 <- cor.test(data_tech$PhysBlockBall100, data_tech$MathBlockBall100, method = "spearman")
print(cor_test_2)

# rho = 0.709 - сильна позитивна кореляція.
# Це ще кращий результат, ніж у філологічній гіпотезі.

# Висновок: Дуже сильна залежність — ті, хто сильні у фізиці, зазвичай сильні й у математиці.

# Гіпотеза 3: «Через більш доступну освіту люди з міст мають у середньому вищі оцінки з 
# української мови, ніж люди з сільської місцевості.»

# Вибираємо лише потрібні дані
data_geo <- subset(my_data, TerTypeName %in% c(2, 3))
data_geo <- na.omit(data_geo[, c("UkrBlockBall100", "TerTypeName")])

# --- 1. T-тест ---
t_test_3 <- t.test(UkrBlockBall100 ~ TerTypeName, data = data_geo)
print(t_test_3)

# Середні бали:
# місто ≈ 147.0
# село ≈ 143.4
# Різниця: ~3.6 бала, p < 2.2e-16 — статистично значуща, але невелика.

# --- 2. ANOVA ---
anova_3 <- aov(UkrBlockBall100 ~ TerTypeName, data = data_geo)
summary(anova_3)

# F = 1719, p < 2e-16 — підтверджує, що тип місцевості впливає на оцінку.

# --- 3. x-квадрат тест ---
# Категоризуємо бали
data_geo$ukr_cat <- ifelse(data_geo$UkrBlockBall100 >= 180, "high", "low")
table_geo <- table(data_geo$TerTypeName, data_geo$ukr_cat)
chisq_test_3 <- chisq.test(table_geo)
print(chisq_test_3)

# X^2 = 317.2, p < 2.2e-16 — наявна асоціація між типом території та шансом мати високий бал.

# --- 4. Лінійна регресія ---
lm_model <- lm(UkrBlockBall100 ~ TerTypeName, data = data_geo)
summary(lm_model)

# Коефіцієнт при TerTypeName = -3.62, тобто село ≈ -3.6 бала нижче за місто.
# R^2 = 0.006 - вплив на результат є, але не дуже сильний (тип місцевості пояснює лише 0.6% варіації).

# Висновок: Міські учні мають трохи вищі бали, але ефект незначний по силі, хоч і статистично значущий.

# У ході дослідження було перевірено три гіпотези, пов’язані з результатами учасників НМТ 2024 року:
# 1. Філологічна гіпотеза: Високі бали з англійської мови асоціюються з високими балами з української. 
# # Усі методи (t-тест, ANOVA, χ²-тест, кореляція Пірсона) показали статистично значущий зв’язок 
# (p-value < 0.001). Кореляція Пірсона = 0.47 вказує на помірний позитивний лінійний зв’язок.
# 2. Технічна гіпотеза: Високі бали з фізики пов’язані з високими балами з математики. 
# Результати усіх методів підтвердили гіпотезу (p-value < 0.001). 
# Спірманова кореляція = 0.71 вказує на сильний позитивний монотонний зв’язок.
# 3. Географічна гіпотеза: Учні з міських територій мають дещо вищі бали з української мови, 
# ніж учні з сіл. Хоча різниця у середніх балах невелика (~3.6 бала), вона статистично значуща 
# (усі p-value < 0.001). Проте коефіцієнт детермінації (R² ≈ 0.006) вказує, 
# що територіальний чинник слабко пояснює варіацію оцінок.
# Таким чином, перші дві гіпотези підтверджено з помірним/сильним ефектом, третю — лише частково, 
# з дуже слабким ефектом, хоч і статистично значущим.

# Обґрунтування використаних методів
# 1. T-тест
# Застосовано для порівняння середніх значень між двома незалежними групами 
# (наприклад, "високі" та "низькі" бали).
# Підходить для числових змінних з приблизно нормальним розподілом або великим розміром вибірки.
# 2. ANOVA
# Використовується для перевірки впливу факторної змінної (групи) на змінну-відповідь.
# Дає подібну інформацію, що й t-тест, але дозволяє розширення на більшу кількість груп.
# Дублює t-тест при 2 групах, але дає змогу оцінити загальну дисперсію та F-статистику.
# 3. X-квадрат тест
# Застосовано для категоріальних змінних (наприклад, "високий"/"низький" бал).
# Визначає, чи існує статистично значуща асоціація між двома категоріальними змінними.
# Не вимагає нормального розподілу, але потребує достатнього обсягу спостережень.
# 4. Кореляційний аналіз
# Пірсон — для вимірювання лінійного зв’язку між двома числовими змінними (як у гіпотезі 1).
# Спірман — (як аналог Пірсона) для оцінки монотонного зв’язку, стійкий до викидів і 
# не вимагає нормальності (як у гіпотезі 2, де є багато однакових балів).
# 5. Лінійна регресія
# Дозволяє оцінити вплив однієї змінної (тип населеного пункту) на іншу (бал з української).
# Дає змогу інтерпретувати ступінь зміни залежної змінної при зміні незалежної.
# Показує, наскільки добре незалежна змінна пояснює варіацію залежної (через R^2).

#----- РЕГРЕСІЙНИЙ АНАЛІЗ -----

# Визначаємо залежну змінну: результат з української мови (UkrBlockBall100)
# Незалежні змінні: тип території, стать, результати з інших предметів (математика, історія, англійська)
# Виключаємо учасників, які не подолали поріг (значення 0)

cat("1. У цій регресійній моделі ми розглядаємо лише учасників, які подолали пороговий бал з української мови.\n")
cat("2. Ми також враховуємо, що значення 0 в інших предметах (математика, історія, англійська) також означає непроходження порогового бала.\n")
cat("3. Тому ця модель відображає взаємозв'язки між балами для тих учасників, які успішно склали тести.\n")

library(car)  # для аналізу мультиколінеарності
library(dplyr)

# Вибираємо дані для регресійної моделі 
# Виключаємо учасників, які не подолали поріг з української мови
# Також враховуємо, що 0 у всіх BlockBall100 означає непроходження порога
reg_data <- my_data %>%
  filter(UkrBlockBall100 > 0) %>%  # Виключаємо тих, хто не подолав поріг з української
  # Замінюємо нульові значення в інших предметах на NA, оскільки вони також означають непроходження порогу
  mutate(
    MathBlockBall100 = ifelse(MathBlockBall100 == 0, NA, MathBlockBall100),
    HistBlockBall100 = ifelse(HistBlockBall100 == 0, NA, HistBlockBall100),
    EngBlockBall100 = ifelse(EngBlockBall100 == 0, NA, EngBlockBall100)
  ) %>%
  select(UkrBlockBall100, TerTypeName, SexTypeName, 
         MathBlockBall100, HistBlockBall100, EngBlockBall100) %>%
  na.omit()  # Видаляємо рядки з NA

# Перевіряємо розмір підготовленого набору даних
cat("Розмір датасету для регресійного аналізу:", nrow(reg_data), "рядків\n")

# Будуємо початкову модель з усіма змінними
model1 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                       MathBlockBall100 + HistBlockBall100 + EngBlockBall100, 
                     data = reg_data)

# Виводимо результати початкової моделі
summary(model1)

# Аналіз проблеми мультиколінеарності
vif_values <- vif(model1)
print("Фактори інфляції дисперсії (VIF):")
print(vif_values)

# Побудова кореляційної матриці для перевірки можливої мультиколінеарності
cor_matrix <- cor(reg_data[,c("UkrBlockBall100", "MathBlockBall100", 
                                             "HistBlockBall100", "EngBlockBall100")], 
                        use = "complete.obs")
print("Кореляційна матриця предикторів:")
print(cor_matrix)

# Оптимізація моделі: видалення змінних з високою мультиколінеарністю
# або незначущих предикторів
# На основі результатів VIF та кореляції, створюємо оптимізовану модель
# (припускаючи, що HistBlockBall100 має найбільшу кореляцію з іншими предикторами)
model2 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                       MathBlockBall100 + EngBlockBall100, 
                     data = reg_data)

# Порівняння моделей за допомогою ANOVA
anova(model2, model1)

# Перевірка залишків оптимізованої моделі
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Перевірка VIF для оптимізованої моделі
vif_values2 <- vif(model2)
print("VIF для оптимізованої моделі:")
print(vif_values2)

# Перевірка нормальності залишків за допомогою тесту Шапіро-Вілка на підвибірці
set.seed(123)  # для відтворюваності
resid_sample <- sample(residuals(model1), min(5000, length(residuals(model2))))
shapiro_test <- shapiro.test(resid_sample)
print("Тест Шапіро-Вілка на нормальність залишків (підвибірка):")
print(shapiro_test)

# Графічні методи перевірки нормальності
# Q-Q plot
qqnorm(residuals(model1), main = "Q-Q графік залишків")
qqline(residuals(model1), col = "red", lwd = 2)

# Гістограма залишків
hist(residuals(model1), breaks = 50, 
     main = "Гістограма залишків",
     xlab = "Залишки", 
     col = "lightblue", 
     border = "white",
     probability = TRUE)
curve(dnorm(x, mean = mean(residuals(model2)), sd = sd(residuals(model2))), 
      add = TRUE, col = "darkred", lwd = 2)

# Використання альтернативних тестів на нормальність
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)

# Тест Андерсена-Дарлінга
ad_test <- ad.test(residuals(model1))
print("Тест Андерсена-Дарлінга на нормальність залишків:")
print(ad_test)

# Тест Колмогорова-Смирнова
ks_test <- ks.test(residuals(model1), "pnorm", 
                         mean = mean(residuals(model1)), 
                         sd = sd(residuals(model1)))
print("Тест Колмогорова-Смирнова на нормальність залишків:")
print(ks_test)

# Перевірка моделі з взаємодією між статтю та балами з математики
model3 <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
               MathBlockBall100 + HistBlockBall100 + EngBlockBall100 + 
               SexTypeName:MathBlockBall100,
             data = reg_data
)

# Порівнюємо моделі з та без взаємодії
anova(model1, model3)
summary(model3)

# Визначаємо фінальну модель на основі результатів
final_model <- model1

# Якщо взаємодія виявилась значущою, використовуємо модель з взаємодією
if (anova(model1, model3)$"Pr(>F)"[2] < 0.05) {
  final_model <- model3
  cat("Використовуємо модель з взаємодією як фінальну\n")
} else {
  cat("Використовуємо модель без взаємодії як фінальну\n")
}

# Отримуємо коефіцієнти та метрики для фінальної моделі
summary_final <- summary(final_model)
print("Підсумок фінальної моделі:")
print(summary_final)

# Обчислюємо стандартизовані коефіцієнти для порівняння важливості предикторів
# Стандартизація змінних
reg_data_scaled <- reg_data
reg_data_scaled[, c("MathBlockBall100", "HistBlockBall100", "EngBlockBall100")] <- 
  scale(reg_data_scaled[, c("MathBlockBall100", "HistBlockBall100", "EngBlockBall100")])

# Модель зі стандартизованими предикторами
model_scaled <- lm(UkrBlockBall100 ~ TerTypeName + SexTypeName + 
                           MathBlockBall100 + HistBlockBall100 + EngBlockBall100, 
                         data = reg_data_scaled)

# Виводимо коефіцієнти стандартизованої моделі
print("Коефіцієнти стандартизованої моделі:")
print(summary(model_scaled)$coefficients)

# Розрахунок прогнозованих значень та довірчих інтервалів
predicted <- predict(final_model, interval = "confidence")
head(predicted)

# Графік фактичних проти прогнозованих значень
plot(reg_data$UkrBlockBall100, predicted[,"fit"], 
     xlab = "Фактичні значення UkrBlockBall100", 
     ylab = "Прогнозовані значення",
     main = "Порівняння фактичних і прогнозованих значень", 
     asp = 1)
abline(0, 1, col = "red", lwd = 2)  # лінія y=x

# Оцінка важливості змінних через крос-валідацію
set.seed(123)
train_indices <- sample(nrow(reg_data), size = 0.7*nrow(reg_data))
train_data <- reg_data[train_indices, ]
test_data <- reg_data[-train_indices, ]

# Навчання моделі на навчальній вибірці
train_model <- lm(formula(final_model), data = train_data)

# Оцінка на тестовій вибірці
predictions <- predict(train_model, newdata = test_data)
test_mse <- mean((test_data$UkrBlockBall100 - predictions)^2)
test_rmse <- sqrt(test_mse)
test_r2 <- 1 - sum((test_data$UkrBlockBall100 - predictions)^2) / 
                   sum((test_data$UkrBlockBall100 - mean(test_data$UkrBlockBall100))^2)

print("Результати оцінки на тестовій вибірці:")
cat("MSE:", test_mse, "\n")
cat("RMSE:", test_rmse, "\n")
cat("R²:", test_r2, "\n")

# Підсумок регресійного аналізу
cat("\n--- ПІДСУМОК РЕГРЕСІЙНОГО АНАЛІЗУ ---\n")
cat("R-квадрат фінальної моделі:", round(summary_final$r.squared, 4), "\n")
cat("Скоригований R-квадрат:", round(summary_final$adj.r.squared, 4), "\n")
cat("F-статистика:", round(summary_final$fstatistic[1], 2), 
    "з p-value <", format.pval(pf(summary_final$fstatistic[1], 
                                summary_final$fstatistic[2], 
                                summary_final$fstatistic[3], 
                                lower.tail = FALSE), digits = 3), "\n")

# Інтерпретація результатів регресійного аналізу
cat("\n--- ІНТЕРПРЕТАЦІЯ РЕЗУЛЬТАТІВ ---\n")
cat("1. Модель пояснює приблизно", 
    round(summary_final$r.squared * 100, 1), 
    "% варіації результатів з української мови для учасників.\n")

cat("2. Найбільш впливові фактори (за абсолютною величиною коефіцієнтів):\n")
coef_abs <- abs(coef(final_model))
coef_abs <- coef_abs[-1]  # видаляємо перехват
coef_names <- names(coef_abs)
sorted_indices <- order(coef_abs, decreasing = TRUE)
for (i in sorted_indices) {
  cat("   - ", coef_names[i], ": ", 
      ifelse(coef(final_model)[coef_names[i]] > 0, "позитивний", "негативний"), 
      " вплив\n", sep = "")
}

cat("3. Взаємодія між предикторами:", 
    ifelse("SexTypeName:MathBlockBall100" %in% names(coef(final_model)), 
           "присутня і значуща", "не значуща або відсутня"), "\n")

cat("4. Перевірка припущень регресійного аналізу:", 
    ifelse(shapiro_test$p.value > 0.05, 
           "розподіл залишків близький до нормального", 
           "розподіл залишків відхиляється від нормального"), "\n")

cat("5. Мультиколінеарність:", 
    ifelse(max(vif_values2) < 5, 
           "відсутня проблема мультиколінеарності", 
           "наявна проблема мультиколінеарності"), "\n")

#----- ЛОГІСТИЧНИЙ АНАЛІЗ -----

# - Визначення бінарної залежної змінної та предикоторів незалежних змінних -
# Залежна змінна:
# 1. Результат проходження НМТ з української мови: якщо результат >= 180, то "1", інакше - "0".
# Незалежні змінні:
# 1. Результат проходження НМТ з математики: якщо результат >= 180, то "1", інакше - "0".
# 2. Результат проходження НМТ з історії України: якщо результат >= 180, то "1", інакше - "0".
# 3. Стать учасинка НМТ: 0 - "жіноча", 1 - "чоловіча".
# 4. Місце проходження НМТ: 0 - "місто", 1 - "селище, село"

my_data$UkrBallBinary <- ifelse(my_data$UkrBlockBall100 >= 180, 1, 0)

my_data$MathBallBinary <- ifelse(my_data$MathBlockBall100 >= 180, 1, 0)

my_data$HistBallBinary <- ifelse(my_data$HistBlockBall100 >= 180, 1, 0)

# Раніше значення SexTypeName були перекодовані наступним чином:
# 1 - "жіноча",
# 2 - "чоловіча".
# Для створення бінарної змінної було прийнято рішення виконати змінення значень:
# 0 - "жіноча" (замість 1)
# 1 - "чоловіча" (замість 2)

# Також раніше значення TerTypeName були перекодовані наступним чином:
# 1 - "інша країна",
# 2 - "місто",
# 3 - "селище, село".
# Для створення бінарної змінної було прийнято рішення виконати змінення значень:
# 2 -> 0 ("місто")
# 3 -> 1 ("селище, село")
need_data <- subset(my_data, SexTypeName %in% c(1, 2) & TerTypeName %in% c(2, 3))
need_data$SexTypeBinary <- ifelse(need_data$SexTypeName == 2, 1, 0)
need_data$TerTypeBinary <- ifelse(need_data$TerTypeName == 2, 0, 1)

# - Використовуємо логістичну регресію для моделювання бінарної залежності - 

# Обрання ствопців, що будуть приймати участь у моделюванні
logistic_data <- need_data[, c("UkrBallBinary", "MathBallBinary", "HistBallBinary", "TerTypeBinary", "SexTypeBinary")]

# Видалення рядків із NA, оскільки значення NA присутнє лише в тих учасників, що
# не зʼявилися на НМТ. Врахування цих даних призведе до погіршення результатів
# логістичного аналізу.
logistic_data <- na.omit(logistic_data)

View(logistic_data[1:10, ])

# Логістична регресія
log_model <- glm(UkrBallBinary ~ MathBallBinary + HistBallBinary + TerTypeBinary + SexTypeBinary, data = logistic_data, family = "binomial")

# - Оцінка моделі -
summary(log_model)

# Результати:
#Call:
#  glm(formula = UkrBallBinary ~ MathBallBinary + HistBallBinary + 
#        TerTypeBinary + SexTypeBinary, family = "binomial", data = logistic_data)
#
#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)    -3.08202    0.01422 -216.69   <2e-16 ***
#  MathBallBinary  2.81293    0.03471   81.05   <2e-16 ***
#  HistBallBinary  3.18957    0.02713  117.59   <2e-16 ***
#  TerTypeBinary  -0.35551    0.02768  -12.85   <2e-16 ***
#  SexTypeBinary  -1.20536    0.02351  -51.26   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 106437  on 280729  degrees of freedom
#Residual deviance:  77667  on 280725  degrees of freedom
#AIC: 77677
#
#Number of Fisher Scoring iterations: 7

#Інтерпретація отриманих результатів:
#Значення у стовпці Pr(>|z|) демонструють статистичну значущість змінних, що 
#були використані в ході моделювання бінарної залежності. 
#Також можна побачити, що отримання високих балів з математики та історії України
#підвищують шанси отримання високих балів з НМТ з української мови. 
#Місце складання НМТ має вплив на отримання високих результатів з предметів:
#учасники, що складали НМТ у селищі чи селі, мають менші шанси отримати високі результати,
#ніж ті, хто складав НМТ у містах.
#Також помітно, що учасники НМТ чоловічої статі мають менші шанси отримати високі бали
#з української мови, ніж учасники жіночої статі.

#Використання відношення шансів (Odds Ratio) для демонстрації звʼязку між впливом
#факторів та результатом.
exp(log_model$coefficients)

#Результати:
#(Intercept) MathBallBinary HistBallBinary  TerTypeBinary  SexTypeBinary 
#0.04586639    16.65871830    24.27791843     0.70081434     0.29958294 

#Інтерпретація отриманих результатів:
#Отримані результати відношення шансів підтверджують результати вже отриманих
#коефіцієнтів. Можна помітити, що успішне складання НМТ з математики (>=180 балів) 
#збільшує шанси отримання високих результатів з української мови у 16.65 разів.
#Успішне складання НМТ з історії України (>=180 балів) збільшує шанси отримання 
#високих результатів з української мови у 24.27 рази.
#Шанси отримання високих балів з української мови зменшуються на 30% в тих учасників,
#що складали НМТ у селищі чи селі.
#У чоловіків шанси отримати >=180 балів на НМТ з української мови на 70% нижчі,
#ніж в жінок.

# - Побудова ROC-кривої та отримання значення AUC -
#Встановлення необхідної бібліотеки
library(ROCR)

predictions <- predict(log_model, type = "response")
ROCRpred <- prediction(predictions, logistic_data$UkrBallBin)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

auc_perf <- performance(ROCRpred, "auc")
auc_perf@y.values

#Результати:
#[1] 0.8148797

#Інтерпретація отриманих результатів:
#На отриманому графіку можна побачити, що крива вигинається до лівого верхнього
#кута. Це свідчить про високу чутливість моделі та її здатність до надання коректних
#результатів. Значення AUC на рівні 0.8148 вказує на високу здатність моделі 
#розрізняти позитивні та негативні класи. Тобто, модель із високою ймовірністю зможе
#передбачити, які учасники отримають >= 180 балів з НМТ з української мови.
