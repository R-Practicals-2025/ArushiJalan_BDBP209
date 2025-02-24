# Lab8 21st February by Arushi
#1
is_palindrome_int <- function(num) {
  num_str <- as.character(num)
  rev_num_str <- paste(rev(strsplit(num_str, NULL)[[1]]), collapse = "")
  return(num_str == rev_num_str)
}
test_num <- 12321
result <- is_palindrome_int(test_num)
print(result)
test_num2 <- 12345
result2 <- is_palindrome_int(test_num2)
print(result2)


#2
string <-"seemerightnow"
# a)
substring_a <- substr(string, 1, 3)
print(substring_a)
# b)
substring_b <- substr(string, 4, 5)
print(substring_b)
# c)
substring_c <- substr(string, 6, 10)
print(substring_c)


#3
dna_seq <- "ATTGCGCATAGTCCGGG"
count_G <- sum(strsplit(dna_seq, "")[[1]] == "G")
print(count_G)
count_C <- sum(strsplit(dna_seq, "")[[1]] == "C")
print(count_C)
gc_fraction <- (count_G + count_C) / nchar(dna_seq)
print(gc_fraction)

#4
is_palindromic_dna <- function(seq) {
  complement_map <- c(A = "T", T = "A", G = "C", C = "G")
  seq <- toupper(seq)
  complement_seq <- sapply(strsplit(seq, NULL)[[1]], function(base) complement_map[base])
  rev_complement_seq <- rev(complement_seq)
  return(identical(seq, paste(rev_complement_seq, collapse = "")))
}
test_seq <- "TGGATCCA"
result <- is_palindromic_dna(test_seq)
print(result)


#5
sentence <- "She sells hundreds of sea oysters on the sea shore."
words <- unlist(strsplit(gsub("[[:punct:]]", "", sentence), " "))
word_lengths <- nchar(words)
max_length <- max(word_lengths)
largest_words <- words[word_lengths == max_length]
print(paste("Largest word(s):", paste(largest_words, collapse = ", ")))

second_max_length <- max(word_lengths[word_lengths < max_length])
second_largest_words <- words[word_lengths == second_max_length]
print(paste("Second largest word(s):", paste(second_largest_words, collapse = ", ")))


#6
#a
worldfloras <- read.table("/home/ibab/Downloads/worldfloras.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
continent_data <- split(worldfloras, worldfloras$Continent)
for (continent in names(continent_data)) {
  assign(paste0("df_", gsub("[^a-zA-Z]", "_", continent)), continent_data[[continent]])
}
head(df_Africa)

#b
boxplot(worldfloras$Flora ~ worldfloras$Continent, main = "Floral Count Distribution by Continent",xlab = "Continent", ylab = "Flora Count", las = 2, col = rainbow(length(unique(worldfloras$Continent))))
flora_summary <- aggregate(Flora ~ Continent, data = worldfloras, summary)
print(flora_summary)
flora_stats <- aggregate(Flora ~ Continent, data = worldfloras, function(x) c(mean = mean(x), sd = sd(x)))
print(flora_stats)
library(moments)
skewness_kurtosis <- aggregate(Flora ~ Continent, data = worldfloras, function(x) c(skew = skewness(x), kurt = kurtosis(x)))
print(skewness_kurtosis)

#c
boxplot(worldfloras$Population ~ worldfloras$Continent, main = "Population Distribution by Continent",xlab = "Continent", ylab = "Population (millions)", las = 2, col = rainbow(length(unique(worldfloras$Continent))))
hist(worldfloras$Population, breaks = 20, col = "pink", main = "Population Distribution", xlab = "Population (millions)")
pop_summary <- aggregate(Population ~ Continent, data = worldfloras, summary)
print(pop_summary)
skewness_kurtosis_pop <- aggregate(Population ~ Continent, data = worldfloras, function(x) c(skew = skewness(x), kurt = kurtosis(x)))
print(skewness_kurtosis_pop)


#7
bones_data <- read.table("/home/ibab/Downloads/HumanBones.txt", header = FALSE,sep = "\t", stringsAsFactors = FALSE)
head(bones_data)
categories <- c()
bone_names <- c()
bone_numbers <- c()
current_category <- NULL
for (i in 1:nrow(bones_data)) {
  line <- bones_data$V1[i]
  if (!grepl("\\(", line)) {
    current_category <- line
  } else {
    bone_info <- strsplit(line, "\\(")[[1]]
    bone_name <- trimws(bone_info[1])
    bone_number <- gsub("[^0-9]", "", bone_info[2]) 
    categories <- c(categories, current_category)
    bone_names <- c(bone_names, bone_name)
    bone_numbers <- c(bone_numbers, as.numeric(bone_number))
  }
}
bones_info <- data.frame(category = categories, name_of_bone = bone_names, number_of_bones = bone_numbers, stringsAsFactors = FALSE)
head(bones_info)


#8
category_bones_summary <- aggregate(number_of_bones ~ category, data = bones_info, sum)
max_category <- category_bones_summary[which.max(category_bones_summary$number_of_bones), ]
print(max_category$category) 
category_frequency <- table(bones_info$category)
print(category_frequency)
barplot(category_bones_summary$number_of_bones, names.arg = category_bones_summary$category, main = "Number of Bones by Category", xlab = "Category", ylab = "Number of Bones", col = "lavender")


#9.
legs_data <- subset(bones_info, category == "Legs")
long_bones_legs <- subset(legs_data, nchar(name_of_bone) > 5)
print(long_bones_legs$name_of_bone)


#10
bones_starting_M <- subset(bones_info, grepl("^M", name_of_bone))
bones_starting_M$name_of_bone <- gsub("a", "A", bones_starting_M$name_of_bone)
print(bones_starting_M$name_of_bone)


#11
bones_ending_with_e <- subset(bones_info, grepl("e$", name_of_bone))
bones_ending_with_e$name_of_bone <- tolower(bones_ending_with_e$name_of_bone)
print(bones_ending_with_e$name_of_bone)


#12
bones_with_two_o <- subset(bones_info, grepl("o.*o", name_of_bone))
print(bones_with_two_o$name_of_bone)