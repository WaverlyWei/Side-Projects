# Routine to read in image files three data frames.
# The files will be loaded as:
# image.file1
# image.file2
# image.file3

# Import the data
image.file1 <- read.table("data/image1.txt")
image.file2 <- read.table("data/image2.txt")
image.file3 <- read.table("data/image3.txt")

# Reconfigure data frames
name.cols <- c("y.coord", "x.coord", "exp.label", 
               "ndai", "sd", "corr", "df", "cf", "bf","af", "an")
colnames(image.file1) <- name.cols
colnames(image.file2) <- name.cols
colnames(image.file3) <- name.cols

image.file1 <- data.frame(image.file1)
image.file2 <- data.frame(image.file2)
image.file3 <- data.frame(image.file3)

# Convert expert label column to factor for easier plotting
image.file1 <- image.file1 %>%
  mutate(exp.label = as.factor(exp.label))
image.file2 <- image.file2 %>%
  mutate(exp.label = as.factor(exp.label))
image.file3 <- image.file3 %>%
  mutate(exp.label = as.factor(exp.label))