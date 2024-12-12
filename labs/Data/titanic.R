#Loading dataset from internet
df_titanic <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")

names <- c()  # Initialize an empty vector

for (name in colnames(df_titanic)) {
  if (is.numeric(df_titanic[[name]])) {  # Use df_titanic[[name]] to check column type
    names <- append(x = names, values = name)  # Assign the result back to names
  }
}

cor(df_titanic[names])

## More efficient way without looping
# Select only numeric columns
sapply(df_titanic, is.numeric)
numeric_columns <- df_titanic[, sapply(df_titanic, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")  # 'complete.obs' ignores missing values
print(cor_matrix)