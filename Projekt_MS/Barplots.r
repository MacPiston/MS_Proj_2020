Autko <- read.table("./data/auto-mpg.data", quote="\"", comment.char="")

barplot(table(Autko$cylinders), main = "Cylindry", xlab = "Iloœæ cylindrów", col = "sky blue")
barplot(table(Autko$model_year), main = "Rok produkcji", xlab = "Rok", col = "magenta")
barplot(table(Autko$origin), main = "Origin", xlab = "numer", col = "red")
