# Import .txt file as data frame

dat.dir <- "analysis/txt/"
dat.fn <- "rick_stats.txt"

l <- readLines(paste(dat.dir, dat.fn, sep=""))

# Remove " Total volumes: ", " Total sessions: ", " Total time: "
# Remove " hrs"
l <- gsub(pattern = " Total volumes: ", x = l, replacement = "")
l <- gsub(pattern = " Total sessions: ", x = l, replacement = "")
l <- gsub(pattern = " Total time: ", x = l, replacement = "")
l <- gsub(pattern = " hrs", x = l, replacement = "")
l <- gsub(pattern = ", ", x = l, replacement = ",")

# Read four lines of 
Read.pi.dat <- function(start=1, l){
  l1 <- l[start]
  l2 <- l[start+1]
  l3 <- l[start+2]
  l4 <- l[start+3]
  dat <- sprintf("%s,%s,%s,%s\n", l1, l2, l3, l4)
  dat
}

fc <- file("analysis/csv/pi.vols.sess.hrs.csv", "w")
cat("pi,institution,vols,sessions,hrs\n", file=fc)
for (i in seq(from = 1, to = length(l), by = 4)) {
  cat(Read.pi.dat(start=i, l=l), file=fc)
}
close(fc)

