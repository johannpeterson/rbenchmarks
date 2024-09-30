require(doParallel)
require(missForest)
require(utils)
require(gtools)

args <- commandArgs(trailingOnly = TRUE)
if("dryrun" %in% args) {
    dryrun <- TRUE
} else {
    dryrun <- FALSE
}

print_header <- function(title) cat("\n", title, "\n", date(), "\n", rep("-",40), "\n", sep='')

random_matrix <- function(rows, cols) {
  letter_string <- function(n) intToUtf8( baseOf(n,26)+utf8ToInt("a"))
  letter_string <- Vectorize(letter_string)

  m <- t(mapply(rnorm, n=cols, mean=runif(rows, -5, 5), sd=rchisq(rows, 2.5)))
  row.names(m) <- letter_string(0:(nrow(m)-1))
  return(m)
}

make_missing <- function(data, prob) {
  rows <- nrow(data)
  cols <- ncol(data)
  new_data <- data
  for(i in 1:rows) {
    new_data[i, rbinom(cols, 1, prob)==1] <- NA
  }
  return(new_data)
}

commands_list <- list(
  "Darwin" = list(
    "cpu info:" = "sysctl -a | grep machdep.cpu",
    "memory info:" = "sysctl -a | grep memsize"
  ),
  "Linux" = list(
    "cpu info:" = "lscpu",
    "memory info:" = "lsmem"
  )
)

print_header("Machine Info")
execute_os_commands <- function() {
  os <- Sys.info()["sysname"]  # Detect the operating system
  commands <- commands_list[[os]]  # Look up the commands for the detected OS

  if (is.null(commands)) {
    stop("Unsupported operating system.")
  }

  # Execute each command
  for (cmd_name in names(commands)) {
    cat(cmd_name, "\n")
    system(commands[[cmd_name]])  # You can also use system2 if needed
  }
}

cat("host name:", Sys.info()["nodename"], "\n")
cat("R.version:")
print(R.version)
cat("detectCores():", detectCores(), "\n")

execute_os_commands()

print_header("Start")

print_header("imputation")

m <- random_matrix(1000,1000)

if(dryrun) {
    benchmark_imputation <- function(data, cores) {
        return(list("time_user" = 0,
                    "time_system" = 0,
                    "time_total" = 0,
                    "n_cores" = n_cores)) }
    } else {
        benchmark_imputation <- function(data, cores) {
            if(cores == 1) {
                t <- system.time(
                    m_imputed <- missForest(data, parallelize = "no"))
                n_cores <- 1
            } else {
                registerDoParallel(cores = cores)
                t <- system.time(
                    m_imputed <- missForest(data, parallelize = "variables"))
                n_cores <- getDoParWorkers()
            }
            return(list("time_user" = t[[1]],
                        "time_system" = t[[2]],
                        "time_total" = t[[3]],
                        "n_cores" = n_cores))
        }
    }

# pick numbers from 1 through 1 less than the number of cores
available_cores <- detectCores()
n_cores <- c(2^(0:floor(log2(available_cores))),
            available_cores-1)
cat("n_cores:", n_cores, "\n")

timings_imputation <- expand.grid(
  size = c(10,50,100,200,300,400,500),
  n_cores = n_cores,
  drop_fraction = c(0.05, 0.1, 0.2),
  replicate = c(1),
  arch = R.version["arch"],
  os = R.version["os"],
  host = Sys.info()["nodename"]
)
col_names <- c("n_rows", "n_cols", "NA_count", "time_user", "time_system", "time_total", "n_cores", "os", "arch", "host")
for(c in col_names) if(! c %in% names(timings_imputation)) timings_imputation[c] <- NA

# don't use more cores than there are rows in the table:
timings_imputation <- timings_imputation[timings_imputation$n_cores < timings_imputation$size,]
# skip the really slow trials:
timings_imputation <- timings_imputation[!((timings_imputation$n_cores < 4) & (timings_imputation$size > 200)),]

print(timings_imputation)

for(i in 1:nrow(timings_imputation)) {
  s <- timings_imputation[i, "size"]
  timings_imputation[i, "n_rows"] <- s
  timings_imputation[i, "n_cols"] <- s
  data <- make_missing(m[1:s,1:s], timings_imputation[i,"drop_fraction"])
  timings_imputation[i, "NA_count"] <- sum(is.na(data))
  t <- benchmark_imputation(data, timings_imputation[i, "n_cores"])
  timings_imputation[i, "time_system"] <- t$time_system
  timings_imputation[i, "time_user"] <- t$time_user
  timings_imputation[i, "time_total"] <- t$time_total
  print(timings_imputation[i,], width = 150)
}

print_header("write imputation timings")
saveRDS(timings_imputation, "timings_imputation.rds")

# print_header("distances & permutation tests")
#
# benchmark_distances <- function(data, n_cores) {
#
# }
#
# timings_distances <- expand.grid(
#   size = c(10,50,100,200),
#   n_cores = n_cores,
#   drop_fraction = c(0.05, 0.1, 0.2),
#   replicate = c(1)
# )
# col_names <- c("n_rows", "n_cols", "elements",
#                "NA_count", "time_user", "time_system",
#                "time_total", "n_cores")
# for(c in col_names) if(! c %in% names(timings_imputation)) timings_imputation[c] <- NA
# timings_distances$n_cols <- timings_distances$size
# timings_distances$n_rows <- timings_distances$size
# timings_distances$elements <- timings_distances$n_cols * timings_distances$n_rows
#
# for(i in 1:nrow(timings_distances)) {
#   s <- timings_distances[i, "size"]
#   data <- make_missing(m[1:s,1:s], timings_distances[i,"drop_fraction"])
#   timings_distances[i, "NA_count"] <- sum(is.na(data))
#   t <- benchmark_distances(data, timings_distances[i, "n_cores"])
#   timings_distances[i, "time_system"] <- t$time_system
#   timings_distances[i, "time_user"] <- t$time_user
#   timings_distances[i, "time_total"] <- t$time_total
#   print(timings_distances[i,], width = 150)
# }

print_header("finish")
