source("permutation.testing.R")
require(doParallel)
require(gtools)
require(progress)

args <- commandArgs(trailingOnly = TRUE)
if("dryrun" %in% args) {
    dryrun <- TRUE
    print("dryrun")
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

print_header("run timings")

compute_cor <- function(data) {
  ref_row <- data[1,]
  results <- foreach(i = 1:nrow(data), .combine = 'c') %dopar% {
    permutation.test(ref_row, data[i,], FUN = cor, n = 1000, return.samples = FALSE)
  }
}

if(dryrun) {
    benchmark <- function(data, cores, n_perm) {
        return(list("time_user" = 0,
                    "time_system" = 0,
                    "time_total" = 0,
                    "n_cores" = n_cores)) }
    } else {
        benchmark <- function(data, cores, n_perm) {
          ref_row <- data[1,]
          if(cores == 1) {
                t <- system.time(
                  results <- foreach(i = 1:nrow(data), .combine = 'c') %do% {
                    permutation.test(ref_row, data[i,], FUN = cor, n = n_perm, return.samples = FALSE)
                  }
                )
                n_cores <- 1
            } else {
                registerDoParallel(cores = cores)
                t <- system.time(
                  results <- foreach(i = 1:nrow(data), .combine = 'c') %dopar% {
                    permutation.test(ref_row, data[i,], FUN = cor, n = n_perm, return.samples = FALSE)
                  }
                  )
                n_cores <- getDoParWorkers()
            }
            return(list("time_user" = t[[1]],
                        "time_system" = t[[2]],
                        "time_total" = t[[3]],
                        "n_cores" = n_cores))
        }
    }

header("test %dopar%")
times <- expand.grid(
  cores = 4,
  tasks = c(1:6, 9),
  time_serial = NA,
  time_parallel = NA
)
for(r in 1:nrow(times)) {
  registerDoParallel(cores = times[r,"cores"])
  t_serial <- system.time(foreach(i=1:times[r,"tasks"]) %do% Sys.sleep(1))
  t_parallel <- system.time(foreach(i=1:times[r,"tasks"]) %dopar% Sys.sleep(1))
  times[r,"time_serial"] <- t_serial[3]
  times[r,"time_parallel"] <- t_parallel[3]
}
print(times)

print_header("test permutation.test")
m <- random_matrix(10,10)
ref_row <- m[1,]
correllations <- foreach(r = 1:nrow(m), .combine = 'c') %do%
  permutation.test(ref_row, m[r,], FUN = cor, n = 1000, return.samples = FALSE)$obs
print(correllations)

m_size <- 100
n_cores <- 4
n_perm <- 10000
m <- random_matrix(m_size, m_size)
ref_row <- m[1,]
registerDoParallel(cores = n_cores)
time_serial <- system.time(
  correllations <- foreach(r = 1:nrow(m), .combine = 'c') %do%
    permutation.test(ref_row, m[r,], FUN = cor, n = n_perm, return.samples = FALSE)$obs
)
time_parallel <- system.time(
  correllations <- foreach(r = 1:nrow(m), .combine = 'c') %dopar%
    permutation.test(ref_row, m[r,], FUN = cor, n = n_perm, return.samples = FALSE)$obs
)
cat("cores:", n_cores, 
    "permutations:", n_perm,
    "matrix size:", m_size, "x", m_size,
    "serial (%do%):", time_serial[3], 
    "parallel (%dopar%):", time_parallel[3], 
    "\n")

print_header("run benchmarks")

# pick numbers from 1 through 1 less than the number of cores
available_cores <- detectCores()
n_cores <- c(2^(0:floor(log2(available_cores))),
             available_cores-1)
cat("n_cores:", n_cores, "\n")

timings <- expand.grid(
  n_rows = c(25,50,75,100,200),
  n_cols = c(25,50,75,100,200),
  n_cores = n_cores,
  drop_fraction = c(0),
  n_perm = 10000,
  NA_count = NA,
  time_user = NA,
  time_system = NA,
  time_total = NA,
  replicate = 1:3,
  arch = R.version["arch"],
  os = R.version["os"],
  host = Sys.info()["nodename"]
)
timings$elements <- timings$n_rows * timings$n_cols
# don't use more cores than there are rows in the table:
timings <- timings[timings$n_cores < timings$n_rows,]
# skip the really slow trials:
# timings <- timings[!((timings$n_cores < 4) & (timings$elements > 10000)),]

# print(timings)

m <- random_matrix(1000,1000)
total_rows <- nrow(timings)
pb <- progress_bar$new(total = total_rows)
for(i in 1:total_rows) {
  r <- timings[i, "n_rows"]
  c <- timings[i, "n_cols"]
  data <- make_missing(m[1:r,1:c], timings[i,"drop_fraction"])
  timings[i, "NA_count"] <- sum(is.na(data))
  t <- benchmark(data, 
                 timings[i, "n_cores"],
                 timings[i, "n_perm"])
  timings[i, "time_system"] <- t$time_system
  timings[i, "time_user"] <- t$time_user
  timings[i, "time_total"] <- t$time_total
  # print(timings[i,], width = 150)
  pb$tick()
}

print_header("write timings")
saveRDS(timings, "timings.rds")

print_header("finish")
