library(tidyverse)
start_time <- Sys.time()
mean(bd$CCI1, ra.rm = TRUE)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
bd |> summarise(média = mean(CCI1, ra.rm = TRUE))
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
bd  %>%  summarise(média = mean(CCI1, ra.rm = TRUE))
end_time <- Sys.time()
end_time - start_time

