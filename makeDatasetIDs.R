set.seed(1)

ids <- sapply(seq_len(2500), function(i) {
	paste0(sample(0:9, size = 10, replace = TRUE), collapse = "")
})
ids <- unique(ids)
write(ids, "ids_datasets.txt")