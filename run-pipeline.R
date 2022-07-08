# Visualise
tar_visnetwork()

# Run/reproduce full analysis
#tar_make()
tar_make_future(workers = availableCores() - 1) # use min(1, ...) formulation

#tar_meta(fields = warnings)
