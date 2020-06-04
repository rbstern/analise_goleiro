dir_analysis = "./code/analysis/"
dir_data = "./data/"
dir_data_raw = "./data_raw/"
dir_plot = "./plots/"
dir_proj = "amparo_2018/"

fname_analysis = function(fname)
  paste(dir_analysis, fname, sep = "")

fname_data = function(fname) 
  paste(dir_data, dir_proj, fname, sep = "")

fname_data_raw = function(fname)
  paste(dir_data_raw, dir_proj, fname, sep = "")

fname_plot = function(fname)
  paste(dir_plot, dir_proj, fname, sep = "")

get_data_raw_dir = function()
  paste(dir_data_raw, dir_proj, sep = "")
