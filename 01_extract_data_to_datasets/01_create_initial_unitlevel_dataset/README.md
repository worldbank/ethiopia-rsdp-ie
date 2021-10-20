# Create Unit Level Datasets

Creates datasets to extract variables into. For grid level datasets, creates a `points.Rds` and `polygons.Rds` file. For woreda level datasets, creates the same two files--but they are the same; `points.Rds` actually contains polygons. The reason for this is to help make code compatible no matter whether grid or woreda level datasets are used.
