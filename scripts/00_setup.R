# Run from the repository root (open the .Rproj file).
if (!file.exists("TP_Soil_Phosphorus.Rproj")) {
  stop("Open TP_Soil_Phosphorus.Rproj (or set the working directory to the project root).")
}
for (folder in c("data/processed", "outputs/figures", "outputs/tables")) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
