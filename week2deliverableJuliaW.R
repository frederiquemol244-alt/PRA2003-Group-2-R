
# PRA2003 - Week 2 Deliverable
# Reads one event and calculates momentum of each particle

# HOW TO RUN THIS SCRIPT:
#   1. Place this file (week2deliverableJuliaW.R) and the data file
#      (output-Set0.txt) in the SAME folder.
#   2. Open a terminal and navigate to that folder:
#         cd path/to/your/folder
#   3. Run the script with:
#         Rscript week2deliverableJuliaW.R
#
#   In VS Code: open the folder, open the terminal (Ctrl+`)
#   and run: Rscript week2deliverableJuliaW.R

# Function: calculate momentum magnitude
# Extra if statement included to ensure that the sum of squares is not negative before taking the square root.
# Takes the three momentum components px, py, pz of a particle
# and returns the total momentum magnitude using:
#   p = sqrt(px^2 + py^2 + pz^2)

calculate_momentum <- function(px, py, pz) {
 sum_of_squares <- px^2 + py^2 + pz^2
  if (any(sum_of_squares < 0)) {
    stop("ERROR: Cannot take square root of a negative number.")
  }
  p <- sqrt(sum_of_squares)
  return(p)
}

# Read the input file
# The data file must be in the same folder as this script.
# Using a relative path so it works on any machine.

filename <- "output-Set0.txt"

# Check that the file exists before trying to open it
if (!file.exists(filename)) {
  stop(paste("ERROR: File not found:", filename,
             "\nMake sure output-Set0.txt is in the same folder as this script."))
}

lines <- readLines(filename)

# Parse event header
# First line format: event_id  n_particles
# Example: "1 29" means event 1 with 29 particles

header      <- as.numeric(strsplit(trimws(lines[1]), "\\s+")[[1]])
event_id    <- header[1]
n_particles <- header[2]

cat("Event ID           :", event_id, "\n")
cat("Number of particles:", n_particles, "\n\n")

# Parse particle data 
# Each of the following lines has four values: px  py  pz  code
# px, py, pz = momentum components
# code        = particle type identifier

px   <- numeric(n_particles)
py   <- numeric(n_particles)
pz   <- numeric(n_particles)
code <- numeric(n_particles)

for (i in 1:n_particles) {
  values  <- as.numeric(strsplit(trimws(lines[i + 1]), "\\s+")[[1]])
  px[i]   <- values[1]
  py[i]   <- values[2]
  pz[i]   <- values[3]
  code[i] <- values[4]
}

# Calculate momentum for each particle 
# Calls our function once per particle using R's vectorisation

momentum <- calculate_momentum(px, py, pz)

# Print results 

cat(sprintf("%-10s %-12s %-12s %-12s %-10s %-14s\n",
            "Particle", "px", "py", "pz", "Code", "Momentum |p|"))
cat(strrep("-", 72), "\n")

for (i in 1:n_particles) {
  cat(sprintf("%-10d %-12.6f %-12.6f %-12.6f %-10d %-14.6f\n",
              i, px[i], py[i], pz[i], code[i], momentum[i]))
}

cat("\n")
cat("Done. Momentum calculated for", n_particles,
    "particles in event", event_id, "\n")