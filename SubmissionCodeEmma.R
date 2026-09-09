#Week 2 deliverable Code that reads one/run event and implements a f function that calculates and returm the momentum of each bacteria/molecules/particles
# provide directory of input file --> make sure the file is in the same directory as where your code runs
#calculate the momentum of every bacteria
#File used: "output-Set0.txt"

# HOW TO RUN THIS SCRIPT:
#   1. Place this file (SubmissionCodeEmma.R) and the data file
#      (output-Set0.txt) in the SAME folder and same name
#   2. Open a terminal and navigate to that folder:
#         cd path/to/your/folder
#   3. Run the script with:
#         Rscript SubmissionCodeEmma.R
#
#In VS Code: open the folder, open the terminal (Ctrl+`)
# and run: Rscript SubmissionCode.R


#Function: calculate momentum magnitude
# Takes the three momentum components px, py, pz of a particle and returns the total momentum magnitude using:
# p = sqrt(px^2 + py^2 + pz^2)

calculate_momentum <- function(px, py, pz) {
  p <- sqrt(px^2 + py^2 + pz^2)
  return(p)
}

# Read the input file
# The data file must be in the same folder as where this script is run
# Using a relative path so it works on any machine.

filename <- "output-Set0.txt"

# Check that the file exists before trying to open it
if (!file.exists(filename)) {
  stop(paste("ERROR: File not found:", filename,
        "\nMake sure output-Set0.txt is in the same folder as this script."))
}

lines <- readLines(filename)

#Parse event header
# First line format: event_id  n_particles
# Example: "3 15" means event 3 with 15 particles

header      <- as.numeric(strsplit(trimws(lines[1]), "\\s+")[[1]])
event_id    <- header[1]
n_particles <- header[2]

cat("Event ID           :", event_id, "\n")
cat("Number of particles:", n_particles, "\n\n")

#Analayze particle data
# Each of the following lines has four values: px  py  pz  code
# px, py, pz = momentum components
# code = particle type identifier

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

#Calculate the momentum for each particle
# Calls our function once per particle using R's vectorisation

momentum <- calculate_momentum(px, py, pz)

#Print results

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