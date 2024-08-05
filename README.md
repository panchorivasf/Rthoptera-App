# Rthoptera
A Shiny app for standard analysis of insect sounds. The app is multi-page, with separate tabs for each process in the bioacoustics pipeline. It includes the following apps:

Pre-processing:
-Import: Browse your data to import audio files as Wave objects into R. All files are automatically corrected for DC offset.
-Downsample: When appropriate, reduce the sampling rate to increase computing speed for analyses and plots. 
-High-pass filter: When needed and possible, apply a user-defined high-pass filter to eliminate low-frequency noise. 
-Trim: Use an interactive oscillogram to select and trim a Wave object. 

Analysis
-Spectral statistics: automated spectral metrics based on the mean power spectrum of a wave object. 
-Temporal statistics: automated temporal metrics of peaks (pulses), train (syllables), and echemes (groups of syllables, trills). 
-Multi-Power Spectra: Interactive color-coded oscillogram + multiple, overlayed power spectra. 
-Spectrogram: Standard spectrogram + lateral mean power spectrum.
-Multi Plot: Spectrogram + oscillogram.
-Oscillogram: Standard oscillogram + interactive oscillogram
-Multi-oscillogram: Standard multi-species oscillogram stack plot.


Currently, the app is available as a script only. Shortly, I will create an R package for those with some experience in R. A demo will be uploaded to Youtube.
