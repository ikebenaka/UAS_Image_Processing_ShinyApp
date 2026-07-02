# NEFSC UAS Imagery Processing App User Guide

This guide is for a first-time user processing drone-based photogrammetry imagery and video frames for whale research. It covers work done inside the Shiny app and the manual work that still happens outside the app.

The app creates folders, metadata CSVs, selected-image folders, and measurement products. It should not modify raw imagery, raw videos, original logs, trigger files, or original external inputs.

## Core Principles

- Keep the generated flight-day folder structure intact. Past seasons depend on these folder and CSV conventions.
- Keep raw files read-only. The app may create copies and derived CSVs, but raw media and logs should not be edited in place.
- Preserve existing CSV columns. Existing `_imgdata.csv` and `_photos_measured.csv` columns are required by downstream workflows.
- Use local copied test data when experimenting. Do not test destructive workflows on original field data.
- Review app warnings before moving to the next stage. Warnings are intended to catch missing metadata, bad joins, missing altitude, bad sensor fields, duplicate files, and measurement conversion problems.

## Required Software

- R and RStudio or another way to run the Shiny app.
- Required R packages listed in `global.R`.
- ExifTool support through the bundled `exiftool.exe` and the app's EXIF packages.
- VLC for selecting and exporting video frames.
- Lightroom or equivalent external software for Astro lens distortion correction, when corrected Astro imagery or video will be used.
- Morphometrix for measuring selected still images and exported video frames.
- Excel or another CSV editor for manual `whaleinfo` and grading edits, if you choose not to edit CSVs elsewhere.

## End-to-End Workflow

1. Create or select a field-season folder.
2. Use the app to generate the flight-day folder structure.
3. Copy raw media, logs, trigger files, lidar logs, corrected files, and related data into the correct generated folders.
4. For EVO, use the time-check tab if a GPS clock image was collected.
5. Run Image Metadata Processing to generate platform `_imgdata.csv` files.
6. If videos will be used, export selected frames from VLC into the platform `video_frames` folder.
7. Run Refresh Video Frame Metadata to generate `_video_inventory.csv` and `_video_frames.csv`.
8. Fill `whaleinfo` in each relevant `_imgdata.csv` and `_video_frames.csv`.
9. Run Assign Whale IDs / EGNO so each whale observation gets its own row.
10. Fill photogrammetry grading fields in the metadata CSVs.
11. Run Create Photogrammetry File Structure to create `Photos_Measured`, EGNO folders, and measurement-ready CSVs.
12. Measure copied files in Morphometrix.
13. Save Morphometrix CSVs into the correct `Photos_Measured/EGNO` folders.
14. Run Collate Flight Day Measurements to update platform `_photos_measured.csv` files.
15. Run Combine Field Season Measurements to create field-season pixel and meter outputs.
16. Review all QA warning columns and app messages before final analysis.

## Generated Flight-Day Folder Structure

Run the Generate Flight Day File Structure tab before copying data into the flight day.

### APH-22

- `APH-22/jpg`: APH still images.
- APH GPX/KML or flight-day folders should be placed in the APH platform folder as expected by the current APH processing code.

### EVO II Pro and EVO II Dual

- `EVO II Pro/jpg` or `EVO II Dual/jpg`: still images.
- `EVO II Pro/video` or `EVO II Dual/video`: videos and existing EVO video metadata products.
- `EVO_logs`: EVO/AirData logs when available.
- `log`: LiDARBoX or altimeter logs.
- `thermal`: EVO II Dual thermal files when applicable.
- `video_frames`: created or confirmed when video is detected during metadata processing.

Expected EVO video products include:

- `*_video_metadata.csv`: video filenames, start/end timestamps, duration, platform, and sensor fields.
- `*_Video_GPS_Time.csv`: video start time and GPS time linkage fields.
- `*_video_CleanedLidar.csv`: cleaned lidar/altitude records by time.

### FreeFly Astro

- `Astro/jpg/card01`, `Astro/jpg/card02`, etc.: uncorrected SD-card stills. `card01` is created by default; add more card folders manually as needed.
- `Astro/jpg/thumbdrive`: thumbdrive stills, when used.
- `Astro/jpg_corr`: lens-corrected still images exported from Lightroom or another correction workflow.
- `Astro/video/card01`, `Astro/video/card02`, etc.: uncorrected SD-card videos. `card01` is created by default; add more card folders manually as needed.
- `Astro/video_corr/card01`, `Astro/video_corr/card02`, etc.: corrected videos. `card01` is created by default.
- `Astro/video_frames/card01`, `Astro/video_frames/card02`, etc.: VLC-exported video frame stills.
- `Astro/log`: Drone Amplified `photo_info` logs.
- `Astro/flight_logs`: `.ulg` or other flight logs, when available.

## Tab-by-Tab Instructions

### 1. Generate Flight Day File Structure

Use this first for each flight day.

1. Select the field-season folder.
2. Enter the flight date as `YYYYMMDD`.
3. Select the platforms flown that day.
4. Click Generate File System.
5. Copy the day's files into the generated folders.

This tab only creates or confirms folders. It does not move, rename, or modify raw files.

### 2. Check Time on EVO Image

Use this when an EVO image of a GPS clock or other GPS time reference was collected.

1. Upload the GPS clock image.
2. Read the EXIF timestamp shown by the app.
3. Compare that timestamp with the GPS time visible in the image.
4. Use the difference as the EVO time offset in Image Metadata Processing.

This matters because EVO camera EXIF time and GPS/log time can differ.

### 3. Image Metadata Processing

Use this after media and logs are in the platform folders.

1. Select the flight-day folder.
2. Enter permit, species, and pilot.
3. Enter EVO time offsets if needed.
4. Enter platform-specific barometric altitude offsets if needed.
5. For Astro, choose whether to process uncorrected `jpg` or corrected `jpg_corr` stills.
6. Click Process Data.
7. Review status messages and QA warnings.

This creates platform `_imgdata.csv` files. These files include filename, image number, datetime/time, flight number, platform, pilot, permit, species, whaleinfo, latitude, longitude, GPS altitude, raw laser altitude, tilt, corrected altitude, barometric altitude, focal length, image dimensions, sensor width, pixel dimension, and QA warning fields where applicable.

For Astro SD-card stills, the app can use Drone Amplified `photo_info` logs to assign altitude and position by order or timing when the camera files themselves do not contain full altitude metadata.

Corrected Astro stills currently use a placeholder missing pixel dimension until corrected calibration constants are supplied. Review warnings before using meter conversions from corrected Astro media.

### 4. Refresh Video Frame Metadata

Use this after video files are present and VLC frames have been exported.

1. Export selected video frames from VLC into the correct `video_frames` folder.
2. For EVO, frame names should encode the source video and time, for example `MAX_0047_f2.MP4_00_00_14_vlc_00001.jpg`.
3. For Astro with multiple SD cards, put frames in matching card folders such as `Astro/video_frames/card01`.
4. If the Astro Drone Amplified flight is known, use a flight subfolder such as `Astro/video_frames/card01/f2`.
5. Select the flight-day folder or platform folder in the app.
6. For Astro, choose whether video inventory should use `video` or `video_corr`.
7. Click Refresh Video Frame Metadata.
8. Review `_video_inventory.csv`, `_video_frames.csv`, and app warnings.

This tab creates or refreshes:

- `YYYYMMDD_PLATFORM_video_inventory.csv`
- `YYYYMMDD_PLATFORM_video_frames.csv`

Video-frame metadata should be generated before assigning `whaleinfo`, because the frame rows need to exist before they can be annotated and split by EGNO.

### 5. Assign Whale IDs / EGNO

Use this after `_imgdata.csv` exists and, if applicable, `_video_frames.csv` exists.

1. Open each relevant metadata CSV in Excel or another CSV editor.
2. Fill `whaleinfo` with the whale IDs/names present in each image or frame.
3. If multiple whales occur in one image/frame, separate IDs with commas or semicolons.
4. Save and close the CSVs.
5. In the app, select the flight-day folder or platform folder.
6. Click Split Whale IDs Across Metadata Files.

The app writes rows back to the separate still-image and video-frame CSVs. Each whale observation gets its own row and `EGNO` value. Backups are created before overwriting.

### 6. Create Photogrammetry File Structure

Use this after whale IDs are split and photogrammetry grading fields are filled.

1. Fill grading fields in `_imgdata.csv` and `_video_frames.csv`.
2. Use a positive `photogram_quality` value for rows that should be measured.
3. Select the flight-day folder.
4. Click Generate Photogrammetry File Structure.
5. Review the status output.

This creates:

- platform `Photos_Measured` folders
- EGNO subfolders inside `Photos_Measured`
- copied measurement-ready still images and video frames
- platform `YYYYMMDD_PLATFORM_photos_measured.csv` files

Video-frame provenance columns remain in `_video_frames.csv` and are not carried into `_photos_measured.csv`.

### 7. Collate Flight Day Measurements

Use this after images/frames have been measured in Morphometrix.

1. Save each Morphometrix CSV into the matching `Photos_Measured/EGNO` folder.
2. Keep the Morphometrix image path row or filename information intact.
3. Select the flight-day folder.
4. Click Collate Flight Day Measurements.
5. Review matched/unmatched counts and QA warnings.

This updates each platform `_photos_measured.csv` for the selected flight day. It joins Morphometrix pixel measurements back to metadata rows by filename and EGNO, preserves unmeasured rows, and reports unmatched measurement files.

The collation step warns about duplicate `FileName + EGNO`, missing altitude or sensor fields, unmatched Morphometrix files, and impossible or suspicious meter conversion results.

### 8. Combine Field Season Measurements

Use this after each flight day has been collated.

1. Select the field-season folder that contains `YYYYMMDD` flight-day folders.
2. Click Combine Field Season Measurements.
3. Review app messages.
4. Review the field-season pixel and meter CSVs.
5. Check `measurement_qa_warnings` before final analysis.

This creates field-season-level files that combine platform `_photos_measured.csv` files across flight days.

## Platform Workflows

### APH-22 Still Images

1. Generate folders with APH-22 selected.
2. Place still images in `APH-22/jpg`.
3. Place APH log/flight files in the expected APH platform location.
4. Run Image Metadata Processing.
5. Fill `whaleinfo`.
6. Run Assign Whale IDs / EGNO.
7. Fill grading fields.
8. Run Create Photogrammetry File Structure.
9. Measure in Morphometrix.
10. Run Collate Flight Day Measurements.

### EVO Still Images

1. Generate folders with EVO II Pro or EVO II Dual selected.
2. Place images in `jpg`.
3. Place AirData/EVO logs in `EVO_logs`.
4. Place altimeter logs in `log`.
5. Use Check Time on EVO Image if a time-reference image exists.
6. Run Image Metadata Processing with the correct time offset.
7. Fill `whaleinfo`, split EGNO, grade, create `Photos_Measured`, measure, and collate.

### EVO Video Frames

1. Complete EVO still-image metadata processing first.
2. Confirm videos and video metadata products are in the platform `video` folder.
3. Export frames with VLC into the platform `video_frames` folder.
4. Use frame names that encode source video, time, and VLC frame number.
5. Run Refresh Video Frame Metadata.
6. Review altitude and metadata fields.
7. Fill `whaleinfo` in `_video_frames.csv`.
8. Split EGNO, grade, create `Photos_Measured`, measure, and collate.

### Astro Still Images

1. Generate folders with FreeFly Astro selected.
2. Place SD-card stills under `Astro/jpg/card##`.
3. Place thumbdrive stills in `Astro/jpg/thumbdrive` when used.
4. Place Drone Amplified `photo_info` logs in `Astro/log`.
5. If using corrected images, apply lens correction externally and save corrected files in `Astro/jpg_corr`.
6. In Image Metadata Processing, choose `jpg` or `jpg_corr`.
7. Review altitude, position, and pixel-dimension warnings.
8. Fill `whaleinfo`, split EGNO, grade, create `Photos_Measured`, measure, and collate.

### Astro Video Frames

1. Place uncorrected videos under `Astro/video/card##`.
2. If using corrected video, save corrected videos under `Astro/video_corr/card##` and ensure corrected filenames include `_corr`.
3. Place Drone Amplified `photo_info` logs in `Astro/log`.
4. Export selected VLC frames into `Astro/video_frames/card##`.
5. If known, place frames in a flight subfolder like `Astro/video_frames/card01/f2`.
6. Run Refresh Video Frame Metadata and choose the correct Astro video source.
7. Review `_video_inventory.csv` and `_video_frames.csv`.
8. Fill `whaleinfo`, split EGNO, grade, create `Photos_Measured`, measure, and collate.

## Manual CSV Fields

The following fields may still be edited manually in Excel:

- `whaleinfo`
- `camera_focus`
- `body_straightness`
- `body_roll`
- `body_arch`
- `body_pitch`
- `body_length_measurability`
- `body_width_measurability`
- `photogram_quality`
- `photogram_comments`

Use `whaleinfo` for whale IDs/names visible in the image or frame. Use `photogram_quality` to decide which rows should be copied into `Photos_Measured`.

## Video Frame Naming

Recommended EVO frame naming:

`VIDEOFILENAME_f#_HH_MM_SS_vlc_FRAMENUMBER.jpg`

Example:

`MAX_0047_f2.MP4_00_00_14_vlc_00001.jpg`

For Astro, the app is designed to avoid requiring manual card prefixes in the filename. Use folder placement instead:

- `Astro/video_frames/card01`
- `Astro/video_frames/card02`
- `Astro/video_frames/card01/f2`

This lets the app infer card and flight context from folders.

## QA/QC Checks to Review

Review status output and warning columns for:

- missing folders or expected files
- missing EXIF timestamps
- missing log files
- missing altitude records
- timestamp mismatches
- duplicate filenames
- invalid or missing `whaleinfo`
- multiple whales in one image/frame before splitting
- inconsistent platform naming
- invalid sensor specifications
- altitude outside the valid 5 to 100 m range
- failed joins between media and logs
- outputs that already exist and will be backed up before overwrite
- missing measurement conversion inputs
- impossible body length or width values after meter conversion

## Output Files

Common outputs include:

- `YYYYMMDD_PLATFORM_imgdata.csv`: platform still-image metadata.
- `YYYYMMDD_PLATFORM_video_inventory.csv`: video inventory and video-level metadata.
- `YYYYMMDD_PLATFORM_video_frames.csv`: exported video-frame metadata.
- `YYYYMMDD_PLATFORM_photos_measured.csv`: selected/grading/measurement-ready metadata inside `Photos_Measured`.
- field-season combined pixel CSV: combined measurements in pixels.
- field-season combined meter CSV: combined measurements converted to meters where conversion inputs are available.

Backups are created before overwriting key metadata files.

## Troubleshooting

If expected rows are missing:

- Confirm the correct flight-day or platform folder was selected.
- Confirm files are in the expected platform subfolders.
- Confirm CSVs are closed in Excel before running the app.
- Confirm filenames match expected conventions.
- For Astro multi-card days, confirm frame stills are in the correct `video_frames/card##` folder.

If altitude is missing:

- Confirm log files are present.
- Confirm timestamps or frame times are within the available log range.
- For Astro, confirm Drone Amplified `photo_info` logs are in `Astro/log`.
- For EVO, confirm cleaned lidar and video metadata products exist.
- Review whether altitude values were filtered because they were zero, negative, outside 5 to 100 m, or otherwise impossible.

If corrected Astro media is used:

- Confirm corrected filenames include `_corr`.
- Confirm corrected files are in `jpg_corr` or `video_corr/card##`.
- Treat meter conversion results as incomplete until corrected pixel dimension constants are configured.

## Current Known Limitation

Corrected Astro stills and corrected Astro videos currently write missing pixel dimension values because corrected/cropped calibration constants have not yet been supplied. Do not rely on meter conversions from corrected Astro media until those constants are added and tested.
