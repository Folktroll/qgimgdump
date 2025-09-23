
## 👌 Project Goals and Supported Features
- Ability to read **GARMIN maps** and use them in QGIS or other open source GIS software  
- Dump data from the **GARMIN container format (.img)** into a text-based format ready for further conversion  
- Support for **GARMIN non-NT** and **pseudo-NT** formats  

### ⁉️ Todo or what doesn't work:
- *Missing strings for with LBL in the NET subfile*

## ✖️ Out of Scope
- Decoding of **NT format**, locked/protected maps, or similar features  
- Exporting of *all* possible data fields  
- Turn-by-turn navigation information  
- Reading of **labels (LBL)** is not a current priority  
- Export to **.mp (Polish format)** is considered a temporary solution, mainly for comparing outputs, and will likely be replaced by a more lightweight JSON format in the future  


## 📚 Wiki & Knowledge

This section contains useful resources for working with Garmin IMG files, including PDFs and forum discussions.

### 📄 PDF Documents

- **[Garmin IMG Format Overview](https://sourceforge.net/projects/garmin-img/files/IMG%20File%20Format/1.0/imgformat-1.0.pdf)**  
  Introduction to IMG format, including file system structure and main subfiles (by John Mechalas).

- **[Exploring Garmin's IMG Format](https://www.pinns.co.uk/osm/docs/expl_img2015.pdf)**  
  Practical guide for decoding coordinate data and bit-level structures in Garmin IMG files (by N. Willink).

- **[Garmin IMG Subfiles Format](https://www.memotech.franken.de/FileFormats/Garmin_IMG_Subfiles_Format.pdf)**  
  Detailed description of Garmin IMG subfile structures, including TRE, RGN, LBL, NET, NOD, and DEM (by Herbert Oppmann).

### 📄 PDF Manuals
- **[cGPSmapper User Manual](https://gpstraces.net/tutos/cGPSmapper-UsrMan-v02.5.pdf)**  

### 💬 Forum Discussion

- **[GPSPower: Convert Garmin IMG pseudo-NT to non-NT](https://www.gpspower.net/creating-maps/302614-convert-garmin-img-pseudo-nt-non-nt-i-know-its-possible-but-how-solved.html)**  
  Discussion on converting Garmin IMG files from pseudo-NT to non-NT format (GMP->IMG) (thanks @testlelelala).

## License & Project Origin

This project is a fork of [garmin parser of QMapShack](https://github.com/Maproom/qmapshack/tree/dev/src/qmapshack/map/garmin), licensed under GPLv3. Earlier commits were missing license headers by mistake, but the project has always been under GPLv3.


## 🔨 Extra tools

### mp2json

Tool for normalizing and exporting **Polish Format (.mp)** files into JSON for easier comparison and diffing.

#### Features
- Robust parsing of `.mp` files (handles `[END]` markers, ignores comments)
- Deduplication of identical sections
- Skipping empty props
- Property filtering with wildcards:
  - `--drop-props` → remove matching keys
  - `--allow-props` → keep only matching keys
- Coordinate trimming for `Data*` keys (`--coord-trim N`)
- Flexible JSON output:
  - Pretty print (`--indent`)
  - Compact diff-friendly (`--compact`)
  - Line wrapping with `--maxwidth`
- Encoding choice: `utf-8` (default) or `cp1251`
- Splitting output:
  - by section type (`--split`)
  - by max file size MB (`--split-size`)
  - can combine both
