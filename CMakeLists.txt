
cmake_minimum_required(VERSION 3.20.0)
project(qgimgdump VERSION 1.0.0 LANGUAGES CXX C)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH "C:/Qt/6.9.1/msvc2022_64")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_BUILD_TYPE Release)
find_package(Qt6 REQUIRED COMPONENTS Core Widgets Core5Compat)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
add_executable(qgimgdump 
    src/app/global.h
    src/app/config.h
    src/app/context.h
    src/app/subfile.h
    src/app/submap.h
    src/app/structs/base_header.h
    src/app/structs/fat_header.h
    src/app/structs/img_headers.cpp
    src/app/structs/img_headers.h
    src/app/structs/subdiv.h
    src/app/structs/subfile_sections.h
    src/main.cpp 
    src/stages/argparser.cpp
    src/stages/argparser.h
    src/stages/dataparser.cpp
    src/stages/dataparser.h
    src/stages/fileloader.cpp
    src/stages/fileloader.h
    src/stages/filewriter.cpp
    src/stages/filewriter.h
    src/subFiles/lbl.cpp
    src/subFiles/lbl.h 
    src/subFiles/rgnline.cpp
    src/subFiles/rgnline.h 
    src/subFiles/rgnpoint.cpp
    src/subFiles/rgnpoint.h 
    src/utils/bitstrreader.cpp 
    src/utils/bitstrreader.h 
    src/utils/exception.h
    src/utils/inline.h
    src/utils/misc.h 
    src/utils/print.h
    src/utils/task.h
)
target_include_directories(qgimgdump PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/app
    ${CMAKE_CURRENT_SOURCE_DIR}/src/stages
    ${CMAKE_CURRENT_SOURCE_DIR}/src/subFiles
    ${CMAKE_CURRENT_SOURCE_DIR}/src/utils
)
target_link_libraries(qgimgdump Qt6::Core Qt6::Widgets Qt6::Core5Compat)
