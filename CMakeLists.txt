
cmake_minimum_required(VERSION 3.20.0)
project(qgimgdump VERSION 1.0.0 LANGUAGES CXX C)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_PREFIX_PATH "C:/Qt/6.9.1/msvc2022_64")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_BUILD_TYPE Release)
find_package(Qt6 REQUIRED COMPONENTS Core Widgets Core5Compat)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
add_executable(qgimgdump 
    src/main.cpp 
    src/imgdump.cpp 
    src/imgdumpprint.cpp 
    src/imgdump.h 
    src/subfiles/lbl.cpp
    src/subfiles/lbl.h 
    src/subfiles/rgnline.cpp
    src/subfiles/rgnline.h 
    src/subfiles/rgnpoint.cpp
    src/subfiles/rgnpoint.h 
    src/utils/bitstrreader.cpp 
    src/utils/bitstrreader.h 
    src/utils/exception.h
    src/utils/inline.h
    src/utils/misc.h 
    src/utils/print.h
    src/utils/task.h
)
target_include_directories(qgimgdump PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src/subfiles
    ${CMAKE_CURRENT_SOURCE_DIR}/src/utils
)
target_link_libraries(qgimgdump Qt6::Core Qt6::Widgets Qt6::Core5Compat)
