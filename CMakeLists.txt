
cmake_minimum_required(VERSION 3.20.0)
project(qgimgdump VERSION 1.0.0 LANGUAGES CXX C)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_PREFIX_PATH "C:/Qt/6.9.1/msvc2022_64")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)
# set(CMAKE_BUILD_TYPE Release)
find_package(Qt6 REQUIRED COMPONENTS Core Widgets Core5Compat)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
add_executable(qgimgdump
    src/app/global.h
    src/app/config.h
    src/app/context.h
    src/app/subblock.h
    src/app/submap.h
    src/app/hdr/supp.h
    src/app/hdr/fat.h
    src/app/hdr/block.cpp
    src/app/hdr/block.h
    src/app/hdr/subdiv.h
    src/app/hdr/section.h
    src/main.cpp 
    src/stages/argparser.cpp
    src/stages/argparser.h
    src/stages/dataparser.cpp
    src/stages/dataparser.h
    src/stages/fileloader.cpp
    src/stages/fileloader.h
    src/stages/filewriter.cpp
    src/stages/filewriter.h
    src/blocks/lbl.cpp
    src/blocks/lbl.h
    src/blocks/rgnnode.cpp
    src/blocks/rgnnode.h
    src/blocks/rgnpath.cpp
    src/blocks/rgnpath.h
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
    ${CMAKE_CURRENT_SOURCE_DIR}/src/blocks
    ${CMAKE_CURRENT_SOURCE_DIR}/src/utils
)
target_link_libraries(qgimgdump Qt6::Core Qt6::Widgets Qt6::Core5Compat)
