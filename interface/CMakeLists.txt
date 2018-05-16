cmake_minimum_required(VERSION 3.4)
project(frameAnnotator)

set(CMAKE_INCLUDE_CURRENT_DIR)
set(CMAKE_AUTOMOC ON)

find_package(Qt5 COMPONENTS
             Core
             Widgets
             Gui
             OpenGL)

set(CMAKE_CXX_STANDARD 14)

set(CPP_SOURCE_FILES
    src/main.cpp
    src/models.cpp)

include_directories(nodeeditor/include)
add_executable(frameAnnotator ${CPP_SOURCE_FILES})
target_link_libraries(frameAnnotator
    Qt5::Core
    Qt5::Widgets
    Qt5::Gui
    Qt5::OpenGL)

add_subdirectory(nodeeditor)
target_link_libraries(frameAnnotator nodes)

