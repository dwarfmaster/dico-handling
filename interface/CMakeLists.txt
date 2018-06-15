cmake_minimum_required(VERSION 3.4)
project(frameAnnotator)

set(CMAKE_INCLUDE_CURRENT_DIR)
set(CMAKE_AUTOMOC ON)

find_package(Qt5 COMPONENTS
             Core
             Widgets
             Gui
             DBus
             OpenGL)
find_package(Boost COMPONENTS
             system)

set(CMAKE_CXX_STANDARD 14)

set(CPP_SOURCE_FILES
    src/main.cpp
    src/app.cpp
    src/frames.cpp
    src/feData.cpp
    src/sexpr.cpp
    src/server.cpp
    src/frameModel.cpp
    src/framegraph.cpp
    src/framedico.cpp
    src/frameScene.cpp)

include_directories(nodeeditor/include)
include_directories(${Boost_INCLUDE_DIRS})
add_executable(frameAnnotator ${CPP_SOURCE_FILES})
target_link_libraries(frameAnnotator
    Qt5::Core
    Qt5::Widgets
    Qt5::Gui
    Qt5::DBus
    Qt5::OpenGL
    ${Boost_LIBRARIES})

add_subdirectory(nodeeditor)
target_link_libraries(frameAnnotator nodes)

