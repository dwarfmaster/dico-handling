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
find_package(PkgConfig REQUIRED)
pkg_check_modules(SRCHL REQUIRED source-highlight)

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
    src/frameScene.cpp
    src/xmlView.cpp
    src/treeSequence.cpp)

include_directories(nodeeditor/include)
add_executable(frameAnnotator ${CPP_SOURCE_FILES})
target_include_directories(frameAnnotator PUBLIC
    ${Boost_INCLUDE_DIRS}
    ${SRCHL_INCLUDE_DIRS})
target_link_libraries(frameAnnotator
    Qt5::Core
    Qt5::Widgets
    Qt5::Gui
    Qt5::DBus
    Qt5::OpenGL
    ${Boost_LIBRARIES}
    ${SRCHL_LIBRARIES})

add_subdirectory(nodeeditor)
target_link_libraries(frameAnnotator nodes)

