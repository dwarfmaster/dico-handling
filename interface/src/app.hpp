
#pragma once

#include <QtWidgets/QGridLayout>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtWidgets/QLineEdit>
#include "frameScene.hpp"
#include "server.hpp"

class App : public QApplication {
    Q_OBJECT

    public:
        App() = delete;
        App(int argc, char *argv[]);
        virtual ~App();

        int exec();

    public slots:
        void compute();

    private:
        QWidget* m_root;
        FrameScene* m_scene;
        QLineEdit* m_line;
        Server m_server;
};

