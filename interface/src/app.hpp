
#pragma once

#include <QtWidgets/QGridLayout>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include "frameScene.hpp"
#include "server.hpp"
#include "xmlView.hpp"

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
        XmlView* m_xml;
        QPushButton *m_copy, *m_recompute, *m_valid;
        Server m_server;
};

