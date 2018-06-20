
#pragma once

#include <QtWidgets/QTextEdit>

class XmlView : public QTextEdit {
    public:
        XmlView() = delete;
        XmlView(QWidget* parent = nullptr);
        virtual ~XmlView();

        void setText(const std::string& str);
};

