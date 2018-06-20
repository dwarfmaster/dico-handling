
#include "xmlView.hpp"
#include <srchilite/sourcehighlight.h>
#include <sstream>
#include <iostream>

using namespace srchilite;

const char* datadir = "/nix/store/6bg39v73cpxpbkpq9ibz7in4h3xzwvha-source-highlight-3.1.8/share/source-highlight";

XmlView::XmlView(QWidget* parent)
    : QTextEdit(parent) {
    setReadOnly(true);
    setAcceptRichText(true);
}

XmlView::~XmlView() {
    /* Nothing to do */
}

void XmlView::setText(const std::string& str) {
    SourceHighlight srcHl("html.outlang");
    std::cout << "Initialised" << std::endl;
    srcHl.setDataDir(datadir);
    std::cout << "Datadir setted" << std::endl;

    std::istringstream iss(str);
    std::ostringstream html;
    srcHl.highlight(iss, html, "xml.lang"); 

    clear();
    insertHtml(QString::fromStdString(html.str()));
}

