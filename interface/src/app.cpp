
#include "app.hpp"
#include "sexpr.hpp"

#include <algorithm>
#include <iterator>
#include <memory>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

#include <nodes/NodeStyle>
#include <nodes/FlowViewStyle>
#include <nodes/ConnectionStyle>
#include <QtGui/QClipboard>


using namespace std::chrono_literals;

using QtNodes::DataModelRegistry;
using QtNodes::FlowScene;
using QtNodes::FlowView;
using QtNodes::ConnectionStyle;
using QtNodes::FlowViewStyle;
using QtNodes::NodeStyle;
static void setStyle();

App::App(int argc, char *argv[])
    : QApplication(argc, argv), m_root(new QWidget)
    , m_scene(nullptr), m_line(nullptr), m_xml(nullptr)
    , m_copy(nullptr), m_recompute(nullptr), m_valid(nullptr)
    , m_server(4444)
{
    ::setStyle();
    m_root->setWindowTitle("FCG/Framenet annotator");
    m_root->resize(800,600);
    m_root->show();
    const size_t n = 10;

    QGridLayout* layout = new QGridLayout(m_root);
    layout->setSizeConstraint(QLayout::SetNoConstraint);
    m_root->setLayout(layout);

    m_scene = new FrameScene(m_root);
    layout->addWidget(m_scene, 0, 0, n, n+1);

    m_line = new QLineEdit(m_root);
    layout->addWidget(m_line, n+3, 0, 1, n);

    m_compute = new QPushButton("Compute", m_root);
    layout->addWidget(m_compute, n+3, n, 1, 1);

    m_xml = new XmlView(m_root);
    m_xml->setEnabled(false);
    layout->addWidget(m_xml, n, 0, 3, n);

    m_copy = new QPushButton("Copy", m_root);
    m_copy->setEnabled(false);
    layout->addWidget(m_copy, n, n, 1, 1);

    m_recompute = new QPushButton("Recompute", m_root);
    m_recompute->setEnabled(false);
    layout->addWidget(m_recompute, n+1, n, 1, 1);

    m_valid = new QPushButton("OK", m_root);
    m_valid->setEnabled(false);
    layout->addWidget(m_valid, n+2, n, 1, 1);

    QObject::connect(m_compute, SIGNAL(clicked()),
            this, SLOT(compute()));
    QObject::connect(m_line, SIGNAL(cursorPositionChanged(int,int)),
            this, SLOT(lineCursor(int,int)));
    QObject::connect(m_line, SIGNAL(returnPressed()),
            this, SLOT(compute()));
    QObject::connect(m_copy, SIGNAL(clicked()),
            this, SLOT(copy()));
    QObject::connect(m_recompute, SIGNAL(clicked()),
            this, SLOT(recompute()));
    QObject::connect(m_valid, SIGNAL(clicked()),
            this, SLOT(valid()));
}

App::~App() {
    /* Nothing to do */
}

int App::exec() {
    SExpr expr;

    while(m_root->isVisible()) {
        processEvents();
        if(m_server.receive(expr, 16ms)) {
            m_scene->reset(expr);
            m_treeSeq.rebuild(m_lexemes, m_scene->graph(), expr);

            m_xml->setEnabled(true);
            m_copy->setEnabled(true);
            m_recompute->setEnabled(true);
            m_valid->setEnabled(true);
        }
    }

    return 0;
}

template <typename Predicate>
std::vector<std::pair<size_t,size_t>>& split(
        std::vector<std::pair<size_t,size_t>>& result,
        const std::string& str,
        Predicate pred) {
    std::pair<size_t,size_t> actPair;
    bool inSpace = true;

    for(size_t i = 0; i < str.size(); ++i) {
        if(inSpace) {
            if(pred(str[i])) continue;
            inSpace = false;
            actPair.first = i;
        } else {
            if(!pred(str[i])) continue;
            inSpace = true;
            actPair.second = i;
            result.push_back(actPair);
        }
    }
    if(!inSpace) {
        actPair.second = str.size();
        result.push_back(actPair);
    }
    return result;
}

std::vector<std::string>& apply_splits(
        std::vector<std::string>& result,
        const std::string& str,
        const std::vector<std::pair<size_t,size_t>>& splits) {
    result.resize(splits.size());
    std::transform(splits.begin(), splits.end(), result.begin(),
            [&str] (const std::pair<size_t,size_t>& pr)
                   { return str.substr(pr.first, pr.second - pr.first); });
    return result;
}
        
void App::compute() {
    m_compute->setEnabled(false);
    std::string data = m_line->displayText().toUtf8().constData();
    boost::algorithm::to_lower(data);

    /* Use a real lexeme splitter */
    m_lexBounds.clear();
    ::split(m_lexBounds, data, boost::is_any_of("\t "));
    apply_splits(m_lexemes, data, m_lexBounds);
    
    std::vector<SExpr> lex_exprs;
    std::transform(m_lexemes.begin(), m_lexemes.end(), std::back_inserter(lex_exprs),
            [] (const std::string& str) -> SExpr { return str; });
    SExpr lex_list = std::make_shared<SList>(lex_exprs);

    m_server.send(lex_list);
    m_line->setReadOnly(true);
}

size_t getIdFromPos(const std::vector<std::pair<size_t,size_t>>& bounds,
        size_t idx) {
    auto it = std::lower_bound(bounds.begin(), bounds.end(),
            std::make_pair(idx, idx),
            [] (const std::pair<size_t,size_t>& p1,
                const std::pair<size_t,size_t>& p2)
               { return p1.second < p2.second; });
    return it - bounds.begin();
}

void App::lineCursor(int oldPos, int newPos) {
    if(m_lexemes.size() == 0) return;
    m_xml->set(m_treeSeq, getIdFromPos(m_lexBounds, newPos), m_lexBounds);
    std::cout << "Cursor moved from " << m_lexemes[getIdFromPos(m_lexBounds, oldPos)]
        << " to " << m_lexemes[getIdFromPos(m_lexBounds, newPos)] << std::endl;
}
        
void App::copy() {
    QClipboard* clip = clipboard();
    clip->setText(m_xml->toPlainText());
}

void App::recompute() {
    m_server.send(make_sexpr("nil"));
    reset();
}

void App::valid() {
    m_server.send(make_sexpr("t"));
    reset();
    m_line->setText("");
    m_line->setReadOnly(false);
    m_compute->setEnabled(true);
}

void App::reset() {
    m_copy->setEnabled(false);
    m_recompute->setEnabled(false);
    m_valid->setEnabled(false);
    m_xml->setText("");
    m_xml->setEnabled(false);
}


static void setStyle()
{
  ConnectionStyle::setConnectionStyle(
    R"(
  {
    "ConnectionStyle": {
      "UseDataDefinedColors": true
    }
  }
  )");

  FlowViewStyle::setStyle(
   R"(
   {
     "FlowViewStyle": {
       "BackgroundColor": [255,255,240],
       "FineGridColor":   [245,245,230],
       "CoarseGridColor": [235,235,220]
     }
   }
   )");

  NodeStyle::setNodeStyle(
   R"(
   {
     "NodeStyle": {
       "NormalBoundaryColor": "darkgray",
       "SelectedBoundaryColor": "deepskyblue",
       "GradientColor0": "mintcream",
       "GradientColor1": "mintcream",
       "GradientColor2": "mintcream",
       "GradientColor3": "mintcream",
       "ShadowColor": [200, 200, 200],
       "FontColor": [10, 10, 10],
       "FontColorFaded": [100, 100, 100],
       "ConnectionPointColor": "white",
       "PenWidth": 2.0,
       "HoveredPenWidth": 2.5,
       "ConnectionPointDiameter": 10.0,
       "Opacity": 1.0
     }
   }
   )");
}



