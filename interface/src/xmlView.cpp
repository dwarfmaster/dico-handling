
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
    srcHl.setDataDir(datadir);

    std::istringstream iss(str);
    std::ostringstream html;
    srcHl.highlight(iss, html, "xml.lang"); 

    clear();
    insertHtml(QString::fromStdString(html.str()));
}

using TS = TreeSequence<QtNodes::Node*,size_t>;
using FG = FrameGraph<QtNodes::Node*,size_t>;

TS::Frame get_word_frame(const TS& tree, size_t word) {
    return tree.get_word_frames(word)[0];
}

std::pair<size_t,size_t>& fe_span(std::pair<size_t,size_t>& result,
        const TS& tree, const FG::PlaceId& pid) {
    const FG& fg(tree.graph());
    const std::set<FG::PlaceId>& cc = fg.getCCOf(pid);
    std::set<std::string> lexemes;
    std::transform(cc.begin(), cc.end(), std::inserter(lexemes, lexemes.begin()),
            [fg] (const FG::PlaceId& pid)
                 { return fg.getFrame(pid.frame).lexeme_var; });
    std::string init_lex = fg.getFrame(pid.frame).lexeme_var;
    lexemes.erase(init_lex);

    if(lexemes.empty()) {
        result = std::make_pair((size_t)-1, (size_t)-1);
        return result;
    }

    size_t word = tree.get_word_from_var(*lexemes.begin());
    std::vector<std::pair<size_t,size_t>> levels = tree.encapsulate(word);
    size_t init_word = tree.get_word_from_var(init_lex);

    for(auto pr : levels) {
        if(pr.first > init_word || pr.second < init_word) {
            result = pr;
            break;
        }
    }

    return result;
}

void XmlView::set(const TS& tree, size_t word,
        const std::vector<std::pair<size_t,size_t>>& wordBounds) {
    std::vector<TS::Frame> frames = tree.get_word_frames(word);
    if(frames.empty()) {
        setText("");
        return;
    }
    TS::Frame frame = frames[0];
    std::ostringstream oss;
    
    oss << "<layer rank=\"1\" name=\"FE\">" << std::endl;
    for(auto pr : frame.fes) {
        std::pair<size_t,size_t> wordSpan;
        fe_span(wordSpan, tree, pr.second); 
        if(wordSpan.first == (size_t)-1) continue;
        std::pair<size_t,size_t> charSpan;
        charSpan.first  = wordBounds[wordSpan.first ].first;
        charSpan.second = wordBounds[wordSpan.second].second;

        oss << "\t<label cby=\"KmG"
            << "\" feID=\""  << pr.second.fe_id
            << "\" end=\""   << charSpan.second
            << "\" start=\"" << charSpan.first
            << "\" name=\""  << pr.first
            << "\"/>"        << std::endl;
    }
    oss << "</layer>" << std::endl;

    setText(oss.str());
}

