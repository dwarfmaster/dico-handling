
#include "framedico.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <boost/algorithm/string.hpp>

#include <QtCore/QFile>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonValue>
#include <QtCore/QJsonArray>

using boost::algorithm::to_lower_copy;

FrameDico::FrameDico(const std::string& path) {
    QFile file(QString::fromStdString(path));
    assert(file.open(QIODevice::ReadOnly));
    QByteArray data = file.readAll();
    QJsonDocument json(QJsonDocument::fromJson(data));

    QJsonArray root = json.array();
    for(auto it = root.begin(); it != root.end(); ++it) {
        assert(it->isObject());
        QJsonObject object = it->toObject();
        std::string name(object["name"].toString().toUtf8().constData());
        name = to_lower_copy(name);
        for(auto fe : object["fes"].toArray()) {
            m_frames[name].push_back(to_lower_copy(std::string(
                            fe.toString().toUtf8().constData())));
        }
    }
}

FrameDico::~FrameDico() {
    /* Nothing to do */
}

bool FrameDico::has_frame(const std::string& name) {
    return m_frames.find(to_lower_copy(name)) != m_frames.end();
}

bool FrameDico::frame_has_fe(const std::string& frame, const std::string& fe) {
    std::string lframe = to_lower_copy(frame);
    std::string lfe    = to_lower_copy(fe);
    return std::find(m_frames[lframe].begin(), m_frames[lframe].end(), lfe)
        != m_frames[lframe].end();
}

std::vector<std::string>& FrameDico::fes(const std::string& frame) {
    return m_frames[to_lower_copy(frame)];
}


