
#include "framedico.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>

#include <QtCore/QFile>
#include <QtCore/QJsonDocument>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonValue>
#include <QtCore/QJsonArray>

FrameDico::FrameDico(const std::string& path) {
    QFile file(QString::fromStdString(path));
    assert(file.open(QIODevice::ReadOnly));
    QByteArray data = file.readAll();
    QJsonDocument json(QJsonDocument::fromJson(data));

    QJsonArray root = json.array();
    for(auto it = root.begin(); it != root.end(); ++it) {
        assert(it->isObject());
        QJsonObject object = it->toObject();
        std::string name = object["name"].toString().toUtf8().constData();
        for(auto fe : object["fes"].toArray()) {
            m_frames[name].push_back(fe.toString().toUtf8().constData());
        }
    }
}

FrameDico::~FrameDico() {
    /* Nothing to do */
}

bool FrameDico::has_frame(const std::string& name) {
    return m_frames.find(name) != m_frames.end();
}

bool FrameDico::frame_has_fe(const std::string& frame, const std::string& fe) {
    return std::find(m_frames[frame].begin(), m_frames[frame].end(), fe)
        != m_frames[frame].end();
}

std::vector<std::string>& FrameDico::fes(const std::string& frame) {
    return m_frames[frame];
}


