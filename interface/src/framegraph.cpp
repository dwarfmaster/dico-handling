
#include "framegraph.hpp"

namespace internal {

    void parse_a_meaning(shared_ptr<SList> lst, temp_frames_t& temp_frames,
            FrameDico* dico) {
        if(lst->childrens() != 4) throw "Expected meaning sexpr to be of length 4";
        std::string meaning_ident = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[0]));
        std::string frame_var = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[1]));
        std::string name = boost::algorithm::to_lower_copy(
                read_string_from_sexpr((*lst)[2]));

        auto it = temp_frames.find(frame_var);

        if(meaning_ident == "frame") {
            if(it == temp_frames.end()) {
                temp_frames[frame_var].push_back(make_pair("", vector<string>()));
                temp_frames[frame_var].push_back(make_pair(name, vector<string>()));
            } else {
                std::vector<std::string> fes(temp_frames[frame_var][0].second.size());
                auto fes_end = std::copy_if(
                    temp_frames[frame_var][0].second.begin(),
                    temp_frames[frame_var][0].second.end(),
                    fes.begin(),
                    [&name, dico] (const std::string& fe)
                    { return dico->frame_has_fe(name, fe); } );
                fes.erase(fes_end, fes.end());
                temp_frames[frame_var].push_back(make_pair(name, fes));
            }
        } else if(meaning_ident == "fe") {
            if(it == temp_frames.end()) {
                temp_frames[frame_var].push_back(make_pair("", vector<string>(1,name)));
            } else {
                for(auto& pr : temp_frames[frame_var]) {
                    if(!dico->has_frame(pr.first)
                            || dico->frame_has_fe(pr.first, name)) {
                        pr.second.push_back(name);
                    }
                }
            }
        } else {
            throw "Expected meaning qualifier to be either frame or fe";
        }
    }
}

