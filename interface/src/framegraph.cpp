
#include "framegraph.hpp"

namespace internal {
    string read_string_from_sexpr(const SExpr& expr) {
        return sexpr_map<string>( [](const std::string& str)
                                    { return str; }
                                , [](shared_ptr<SList>)
                                    { throw "Expected string, got SList in SExpr"; 
                                      return "";
                                    }
                                , expr
                                );
    }

    shared_ptr<SList> read_slist_from_sexpr(const SExpr& expr) {
        return sexpr_map<shared_ptr<SList>>
            ( [](const std::string&)
                { throw "Expected SList, got string in SExpr";
                  return shared_ptr<SList>(nullptr);
                }
            , [](shared_ptr<SList> lst)
                { return lst; }
            , expr
            );
    }

    void parse_a_meaning(shared_ptr<SList> lst, temp_frames_t& temp_frames) {
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
                temp_frames[frame_var] = make_pair(name, vector<string>());
            } else {
                temp_frames[frame_var].first = name;
            }
        } else if(meaning_ident == "fe") {
            if(it == temp_frames.end()) {
                temp_frames[frame_var] = make_pair("", vector<string>(1,name));
            } else {
                temp_frames[frame_var].second.push_back(name);
            }
        } else {
            throw "Expected meaning qualifier to be either frame or fe";
        }
    }
}

