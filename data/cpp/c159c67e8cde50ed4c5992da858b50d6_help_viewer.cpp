//
// $Id: help_viewer.cpp 67498 2012-01-28 04:14:52Z unknown $
//
// -------------------------------------------------------------------------
// This file is part of ZeroBugs, Copyright (c) 2010 Cristian L. Vlasceanu
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
// -------------------------------------------------------------------------
//
#include "config.h"
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#ifdef HAVE_SYS_TIME_H
 #include <sys/time.h>
#endif
#include <fstream>
#include <iostream>
#include <set>
#include <boost/format.hpp>
#include <boost/tokenizer.hpp>
#include "gtkmm/box.h"
#include "gtkmm/button.h"
#include "gtkmm/buttonbox.h"
#include "gtkmm/clist.h"
#include "gtkmm/connect.h"
#include "gtkmm/entry.h"
#include "gtkmm/flags.h"
#include "gtkmm/frame.h"
#include "gtkmm/menu.h"
#include "gtkmm/menuitem.h"
#include "gtkmm/notebook.h"
#include "gtkmm/paned.h"
#include "gtkmm/resize.h"
#include "gtkmm/scrolledwindow.h"
#include "dharma/dynamic_lib.h"
#include "dharma/process_name.h"
#include "dharma/system_error.h"
#include "generic/auto_file.h"
#include "generic/temporary.h"
#include "case_insensitive.h"
#include "help_document.h"
#include "help_viewer.h"
#include "html_view.h"
#include "main_window.h"
#include "slot_macros.h"
#include "text_entry.h"

using namespace std;
using namespace boost;
using namespace Gtk;
using namespace Gtk::Notebook_Helpers;
using namespace SigC;


static void
read_topics(const string& fname, TopicList& list, CTree::RowList);

static string full_help_path(const string& filename);

/// memorize visited links
static set<string> visited;

static bool on_is_visited(const char* anchor)
{
    bool result = (visited.find(anchor) != visited.end());

    return result;
}


/// A topic node; may have subtopics; if it does not have subtopics,
/// then the associated filename is set to be the a tree's node user
/// data, so that when the node is selected, the associated file is
/// shown in the HMTL view
class ZDK_LOCAL Topic
{
public:
    Topic(const string& name, const string& file, CTree::RowList);

    const string& name() const { return name_; }

    const string& file() const { return filename_; }

    const TopicList& subtopics() const { return subtopics_; }

    const string& content() const;

    vector<const Topic*> search(const string&) const;

private:
    string          name_;
    string          filename_;
    TopicList       subtopics_;
    mutable string  content_;
};


Topic::Topic(const string& name, const string& file, CTree::RowList rows)
    : name_(name), filename_(file)
{
    vector<string> node;
    node.push_back(name);
    rows.push_back(CTree::Element(node));

    if (filename_.rfind(".hlp\0") != string::npos)
    {
        read_topics(filename_, subtopics_, rows.back().subtree());
    }
    else
    {
        rows.back().set_data(this);
    }
}


const string& Topic::content() const
{
    if (content_.empty())
    {
        content_ = read_help_document(filename_.c_str());
    }
    return content_;
}


/// Simple search: if the topic has subtopics, delegate
/// the search to the children and include all the matching
/// ones in the result; otherwise, search own content
vector<const Topic*> Topic::search(const string& keyword) const
{
    vector<const Topic*> results;

    if (subtopics_.empty())
    {
        CaseInsensitiveString& c = (CaseInsensitiveString&)content();
        CaseInsensitiveString& k = (CaseInsensitiveString&)keyword;

        if (c.find(k) != string::npos)
        {
            results.push_back(this);
        }
    }
    else
    {
        TopicList::const_iterator i = subtopics_.begin();
        for (; i != subtopics_.end(); ++i)
        {
            vector<const Topic*> tmp = (*i)->search(keyword);
            results.insert(results.end(), tmp.begin(), tmp.end());
        }
    }
    return results;
}


HelpViewer::HelpViewer(Properties* prop)
    : DialogBox(btn_close, "Help")
    , prop_(prop)
    , nbook_(0)
    , topicsTree_(0)
    , htmlView_(0)
    , back_(0)
    , next_(0)
    , current_(0) // current topic
{
    // todo: read window size from properties
    Gtk_set_size(this, 720, 580);
    get_button_box()->set_layout(Gtk_FLAG(BUTTONBOX_END));

    back_ = manage(new Button("< _Back"));
    back_->set_flags(Gtk_FLAG(CAN_DEFAULT));
    get_button_box()->add(*back_);
    add_button_accelerator(*back_);
    back_->set_sensitive(false);
    Gtk_CONNECT_0(back_, clicked, this, &HelpViewer::navigate_back);

    next_ = manage(new Button("_Next >"));
    next_->set_flags(Gtk_FLAG(CAN_DEFAULT));
    get_button_box()->add(*next_);
    add_button_accelerator(*next_);
    next_->set_sensitive(false);
    Gtk_CONNECT_0(next_, clicked, this, &HelpViewer::navigate_next);

    Paned* paned = manage(new HPaned);
    get_vbox()->add(*paned);

    // left pane: a notebook (tab control)a
    nbook_ = manage(new Notebook_Adapt);
    Notebook* notebook = nbook_;
    paned->add1(*notebook);

    construct_contents_tab(*notebook);
    construct_search_tab(*notebook);
    construct_index_tab(*notebook);

    // right pane: HTML view
    Gtk::Frame* frame = manage(new Gtk::Frame);
    paned->add2(*frame);
    frame->set_shadow_type(Gtk_FLAG(SHADOW_IN));

    htmlView_ = manage(new HTMLView);
    htmlView_->set_name("HelpView");
    frame->add(*htmlView_);

    Gtk_CONNECT_0(htmlView_, link, this, &HelpViewer::on_link);
    Gtk_CONNECT(htmlView_, is_visited, Gtk_PTR_FUN(on_is_visited));

    paned->show_all();

    Gtk_set_resizable(this, true);

    Gtk_CONNECT_0(topicsTree_, tree_select_row,
        this, &HelpViewer::on_select_topic);

    // load and parse the topics.hlp file
    load();

    // select the first topic
    Gtk::CTree::RowList rows = topicsTree_->rows();
    if (!rows.empty())
    {
        Gtk::get_row(rows, *rows.begin()).select();
    }
}


HelpViewer::~HelpViewer()
{
}


void HelpViewer::construct_contents_tab(Notebook& notebook)
{
    Gtk::Frame* frame = manage(new Gtk::Frame);
    notebook.pages().push_back(TabElem(*frame, "Contents"));

    frame->set_border_width(3);

    static const char* column_heads[] = { "Topics", 0 };
    topicsTree_ = manage(new CTree(column_heads));
    ScrolledWindow* sw = manage(new ScrolledWindow);
    Gtk_add_with_viewport(sw, *topicsTree_);

    frame->add(*sw);
    Gtk_set_size(topicsTree_, 220, -1);
}


void HelpViewer::construct_search_tab(Notebook& notebook)
{
    Gtk::Frame* frame = manage(new Gtk::Frame);
    notebook.pages().push_back(TabElem(*frame, "   Keyword Search"));

    VBox* box = manage(new VBox);
    frame->add(*box);
    frame->set_border_width(3);

    box->set_border_width(10);

    search_ = manage(new TextEntry(prop_, "help_search"));
    box->pack_start(*search_, false, false);
    box->set_spacing(10);

    Gtk_CONNECT_0(search_, activate, this, &HelpViewer::on_search);

    static const char* column_heads[] = { "Results", 0 };

    searchList_ = manage(new CList(column_heads));
    box->pack_start(*searchList_);

    Gtk_CONNECT_0(searchList_, select_row,
        this, &HelpViewer::on_select_search_result);
}


void HelpViewer::construct_index_tab(Notebook& notebook)
{
}


void HelpViewer::load()
{
    read_topics("topics.hlp", topics_, topicsTree_->rows());
}


void HelpViewer::on_select_topic(Gtk::CTree::Row row, int)
{
    if (const Topic* topic = (const Topic*)row.get_data())
    {
        navigate(*topic);
    }
}


void
HelpViewer::on_select_search_result(Gtk::RowHandle nrow, int, GdkEvent*)
{
    assert(searchList_);
    assert(htmlView_);

    if (const Topic* topic = (Topic*)searchList_->row(nrow).get_data())
    {
        static const bool highlite = true;
        navigate(*topic, highlite);
    }
}


static void highlite_keyword(string& document, const string& keyword)
{
    const string beginTag = "<font color=\"purple\" size=\"+1\"><b>";
    const string endTag = "</b></font>";

    for (size_t pos = 0; ;)
    {
        // hack
        CaseInsensitiveString& d = (CaseInsensitiveString&)document;
        CaseInsensitiveString& k = (CaseInsensitiveString&)keyword;
        //size_t n = document.find(keyword, pos);
        size_t n = d.find(k, pos);

        if (n == string::npos) break;

        // is anchor?
        size_t i = document.rfind("<a ", n);
        if (i != string::npos)
        {
            size_t j = document.find("</a>", i);
            if (j != string::npos && j > n)
            {
                pos = n + keyword.size();
                continue;
            }
        }
        else
        {
            // is heading?
            i = document.rfind("<h1", n);
            if (i != string::npos)
            {
                size_t j = document.find("</h1>", i);
                if (j != string::npos && j > n)
                {
                    pos = n + keyword.size();
                    continue;
                }
            }
        }
        document.insert(n + keyword.size(), endTag);
        document.insert(n, beginTag);

        pos = n + keyword.size() + beginTag.size() + endTag.size();
    }
}


void HelpViewer::navigate(const Topic& topic, bool highlite)
{
    assert(htmlView_);

    // save the current topic so that we can go Back to it
    if (current_ && (backPages_.empty() || backPages_.back() != current_))
    {
        backPages_.push_back(current_);
        back_->set_sensitive(true);
    }
    current_ = &topic;

    visited.insert(topic.file());

    if (!highlite)
    {
        htmlView_->source(topic.content());
    }
    else
    {
        string keyword = search_->get_text(false);

        if (keyword.empty())
        {
            htmlView_->source(topic.content());
        }
        else
        {
            string document = topic.content();

            highlite_keyword(document, keyword);
            htmlView_->source(document);
        }
    }
}


BEGIN_SLOT(HelpViewer::navigate_back,())
{
    assert(!backPages_.empty());// otherwise the button is not sensitive

    // save the current topic so that we can see it Next
    if (current_ && (nextPages_.empty() || nextPages_.back() != current_))
    {
        nextPages_.push_back(current_);
        next_->set_sensitive(true);
    }

    current_ = backPages_.back();
    backPages_.pop_back();

    back_->set_sensitive(!backPages_.empty());

    if (current_)
    {
        htmlView_->source(current_->content());
    }
}
END_SLOT()


BEGIN_SLOT(HelpViewer::navigate_next,())
{
    assert(!nextPages_.empty());// otherwise the btn is not sensitive

    // save the current topic so that we can go Back to it
    if (current_ && (backPages_.empty() || backPages_.back() != current_))
    {
        backPages_.push_back(current_);
        back_->set_sensitive(true);
    }
    current_ = nextPages_.back();
    nextPages_.pop_back();

    next_->set_sensitive(!nextPages_.empty());

    if (current_)
    {
        htmlView_->source(current_->content());
    }
}
END_SLOT()


/// When the return key is pressed, search for the keyword
/// in all topics and populate the list with the matching topics
BEGIN_SLOT(HelpViewer::on_search,())
{
    assert(search_);

    string keyword = search_->get_text();
    search(keyword);
}
END_SLOT()


BEGIN_SLOT(HelpViewer::search,(const string& keyword))
{
    nbook_->set_current_page(1);
    searchList_->clear();

    TopicList::const_iterator i = topics_.begin();
    for (; i != topics_.end(); ++i)
    {
        vector<const Topic*> results = (*i)->search(keyword);

        vector<const Topic*>::const_iterator j = results.begin();
        for (; j != results.end(); ++j)
        {
            vector<string> row;
            row.push_back((*j)->name());

            searchList_->rows().push_back(row);
            searchList_->rows().back().set_data((void*)*j);
        }
    }
    // select first matched topic
    Gtk::CList::RowList rows = searchList_->rows();
    if (!rows.empty())
    {
        Gtk::get_row(*searchList_, *rows.begin()).select();
    }
}
END_SLOT()


BEGIN_SLOT_(string, HelpViewer::on_link,(const char* link))
{
    assert(link);
    visited.insert(link);

    if (strchr(link, '#'))
    {
        // backPages_.push_back(current_);
        return string();
    }
    if (current_ && (backPages_.empty() || backPages_.back() != current_))
    {
        backPages_.push_back(current_);
        back_->set_sensitive(true);
    }
    current_ = 0;
    return read_help_document(link);
}
END_SLOT_("")


static void read_topics(
    const string&   filename,
    TopicList&      topics,
    CTree::RowList  subtree)
{
    if (filename.empty())
    {
        return;
    }
    static vector<string> stack;
    string fullname = full_help_path(filename);

    if (find(stack.begin(), stack.end(), fullname) != stack.end())
    {
        cerr << "*** Warning: cycle detected: " << fullname << endl;
        return;
    }
    stack.push_back(fullname);
    vector<char> buf(4096);

    ifstream topicsFile(fullname.c_str());

    while (topicsFile.getline(&buf[0], buf.size()))
    {
        typedef escaped_list_separator<char> DelimFunc;
        typedef tokenizer<DelimFunc> Tokenizer;

        string line(&buf[0]);
        Tokenizer tok(line, DelimFunc('\\', ' ', '\"'));

        vector<string> tmp(tok.begin(), tok.end());

        // lines should have two columns (the topic's name
        // and the associated file name)
        if (tmp.size() < 2)
        {
            continue;
        }
        if (tmp[0][0] == '#')
        {
            // clog << "Comment: " << &buf[0] << endl;
            // treat the line as a comment
            continue;
        }
        boost::shared_ptr<Topic> topic(
            new Topic(tmp.front(), tmp.back(), subtree));

        topics.push_back(topic);
    }

    stack.pop_back();
}


/// If the given filename is relative, prefix it with
/// the path to the help files (given by the ZERO_HELP_PATH
/// environment variable)
static string full_help_path(const string& filename)
{
    string fullpath = filename;
    if (fullpath[0] != '/')
    {
        const char* path = getenv("ZERO_HELP_PATH");
        assert(path);
        fullpath = path + ('/' + filename);
    }
    return fullpath;
}


BEGIN_SLOT_(string, read_help_document, (const char* filename))
{
    string document;
    auto_fd fd(open(full_help_path(filename).c_str(), O_RDONLY));

    if (!fd)
    {
        throw SystemError(filename);
    }
    else
    {
        vector<char> buf(1024);

        for (;;)
        {
            int rc = read(fd.get(), &buf[0], buf.size() - 1);
            if (rc == 0)
            {
                break;
            }
            if (rc < 0)
            {
                if (errno != EINTR) break;
            }
            assert((size_t)rc < buf.size());
            buf[rc] = 0;

            document += &buf[0];
        }
    }
    return document;
}
END_SLOT_("")


namespace
{
    static void format_version(VersionInfo* vinfo, string& ver)
    {
        uint32_t minor = 0, rev = 0;
        uint32_t major = vinfo->version(&minor, &rev);

        ver += vinfo->description();
        ver += (format(" version %1%.%2%.%3% ") % major % minor % rev).str();
        ver += vinfo->copyright();
        ver += "<br><br>";
    }

    /**
     * helper for MainWindow::on_menu_about
     */
    struct ZDK_LOCAL PluginVersionHelper : public EnumCallback<DebuggerPlugin*>
    {
        virtual ~PluginVersionHelper() {}

        string ver_;

        void notify(DebuggerPlugin* p)
        {
            if (VersionInfo* vinfo = interface_cast<VersionInfo*>(p))
            {
                format_version(vinfo, ver_);
            }
        }
    };
}


/**
 * @note this method is implemented in this source file
 * for no other reason but to keep full_help_path() static.
 */
BEGIN_SLOT(MainWindow::on_menu_about,())
{
    string path = full_help_path("");
    ifstream file((path + "about.html").c_str());

    string html;
    string token;
    while (file >> token)
    {
        html += ' ';

        if (strncmp(token.c_str(), "src=\"", 5) == 0 && token[5] != '/')
        {
            // make absolute path on the fly
            token = "src=\"" + path + token.substr(5);
        }
        html += token;
    }
    string info;

    // retrieve engine version info
    if (VersionInfo* verinfo = interface_cast<VersionInfo*>(&debugger()))
    {
        uint32_t minor = 0, rev = 0;
        uint32_t major = verinfo->version(&minor, &rev);

        info += verinfo->description();
        info += (format(" %1%.%2%.%3%<br>") % major % minor % rev).str();
        info += verinfo->copyright();
        info += "<br><br>";
    }
    typedef const char* CopyrightFun ();

    {/************** detect D Demangler presence **************/
        DynamicLibHandle h(dlopen("libdemangle_d.so", RTLD_NOW));
        if (void* dcopyright = dlsym(h.get(), "demangle_d_copyright"))
        {
            info += ((CopyrightFun*) dcopyright)();
            info += "<br>";
        }
    }

    PluginVersionHelper callback;
    debugger().enum_plugins(&callback);

    info += callback.ver_;

    if (Disassembler* disasm = interface_cast<Disassembler*>(&debugger()))
    {
        if (VersionInfo* vinfo = interface_cast<VersionInfo*>(disasm))
        {
            format_version(vinfo, info);
        }
    }
    size_t n = html.rfind("<br>");
    if (n == string::npos)
    {
        n = html.size();
    }
    html.insert(n, info);

    if (!html.empty())
    {
        HTMLView* htmlView = manage(new HTMLView);

        DialogBox about(DialogBox::btn_ok, "About");
        about.get_vbox()->add(*htmlView);
        Gtk_set_size(htmlView, 540, 360);

        HTMLView::make_hrefs(html);
        htmlView->source(html);
        htmlView->show_all();

        htmlView->grab_focus();

        about.set_transient_for(*this);
        about.run(this);
    }
}
END_SLOT()


// vim: tabstop=4:softtabstop=4:expandtab:shiftwidth=4
