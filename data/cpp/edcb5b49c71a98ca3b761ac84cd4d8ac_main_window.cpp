//
// $Id: main_window.cpp 68017 2012-02-24 09:26:09Z unknown $
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

#if (__GNUC__ < 3) && !defined(_GNU_SOURCE)
 #define _GNU_SOURCE // for strsignal
#endif

#include "zdk/stdexcept.h"
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <iomanip>
#include <fstream>
#include <iostream>
#include <boost/tokenizer.hpp>
#include "gtkmm/accelkey.h"
#include "gtkmm/accelmap.h"
#include "gtkmm/box.h"
#include "gtkmm/button.h"
#include "gtkmm/connect.h"
#include "gtkmm/flags.h"
#include "gtkmm/fontselection.h"
#include "gtkmm/frame.h"
#include "gtkmm/handlebox.h"
#include "gtkmm/label.h"
#include "gtkmm/menu.h"
#include "gtkmm/menubar.h"
#include "gtkmm/menuitem.h"
#include "gtkmm/pixmap.h"
#include "gtkmm/resize.h"
#include "gtkmm/scrolledwindow.h"
#include "gtkmm/settings.h"
#include "gtkmm/stock.h"
#include "zdk/breakpoint_util.h"
#include "zdk/buffer_impl.h"
#include "zdk/check_ptr.h"
#include "zdk/data_type.h"
#include "zdk/heap.h"
#include "zdk/history.h"
#include "zdk/interp.h"
#include "zdk/shared_string_impl.h"
#include "zdk/signal_policy.h"
#include "zdk/thread_util.h"
#include "zdk/utility.h"
#include "zdk/variant_util.h"
#include "zdk/zobject_scope.h"
#include "generic/lock_ptr.h"
#include "generic/temporary.h"
#include "dharma/environ.h"
#include "dharma/process_name.h"
#include "dharma/redirect.h"
#include "dharma/sarray.h"
#include "dharma/symbol_util.h"
#include "dharma/system_error.h"
#include "auto_condition.h"
#include "code_view_common.h"
#include "cursor.h"
#include "entry_dialog.h"
#include "edit_breakpoint_dlg.h"
#include "eval_events.h"
#include "expr_eval_dialog.h"
#include "file_selection.h"
#include "find_dialog.h"
#include "interpreter_box.h"
#include "gui.h"
#include "heap_view.h"
#include "help_viewer.h"
#include "html_view.h"
#include "locals_view.h"
#include "memory_view.h"
#include "message_box.h"
#include "modules_view.h"
#include "new_breakpoint_dlg.h"
#include "options_dlg.h"
#include "output_dialog.h"
#include "program_toolbar2.h"
#include "program_view.h"
#include "progress_box.h"
#include "register_view.h"
#include "run_dialog.h"
#include "select_fun_dialog.h"
#include "select_pid_dialog.h"
#include "signals_dialog.h"
#include "step_over_dialog.h"
#include "stack_view.h"
#include "thread_view.h"
#include "icons/question.xpm"
#include "icons/info2.xpm"
#include "icons/stop.xpm"
#include "slot_macros.h"
#include "main_window.h"
#include "memory_req.h"
#include "memory_req_handler.h"
#include "menu_entry.h"
#include "set_cursor.h"
#include "text_entry.h"
#include "view_types.h"
#include "watch_view.h"

/* "Requests" are commands sent from the main thread to the UI;
 * "responses" are commands from the UI thread to the main thread.
 */
#ifdef DEBUG_POST // trace the posting of requests
 #define POST_COMMAND \
    clog << "POST_COMMAND " << __FILE__ << ':' << __LINE__ << endl; \
    post_command
#else
 #define POST_COMMAND post_command
#endif

#ifdef GTKMM_2
 #define SHIFT "<shift>"
#else
 #define SHIFT
#endif


#define WINDOW_X            "window.x"
#define WINDOW_Y            "window.y"
#define WINDOW_HEIGHT       "window.height"
#define WINDOW_WIDTH        "window.width"
#define WINDOW_MAXIMIZED    "window.maximized"

#define WINDOW_DEFAULT_WIDTH    750
#define WINDOW_DEFAULT_HEIGHT   660

using namespace std;
using namespace SigC;
using Platform::byte_size;

const static bool default_toolbar_visible = true;


#define BEGIN_MENU_GROUP(name) const MenuEntry* MainWindow::name[]={

#define MENU(name,accel,state,func, ...) \
    new MenuItemEntry((name), (accel), (state), &MainWindow::func, NULL, ##__VA_ARGS__)
#define MENU_(name,accel,state,func, path, ...) \
    new MenuItemEntry((name), (accel), (state), &MainWindow::func, (path), ##__VA_ARGS__)
#define SEPARATOR new MenuSeparator
#define END_MENU_GROUP() NULL, };

////////////////////////////////////////////////////////////////
BEGIN_MENU_GROUP(openMenu_)
    MENU("_Source Code...", 0, uisAttachedThreadStop, on_menu_source, &Gtk::Stock::OPEN),
    SEPARATOR(),
    MENU("_Core File...", 0, uisThreadStop, on_menu_load_core),
END_MENU_GROUP()


BEGIN_MENU_GROUP(fileMenu_)
    MENU("E_xecute...", "<control>E", uisThreadStop, on_menu_run, &Gtk::Stock::EXECUTE),
    MENU("_Attach...", 0, uisThreadStop, on_menu_attach),
    MENU("_Detach", 0, uisAttachedThreadStop, on_menu_detach),
    SEPARATOR(),
    MENU("_Save Stack", 0, uisAttachedThreadStop, on_menu_save_stack, &Gtk::Stock::SAVE),
    MENU("C_lose Tab", SHIFT "<control>W", uisCodeViews, on_menu_close, &Gtk::Stock::CLOSE),
    MENU("Close All _Windows", 0, uisCodeViews, on_menu_close_all),
    SEPARATOR(),
#if 0
    // useful for some debugging scenarios, but it may confuse users,
    // as control goes back to the command line
    MENU("_Esc to Command Line", 0, uisThreadStop,
         on_menu_escape_to_command_line
        ),
#endif
    MENU("_Quit", "<control>Q", uisNone, on_menu_quit, &Gtk::Stock::QUIT),
END_MENU_GROUP()



BEGIN_MENU_GROUP(breakpointMenu_)
    MENU_("_New Breakpoint...", "F9",
         uisAttachedThreadStop, on_menu_insert_breakpoints,
         "<Action>/menu_new_breakpoint"
         ),
    MENU_("_Edit...", "<alt>F9", uisAttachedThreadStop | uisBreakPoints,
         on_menu_edit_breakpoints,
         "<Action>/menu_edit_breakpoints"
         ),
    MENU("_Watch...", 0, uisAttachedThreadStop, on_menu_watch),
    MENU("Wa_tchpoints...", 0, uisAttachedThreadStop | uisWatchPoints, on_menu_edit_watchpoints),
    SEPARATOR(),
    MENU_("_Clear All Breakpoints", SHIFT "<control>F9",
         uisAttachedThreadStop | uisBreakPoints,
         on_menu_clear_all_breakpoints,
         "<Action>/menu_clear_breakpoints",
         &Gtk::Stock::CLEAR
         ),
#if DEBUG
    SEPARATOR(),
    MENU("List _All", 0, uisAttachedThreadStop, on_menu_list_breakpoints),
#endif
END_MENU_GROUP()



BEGIN_MENU_GROUP(viewMenu_)
    SEPARATOR(),
    MENU("_Memory", "<control>M", uisAttachedThreadStop, on_menu_memory),
    MENU("M_odules", "<control>O", uisAttached, on_menu_view_modules),
END_MENU_GROUP()



BEGIN_MENU_GROUP(progMenu_)
    MENU_("_Continue", "F5", uisAttachedThreadStop, on_menu_continue,
         "<Action>/menu_continue",
         &Gtk::Stock::MEDIA_PLAY
        ),
    MENU_("_Next", "F10", uisAttachedThreadStop, on_menu_next,
         "<Action>/menu_next"
         ),
    MENU_("_Return From Function", "<control>R",
          uisAttachedThreadStop, on_menu_return,
          "<Action>/menu_return"
         ),
    MENU_("_Step", "F11", uisAttachedThreadStop, on_menu_step,
          "<Action>/menu_step"
         ),
    MENU_("Ste_p Instruction", "<control>F11",
          uisAttachedThreadStop, on_menu_instruction,
          "<Action>/menu_step_instruction"),
    SEPARATOR(),
    MENU_("Restart", 0,
          uisThreadStop | uisCommandLine,
          on_menu_restart,
          "<Action>/menu_restart",
          &Gtk::Stock::REFRESH
         ),
    SEPARATOR(),
    MENU("_Break", "<control>C", uisThreadRun | uisAttached, on_menu_stop),
END_MENU_GROUP()



BEGIN_MENU_GROUP(toolsMenu_)
    MENU("E_valuate...", "<control>V",
        (uisThreadStop | uisAttached), on_menu_variable),
    MENU("_History...", "<control>H", uisThreadStop, on_menu_history, &Gtk::Stock::REVERT_TO_SAVED),
END_MENU_GROUP()



BEGIN_MENU_GROUP(helpMenu_)
    MENU("_Contents", 0, uisNone, on_menu_help, &Gtk::Stock::HELP),
    SEPARATOR(),
    //MENU("Check for _Updates", 0, uisCheckUpdates, on_menu_check_for_updates),
    //SEPARATOR(),
    MENU("_About", 0, uisHTMLEnabled, on_menu_about, &Gtk::Stock::ABOUT),
END_MENU_GROUP()



// main thread and process ids
static pthread_t maintid = 0;
static pid_t mainpid = 0;



namespace
{
    template<typename T>
    CLASS PluginDetector : public EnumCallback<DebuggerPlugin*>
    {
        T* plugin_;

    public:
        PluginDetector() : plugin_(0) { }

        void notify(DebuggerPlugin* plugin)
        {
            if (!plugin_)
            {
                plugin_ = interface_cast<T*>(plugin);
            }
        }
        T* get_plugin() const { return plugin_; }
    };

    static bool have_heap_plugin(Debugger& debugger)
    {
        PluginDetector<Heap> detector;

        debugger.enum_plugins(&detector);
        return detector.get_plugin();
    }
} // namespace


////////////////////////////////////////////////////////////////
MainWindow::MainWindow(Debugger& debugger, const string& strategy)
    : OnMapEventImpl<Gtk::Window>(Gtk_FLAG(WINDOW_TOPLEVEL))
    , AppSlots(debugger)
    , statTop_(0)
    , statText_(0)
    , evalView_(0)
    , progressBox_(new ProgressBox(""))
    , toolbox_(manage(new Gtk::VBox))
    , toolbar_(manage(new ProgramToolBar2(debugger)))
    , toolMenu_(0)
    , breakpointCount_(0)
    , waiting_(false)
    , ownsUserInteraction_(true)
    , atDebugEvent_(0)
    , maximized_(false)
    , viewMode_(0)
{
    maintid = pthread_self();
    mainpid = getpid();

    set_name("MainView");
    set_title("zero: No program");

    Gtk::VBox* vbox = manage(new Gtk::VBox());
    vbox->pack_start(create_menu_bar(), false, false);

    Properties& props = *debugger.properties();

    load_key_bindings(props);

    vbox->pack_start(*toolbox_, false, false);

    if (toolbar_)
    {
        add_toolbar("_Main", *toolbar_, default_toolbar_visible);
        connect_toolbar();
    }
    toolbox_->show();

    statTop_ = manage(new Gtk::Label("No thread"));
    statTop_->show();

    Gtk::Box* workArea = manage(new Gtk::VBox);
    layoutStrategy_ = layoutMgr_.get_strategy(debugger, strategy, *workArea);

    layoutStrategy_->visibility_changed.connect(
        Gtk_SLOT(this, &MainWindow::on_visibility_change));

    create_work_area();

    vbox->pack_start(*statTop_, false, false);
    vbox->pack_start(*workArea, true, true);
    vbox->pack_end(create_status_bar(), false, true);
    vbox->show();
    this->add(*vbox);

    maximized_ = props.get_word(WINDOW_MAXIMIZED, false);
    
    word_t w = props.get_word(WINDOW_WIDTH, WINDOW_DEFAULT_WIDTH);
    word_t h = props.get_word(WINDOW_HEIGHT, WINDOW_DEFAULT_HEIGHT);
    word_t x = props.get_word(WINDOW_X, 0);
    word_t y = props.get_word(WINDOW_Y, 0);

    set_default_size(w, h);

    if (x && y)
    {
        Gtk_move(this, x, y);
    }
    show();

    if (maximized_)
    {
        get_window()->maximize();
    }
    update_state(uisThreadStop);

    connect_event_pipes();

#ifdef GTKMM_2
    // remove the F10 key binding
    if (Glib::RefPtr<Gtk::Settings> settings = get_settings())
    {
        settings->property_gtk_menu_bar_accel().reset_value();
    }
#endif
}


////////////////////////////////////////////////////////////////
MainWindow::~MainWindow() throw()
{
    hide();
}


////////////////////////////////////////////////////////////////
void MainWindow::connect_var_common(VariablesView& view)
{
    view.read_symbol.connect(Gtk_SLOT(this, &AppSlots::read_symbol));
    view.filter.connect(Gtk_SLOT(this, &AppSlots::on_apply_filter));
    view.symbol_expand.connect(Gtk_SLOT(this, &MainWindow::on_symbol_expand));
    view.show_raw_memory.connect(Gtk_SLOT(this, &MainWindow::popup_memory_window));
    view.set_watchpoint.connect(Gtk_SLOT(this, &MainWindow::on_menu_watchpoint));
    view.describe_type.connect(Gtk_SLOT(this, &MainWindow::on_describe_type));
    view.edit.connect(Gtk_SLOT(this, &MainWindow::on_variable_edit));
}


///////////////////////////////////////////////////////////////
void MainWindow::connect_toolbar()
{
    assert(toolbar_);
    toolbar_->tool_run.connect(Gtk_SLOT(this, &MainWindow::on_menu_continue));
    toolbar_->tool_step.connect(Gtk_SLOT(this, &MainWindow::on_menu_step));
    toolbar_->tool_next.connect(Gtk_SLOT(this, &MainWindow::on_menu_next));
    toolbar_->tool_instruction.connect(Gtk_SLOT(this, &MainWindow::on_menu_instruction));
    toolbar_->tool_return.connect(Gtk_SLOT(this, &MainWindow::on_menu_return));
    toolbar_->tool_break.connect(Gtk_SLOT(this, &MainWindow::on_menu_stop));
    toolbar_->tool_eval.connect(Gtk_SLOT(this, &MainWindow::on_menu_variable));
    toolbar_->font_set.connect(Gtk_SLOT(this, &MainWindow::on_toolbar_font_set));
}


////////////////////////////////////////////////////////////////
void MainWindow::layout_work_area()
{
    layoutStrategy_->add_program_view(*progView_);
    layoutStrategy_->add_stack_view(*stackView_);
    layoutStrategy_->add_registers_view(*registerView_);
    layoutStrategy_->add_threads_view(*threadView_);
    layoutStrategy_->add_variables_view(*localVarView_);
    layoutStrategy_->add_watches_view(*watches_);
#ifdef GTKMM_2
    for (vector<InterpBoxPtr>::iterator i = interp_.begin();
         i != interp_.end();
         ++i)
    {
        layoutStrategy_->add_interpreter_box(**i, (*i)->name());
    }
#endif
    layoutStrategy_->container().show_all();
}


////////////////////////////////////////////////////////////////
void MainWindow::layout_work_area(const string& strategy)
{
    Gtk::Container& area = layoutStrategy_->container();

    //remove all widgets from work area
    area.foreach(Gtk_SLOT(&area, &Gtk::Container::remove));

    layoutStrategy_ = layoutMgr_.get_strategy(debugger(), strategy, area);
    assert(layoutStrategy_); // by  get_strategy's contract

    layout_work_area();
}


////////////////////////////////////////////////////////////////
void MainWindow::create_work_area()
{
    // displays source code or assembly listing
    progView_.reset(new ProgramView(debugger()));
    CHKPTR(toolbar_)->set_font_name(progView_->font_name());

    memReqHandler_.reset(new MemoryRequestHandler(*this, progView_));

    progView_->read_memory_async.connect(
        Gtk_SLOT(memReqHandler_.get(),
                 &MemoryRequestHandler::on_read_memory));
    progView_->read_symbol.connect(Gtk_SLOT(this, &AppSlots::read_symbol));

    connect_codeview();

    stackView_.reset(new StackView);

    Gtk_CONNECT_0(  stackView_.get(),
                    selection_changed,
                    this,
                    &MainWindow::on_stack_selection);

    stackView_->set_data(STATE_MASK, (void*)uisThreadStop);

    registerView_.reset(new RegisterView);
    registerView_->set_value.connect(Gtk_SLOT(this, &MainWindow::on_set_reg));

    create_thread_view();
    create_local_vars_view();
    create_watches_view();

    create_interp();

    layout_work_area();
}


////////////////////////////////////////////////////////////////
void MainWindow::create_watches_view()
{
    WatchView* w = new WatchView(debugger());
    watches_.reset(w);

    connect_var_common(*watches_);

    w->signal_evaluate()->connect(Gtk_SLOT(this, &MainWindow::on_evaluate));
    w->signal_refresh()->connect(Gtk_SLOT(this, &MainWindow::on_refresh));
}


////////////////////////////////////////////////////////////////
void MainWindow::create_local_vars_view()
{
    assert(!localVarView_.get());
    localVarView_.reset(new LocalsView(debugger()));

    localVarView_->set_data(STATE_MASK,
        reinterpret_cast<gpointer>(uisThreadStop | uisAttached));
    if (RefPtr<Properties> props = debugger().properties())
    {
        localVarView_->restore_settings(*props, "locals");
    }
    // connect common VariableView signals to slots
    connect_var_common(*localVarView_);
}


////////////////////////////////////////////////////////////////
void MainWindow::create_thread_view()
{
    assert(threadView_.get() == 0);

    threadView_.reset(new ThreadView("Threads", *this));
    threadView_->set_data(STATE_MASK,
        reinterpret_cast<void*>(uisThreadStop | uisAttached));
    threadView_->selection_changed.connect(
        Gtk_SLOT(this, &MainWindow::on_thread_selection));
}


namespace
{
    //
    // Helper for MainWindow::create_interp
    //
    class ZDK_LOCAL InterpDetector : public EnumCallback<DebuggerPlugin*>
    {
        MainWindow& w_;
        void (MainWindow::*delegate_)(Interpreter*);

    public:
        InterpDetector(MainWindow& w,
                       void (MainWindow::*delegate)(Interpreter*))
             : w_(w)
             , delegate_(delegate)
        { }

        void notify(DebuggerPlugin* plugin)
        {
            if (Interpreter* interp = interface_cast<Interpreter*>(plugin))
            {
                (w_.*delegate_)(interp);
            }
        }
    };
}


////////////////////////////////////////////////////////////////
void MainWindow::create_interp()
{
#if defined(GTKMM_2)
    InterpDetector detect(*this, &MainWindow::create_interp_box);
    debugger().enum_plugins(&detect);
#endif
}


////////////////////////////////////////////////////////////////
void MainWindow::create_interp_box(Interpreter* interp)
{
#if defined(GTKMM_2)
    InterpBoxPtr box(new InterpreterBox(this, interp));
    interp_.push_back(box);
#endif
}


////////////////////////////////////////////////////////////////
Gtk::Widget& MainWindow::create_status_bar()
{
    Gtk::HBox* box = manage(new Gtk::HBox);

    funcBar_ = manage(new Gtk::Entry);
    box->pack_start(*funcBar_);
    box->set_spacing(3);
    funcBar_->set_editable(false);

    statText_ = manage(new Gtk::Entry);
    Gtk_set_size(statText_, 260, -1);
    box->pack_end(*statText_, false, false);

    statText_->set_editable(false);

    box->set_border_width(2);
    box->show_all();

    return *box;
}


////////////////////////////////////////////////////////////////
Gtk::Widget& MainWindow::create_menu_bar()
{
#define MENU_ELEM(name, fun) \
    MenuElem(name, Gtk_SLOT(this, &MainWindow::fun))

    using namespace Gtk::Menu_Helpers;

    Gtk::MenuBar* menuBar = manage(new ZMenuBar());
#ifndef GTKMM_2
    menuBar->set_shadow_type(Gtk_FLAG(SHADOW_OUT));
#endif

    /*** File ***/
    Gtk::Menu* menuFile(manage(new Gtk::Menu()));

    Gtk::Menu* menuOpen(manage(new Gtk::Menu()));
    create_menu_elem(*menuOpen, openMenu_);

    menuFile->items().push_back(MenuElem("_Open", *menuOpen));

    create_menu_elem(*menuFile, fileMenu_);

    /*** Edit ***/
    Gtk::Menu* menuEdit(manage(new Gtk::Menu()));
    MenuList& listEdit = menuEdit->items();

    /********************* Find Text **************************/
#if 0
    listEdit.push_back(Gtk_MENU_ELEM("_Find...", "<control>F",
        Gtk_SLOT(this, &MainWindow::on_menu_find)));
    Gtk_MENU_ITEM(listEdit.back()).set_data(STATE_MASK, (gpointer)uisAttached);
#else
    MenuItemEntry("_Find...", "<control>F", uisAttached, &MainWindow::on_menu_find,
                   0, &Gtk::Stock::FIND).create(this, listEdit);
#endif

    listEdit.push_back(Gtk_MENU_ELEM("Find A_gain", "<control>G",
        Gtk_SLOT(this, &MainWindow::on_menu_find_again)));
    Gtk_MENU_ITEM(listEdit.back()).set_data(STATE_MASK,
        reinterpret_cast<gpointer>(uisAttached /* | uisFindAgain */));

    listEdit.push_back(SeparatorElem());

    /******************** Lookup Symbol ***********************/
    listEdit.push_back(Gtk_MENU_ELEM("_Lookup Function...", "<control>L",
        Gtk_SLOT(this, &MainWindow::on_menu_lookup_symbol)));
    Gtk_MENU_ITEM(listEdit.back()).set_data(STATE_MASK,
        reinterpret_cast<gpointer>(uisAttached));

    listEdit.push_back(SeparatorElem());
#if 0
    listEdit.push_back(Gtk_MENU_ELEM("_Options...", "",
        Gtk_SLOT(this, &MainWindow::on_menu_edit_options)));

    //Do no allow options to be changed while target (debuggee) is running,
    //since it may have bad side effects. For example, trying to change the
    //layout strategy causes the UI to hang up until an event occurs in the
    //target process
    Gtk_MENU_ITEM(listEdit.back()).set_data(STATE_MASK, (gpointer)uisThreadStop);
#else
    MenuItemEntry("_Options...", "", uisThreadStop, &MainWindow::on_menu_edit_options,
                   0, &Gtk::Stock::PREFERENCES).create(this, listEdit);
#endif
    /*** View ***/
    Gtk::Menu* menuView(manage(new Gtk::Menu()));
    Gtk::Menu* menuSub(manage(new Gtk::Menu()));

    static Gtk::RadioMenuItem::Group g;

#define RADIO_ELEM(name, arg) RadioMenuElem(g, name, \
        Gtk_BIND(Gtk_SLOT(this, &MainWindow::on_menu_view_type), arg))

    viewMode_ = &menuSub->items();
    viewMode_->push_back(RADIO_ELEM("_Source", VIEW_SOURCE));
    viewMode_->push_back(RADIO_ELEM("_Disassembled", VIEW_DISASSEMBLED));
    viewMode_->push_back(RADIO_ELEM("_Mixed", VIEW_MIXED));
#undef RADIO_ELEM

    MenuList& items = menuView->items();
    items.push_back(MenuElem("_Code", *menuSub));

    Gtk_MENU_ITEM(items.back()).set_data(STATE_MASK,
        (void*)(uisThreadStop | uisSymbolicView));

    toolMenu_ = manage(new Gtk::Menu());
    items.push_back(MenuElem("_Toobars", *toolMenu_));

    create_menu_elem(*menuView, viewMenu_);

    if (have_heap_plugin(debugger()))
    {
        menuView->items().push_back(SeparatorElem());
        menuView->items().push_back(
            Gtk_MENU_ELEM("_Heap...", "", Gtk_SLOT(this, &MainWindow::on_menu_heap)));
    }
#ifdef DEBUG_OBJECT_LEAKS
    items.push_back(SeparatorElem());
    items.push_back(Gtk_MENU_ELEM("_Dump Counted Objects", "",
        Gtk_SLOT(this, &MainWindow::print_instance_counted)));
#endif

    /*** Help Menu ***/
    Gtk::Menu* menuHelp(manage(new Gtk::Menu));
    create_menu_elem(*menuHelp, helpMenu_);

    /*** Tools Menu ***/
    Gtk::Menu* menuTools(manage(new Gtk::Menu));

    create_menu_elem(*menuTools, toolsMenu_);

    /*** Program Menu ***/
    Gtk::Menu* menuProg(manage(new Gtk::Menu));

    create_menu_elem(*menuProg, progMenu_);

    /*** Breakpoints Menu ***/
    Gtk::Menu* menuBreak(manage(new Gtk::Menu()));
    create_menu_elem(*menuBreak, breakpointMenu_);

    MenuList& bar = menuBar->items();
    bar.push_front(Gtk_MENU_ELEM("_Help", "<alt>h", *menuHelp));

    bar.push_front(Gtk_MENU_ELEM("_Tools", "<alt>t", *menuTools));
    //bar.push_front(Gtk_MENU_ELEM("_Program", "<alt>p", *menuProg));
    bar.push_front(Gtk_MENU_ELEM("_Debug", "<alt>d", *menuProg));

    bar.push_front(Gtk_MENU_ELEM("_Breakpoints", "<alt>b", *menuBreak));
    Gtk_MENU_ITEM(bar.front()).set_data(STATE_MASK,
        reinterpret_cast<gpointer>(uisThreadStop | uisAttachedLive));

    // add all other menus to menubar
    bar.push_front(Gtk_MENU_ELEM("_View", "<alt>v", *menuView));
    bar.push_front(Gtk_MENU_ELEM("_Edit", "<alt>e", *menuEdit));
    bar.push_front(Gtk_MENU_ELEM("_File", "<alt>f", *menuFile));

    menuBar->show_all();

#undef MENU_ELEM
    return *menuBar;
}


////////////////////////////////////////////////////////////////
void
MainWindow::create_menu_elem(Gtk::Menu_Helpers::MenuList& list,
                             const MenuEntry& entry)
{
    entry.create(this, list);
}


////////////////////////////////////////////////////////////////
void MainWindow::create_menu_elem(Gtk::Menu& menu,
                                  const MenuEntry* entry [])
{
    Gtk::Menu_Helpers::MenuList& menuList = menu.items();
    menu.set_accel_group(get_accel_group());

    for (; entry && *entry; ++entry)
    {
        create_menu_elem(menuList, **entry);
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::error_message(string message)
{
    message_box(message, stop_xpm);
}


////////////////////////////////////////////////////////////////
void MainWindow::info_message(string message)
{
    message_box(message, info2_xpm);
}


////////////////////////////////////////////////////////////////
void MainWindow::error_message_async(const string& message)
{
    message_box(message, stop_xpm, true);
}


////////////////////////////////////////////////////////////////
void MainWindow::info_message_async(const string& message)
{
    message_box(message, info2_xpm, true);
}


////////////////////////////////////////////////////////////////
void MainWindow::question_message
(
    string msg,
    bool* result,
    const char* opt
)
{
    assert(result);

    if (!is_ui_thread())
    {
        run_on_ui_thread(command(&MainWindow::question_message, this, msg, result, opt));
    }
    else
    {
        if (opt)
        {
            if (CHKPTR(debugger().properties())->get_word(opt, 0))
            {
                // todo: result should be whatever last response was
                *result = true;
                return;
            }
        }
        MessageBox dlg(msg, MessageBox::btn_yes_no, "Confirm", question_xpm);
        Gtk::CheckButton* chk = NULL;
        if (opt)
        {
            chk = manage(new Gtk::CheckButton("Do not ask this again, just do it!", .0));
            dlg.get_vbox()->add(*chk);
        }
        dlg.set_transient_for(*this);

        *result = (dlg.run(this) == MessageBox::btn_yes);
        if (chk && chk->get_active())
        {
            CHKPTR(opt);
            CHKPTR(debugger().properties())->set_word(opt, 1);
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::message_box(string msg, const char* pix[])
{
    return message_box(msg, pix, false);
}


////////////////////////////////////////////////////////////////
void
MainWindow::message_box(const string& msg, const char* pix[], bool async)
{
    if (!is_ui_thread())
    {
        CommandPtr cmd = command(&MainWindow::message_box, this, msg, pix);

        if (async)
        {
            post_request(cmd);
        }
        else
        {
            run_on_ui_thread(cmd);
        }
    }
    else
    {
        MessageBox mbox(msg, MessageBox::btn_ok, 0, pix);

        if (Gtk::Dialog* dialog = DialogBox::active_dialog())
        {
            //
            // show message box on top of currently active dialog
            //
            mbox.set_transient_for(*dialog);
        }
        else
        {
            mbox.set_transient_for(*this);
        }
        mbox.run(this);
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::status_message(string text)
{
    if (!is_ui_thread())
    {
        post_safe_command(&MainWindow::status_message, this, text);
        usleep(1);
    }
    else
    {
        assert_ui_thread();

        funcBar_->set_text(text);

    #ifdef GTKMM_2
        funcBar_->get_window()->process_updates(true);
    #endif
    }
}


////////////////////////////////////////////////////////////////
bool MainWindow::on_window_state_event(GdkEventWindowState* event)
{
    if (event)
    {
        if (event->changed_mask & GDK_WINDOW_STATE_MAXIMIZED)
        {
            maximized_ = event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED;
    
            Properties& props = *CHKPTR(debugger().properties());
            props.set_word(WINDOW_MAXIMIZED, maximized_);
        }
    }

    dbgout(1) << "maximized=" << maximized_ << endl;

    return OnMapEventImpl<Gtk::Window>::on_window_state_event(event);
}


////////////////////////////////////////////////////////////////
void MainWindow::on_size_allocate(Gdk::Rectangle& allocation)
{
    OnMapEventImpl<Gtk::Window>::on_size_allocate( allocation );
    
    if (is_ui_thread())
    {
        save_geometry();
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_visibility_change,(NotebookPtr))
{
    if (layoutStrategy_ && layoutStrategy_->is_variables_view_visible())
    {
        if (is_ui_thread())
        {
            CALL_MAIN_THREAD();
        }
        else if (localVarView_)
        {
            localVarView_->update(current_thread());
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
event_result_t MainWindow::on_delete_event(GdkEventAny*)
{
    on_menu_quit();
    return true;
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_change_state,(Gtk::Widget& w, size_t state))
{
    const size_t mask = reinterpret_cast<size_t>(w.get_data(STATE_MASK));
    if (mask & uisDisable)
    {
        w.set_sensitive(false);
        return;
    }
    bool apply = (mask != 0);

    if (mask & uisHTMLEnabled)
    {
        apply = (state & (uisHTMLEnabled | uisHTMLDisabled));
    }
    else
    {
        apply &= ((state & (uisHTMLEnabled | uisHTMLDisabled)) == 0);
    }
    if (apply)
    {
        const bool sensitive = (mask == uisAny) || (mask & state) == mask;

        if (w.is_visible())
        {
        #ifdef GTKMM_2
            // hackish workaround for ToolButton bug
            // http://bugzilla.gnome.org/show_bug.cgi?id=56070
            if (GTK_IS_TOOL_BUTTON(&w))
            {
               w.hide();
               w.show();
            }
        #endif
            w.set_sensitive(sensitive);
        }
        if (!sensitive)
        {
            return;
        }
    }
    if (Gtk::MenuItem* mi = dynamic_cast<Gtk::MenuItem*>(&w))
    {
        if (Gtk::Menu* sub = mi->get_submenu())
        {
            sub->foreach(Gtk_BIND(Gtk_PTR_FUN(&MainWindow::on_change_state), state));
        }
    }
    else if (Gtk::Container* c = dynamic_cast<Gtk::Container*>(&w))
    {
        c->foreach(Gtk_BIND(Gtk_PTR_FUN(&MainWindow::on_change_state), state));
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_stack_selection,())
{
    if (!is_shutting_down())
    {
        assert_ui_thread();

        if (!CHKPTR(stackView_)->is_change_programmatic()
         && !CHKPTR(stackView_)->selection().empty())
        {
            if (!is_debuggee_running())
            {
                set_cursor(*this, Gdk_FLAG(WATCH));
                CALL_MAIN_THREAD();
            }
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_thread_selection,(pid_t lwpid,
                                            unsigned long id))
{
    if (is_ui_thread() && !is_current_thread(lwpid, id))
    {
        if (Thread* thread = debugger().get_thread(lwpid, id))
        {
            set_title(string("zero: ") + thread->filename());
            CALL_MAIN_THREAD_( 
                command(&Debugger::set_current_thread, &debugger(), thread) );
        }
        else
        {
            cerr << "cannot get thread " << lwpid << "/" << id << endl;
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_edit_signals,())
{
    SignalsDialog dlg(debugger());
    dlg.set_transient_for(*this);

    for (;;)
    {
        try
        {
            dlg.run(this);
            break;
        }
        catch (exception& e)
        {
            error_message(e.what());
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_edit_options,())
{
    OptionsDialog dlg(debugger());
    dlg.set_transient_for(*this);
    dlg.run(*this);
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_find,())
{
    FindDialog dialog(debugger().properties());

    dialog.set_text(progView_->selection());
    dialog.get_selection.connect(
        Gtk_SLOT(progView_.get(), &ProgramView::get_selection));
    dialog.find_in_file.connect(
        Gtk_SLOT(progView_.get(), &ProgramView::search));

    dialog.set_transient_for(*this);
    dialog.run();
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_find_again,())
{
    progView_->search_again();
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT_(bool, MainWindow::on_not_found, (const string& str))
{
    string msg = "String not found: " + str + ". Restart from top?";
    MessageBox dlg(msg, MessageBox::btn_yes_no, 0, question_xpm);
    dlg.set_transient_for(*this);

    return dlg.run(this) == MessageBox::btn_yes;
}
END_SLOT_(false)


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_clear_all_breakpoints,())
{
    /**
     * Helper callback
     */
    struct ZDK_LOCAL Enum : public EnumCallback<volatile BreakPoint*>
    {
        vector<BreakPointSite> locs_;

        void notify(volatile BreakPoint* bpnt)
        {
            if (CHKPTR(bpnt)->enum_actions("USER"))
            {
                locs_.push_back(BreakPointSite(*bpnt));
            }
        }

        virtual ~Enum() throw() {} // disable compiler warning
    };

    bool confirm = false;
    question_message("Do you want to remove all breakpoints?", &confirm);
    if (confirm)
    {
        Enum enumCB;
        if (BreakPointManager* mgr = debugger().breakpoint_manager())
        {
            mgr->enum_breakpoints(&enumCB);
        }
        if (!enumCB.locs_.empty())
        {
            on_delete_breakpoints(enumCB.locs_);
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_list_breakpoints,())
{
    Pipe pipe;

    // list all breakpoints, internal and user-defined,
    // for debugging purposes
    OutputDialog output("Breakpoints", pipe.output());

    //static const char msg[] = "***** BREAKPOINTS *****\n"
    //pipe.write(&msg, sizeof(msg) - 1);

    set_service_call(true);
    post_debugger_command("show breakpoints", pipe.input());

    output.set_transient_for(*this);
    output.run(this);
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::add_to_breaklist(
    pid_t           tid,
    Symbol*         sym,
    vector<addr_t>& list)
{
    ZObjectScope scope;
    const addr_t addr = CHKPTR(sym)->addr();
    const SymbolTable* table = sym->table(&scope);

    if (table && table->addr())
    {
        list.push_back(addr);
    }
    else
    {
        //sym->set_deferred_breakpoint(BreakPoint::GLOBAL);
        //call it on main thread to avoid races
        CALL_MAIN_THREAD_(command(&Symbol::set_deferred_breakpoint,
                                  sym,
                                  BreakPoint::GLOBAL,
                                  (Runnable*)0,
                                  (BreakPointAction*)0,
                                  sym));

        string msg = "Added deferred breakpoint";
        if (table)
        {
            msg += " (will activate when ";
            msg += CHKPTR(table->filename())->c_str();
            msg += " is loaded)";
        }
        info_message(msg);
        refresh_state();
    }
}


////////////////////////////////////////////////////////////////
static bool
check_disabled_breakpoints(Debugger&       debugger,
                           MainWindow&     wnd,
                           vector<addr_t>& addrList)
{
    assert_ui_thread();
    vector<addr_t>::iterator i = addrList.begin();
    while (i != addrList.end())
    {
        if (!has_disabled_user_breakpoint_actions(debugger, *i))
        {
            ++i;
        }
        else
        {
            ostringstream msg;
            msg << "There is a disabled breakpoint at 0x";
            msg << hex << *i << ". Enable it?";

            MessageBox mbox(msg.str(),
                            MessageBox::btn_yes_no_cancel,
                            0,
                            stop_xpm);

            if (Gtk::Dialog* parent = DialogBox::active_dialog())
            {
                mbox.set_transient_for(*parent);
            }
            else
            {
                mbox.set_transient_for(wnd);
            }

            switch (mbox.run(&wnd))
            {
            default: assert(false);

            case MessageBox::btn_no:
                i = addrList.erase(i);
                break;

            case MessageBox::btn_cancel:
                return false;

            case MessageBox::btn_yes: ++i;
                break;
            }
        }
    }

    return !addrList.empty();
}


/**
 * Helper for auto-completing function names as the user
 * types them. The workhorse is the AppSlots class,
 * this helper provides some "glue".
 *
 * @note inherits off Gtk::Base for compatibility with
 * old Gtk-1.2
 */
class ZDK_LOCAL AutoComplete : public Gtk::Base
{
    AppSlots& app_;
    bool useUnmappedLibs_;
    Gtk::ToggleButton* btn_;

public:
    AutoComplete(AppSlots& app, Gtk::ToggleButton* btn)
        : app_(app)
        , useUnmappedLibs_(false)
        , btn_(btn)
    { }

    bool run(TextEntry* entry, string str)
    {
        return app_.on_auto_complete(entry, str, useUnmappedLibs_);
    }

    void on_toggle(bool useUnmappedLibs)
    {
        useUnmappedLibs_ = useUnmappedLibs;
    }

    void on_btn_toggle()
    {
        if (btn_)
        {
            useUnmappedLibs_ = btn_->get_active();
        }
    }

    bool use_unmapped_libs() const { return useUnmappedLibs_; }
};


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_insert_breakpoints,())
{
    assert_ui_thread();
    if (!current_thread())
    {
        return;
    }
    AutoComplete autoComplete(*this, NULL);

#if 0

    /**********************************************************/
    // experimental / WIP: redesign the "Insert New Breakpoint" dialog
    NewBreakPointDialog dialog("New Breakpoint", debugger().properties());

    Gtk_set_size(&dialog, 400, -1);
    dialog.use_unmapped_toggled.connect(
        Gtk_SLOT(autoComplete, &AutoComplete::on_toggle));

    dialog.set_transient_for(*this);
    dialog.auto_complete_signal().connect(
        Gtk_SLOT(autoComplete, &AutoComplete::run));

    dialog.set_transient_for(*this);

    string str = dialog.run(this);

#else
    const char message[] = "Enter function name or address:";

    EntryDialog box(message, debugger().properties(), "Breakpoint");
    box.set_transient_for(*this);

    Gtk_set_size(&box, 400, -1);

    if (TextEntry* combo = box.text_entry())
    {
        combo->set_may_auto_complete(true);
        combo->auto_complete.connect(Gtk_SLOT(autoComplete,
                                              &AutoComplete::run));
    }

    string str = box.run();

#endif

    hide_auto_completion_window();

    if (!str.empty())
    {
        set_cursor(*this, Gdk_FLAG(WATCH));
        update_state(uisThreadRun);

        CALL_MAIN_THREAD_(command(&MainWindow::set_breakpoint_on_main_thread,
                                 this,
                                 str,
                                 current_thread()));
    }
}
END_SLOT()


class ZDK_LOCAL SourceSymbolEnum : public SymbolEnum
{
public:
    /**
     * include only symbols for which we have source info
     */
    void notify(Symbol* symbol)
    {
        if (symbol && symbol->line())
        {
            SymbolEnum::notify(symbol);
        }
    }
};


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_lookup_symbol,())
{
    if (!current_thread())
    {
        return;
    }
    EntryDialog dlg("Enter the fully-qualified function name",
                    debugger().properties(),
                    "Lookup Function");

    dlg.set_transient_for(*this);

    // check button
    const char label[] = "Look inside _unmapped modules";
    Gtk::CheckButton* btn = manage(new Gtk::CheckButton(label));
    dlg.add_button_accelerator(*btn);
    CHKPTR(dlg.get_box())->add(*btn);

    AutoComplete autoComplete(*this, btn);
    Gtk_CONNECT_0(btn, toggled, &autoComplete,
                 &AutoComplete::on_btn_toggle);

    // add button for showing modules
    Gtk::Button* btnViewMod = dlg.add_button("View M_odules");
    Gtk_CONNECT_0(btnViewMod, clicked, this,
                  &MainWindow::on_menu_view_modules);

    dlg.text_entry()->auto_complete.connect(
            Gtk_SLOT(autoComplete, &AutoComplete::run));
    dlg.text_entry()->set_may_auto_complete(true);

    string name = dlg.run(this);

    hide_auto_completion_window();

    if (name.empty())
    {
        return;
    }

    // now find the symbols that match the given name
    SourceSymbolEnum syms;

    SymbolTable::LookupMode mode =
            SymbolTable::LKUP_DYNAMIC | SymbolTable::LKUP_RANGE;
    if (autoComplete.use_unmapped_libs())
    {
        mode = mode | SymbolTable::LKUP_UNMAPPED;
    }

    current_thread()->symbols()->enum_symbols(name.c_str(), &syms, mode);

    if (syms.empty())
    {
        error_message("Symbol source not found: " + name);
    }
    else if (syms.size() == 1)
    {
        // open source file for symbols
        RefPtr<Symbol> sym = syms.front();

        const char* filename = "";
        if (SharedString* file = sym->file())
        {
            filename = CHKPTR(file->c_str());
        }
        progView_->set_current(current_thread(), filename);
        progView_->current_view().goto_line(sym->line());
    }
    else
    {
        // several matches found, ask the user to refine
        // the selection
        SelectFunDialog dlg(syms.begin(), syms.end(),
            "Please select the desired function(s) "
            "from the following possible matches:");

        dlg.set_transient_for(*this);
        vector<Gtk::SelectionItem> sel = dlg.run(this);

        vector<Gtk::SelectionItem>::const_iterator i = sel.begin();
        for (; i != sel.end(); ++i)
        {
            Symbol* sym = reinterpret_cast<Symbol*>(Gtk_USER_DATA(*i));
            assert(sym);
            const char* filename = sym->file() ? sym->file()->c_str() : "";
            progView_->set_current(current_thread(), filename);
            progView_->current_view().goto_line(sym->line());
        }
    }
}
END_SLOT()



////////////////////////////////////////////////////////////////
void
MainWindow::set_breakpoint_on_main_thread(
    string          spec,
    RefPtr<Thread>  thread)
{
    assert_main_thread();

    vector<RefPtr<Symbol> > sym;

    if (thread)
    {
        RefPtr<SymbolMap> symbols = CHKPTR(thread->symbols());

        if (isdigit(spec[0]))
        {
            addr_t addr = strtoul(spec.c_str(), 0, 0);
            if (Symbol* s = symbols->lookup_symbol(addr))
            {
                sym.push_back(s);
            }
        }
        else
        {
            // include dynamic tables in our search so
            // that we don't miss things such as __cxa_throw
            static const SymbolTable::LookupMode mode =
                SymbolTable::LKUP_DYNAMIC | SymbolTable::LKUP_UNMAPPED;

            SymbolEnum funcs;
            symbols->enum_symbols(spec.c_str(), &funcs, mode);

            sym.assign(funcs.begin(), funcs.end());
        }
    }

    post_command(&MainWindow::set_breakpoints_on_ui_thread,
                 this, spec, thread, sym);
}


////////////////////////////////////////////////////////////////
void
MainWindow::set_breakpoints_on_ui_thread(
    string str, // symbol name or address
    RefPtr<Thread> thread,
    vector<RefPtr<Symbol> > syms)
{
    assert_ui_thread();

    if (!thread)
    {
        return;
    }

    const pid_t lwpid = thread->lwpid();
    vector<addr_t> addrs;
    if (syms.empty())
    {
        error_message("Symbol not found: " + str);
        on_menu_insert_breakpoints();
    }
    else if (syms.size() == 1)
    {
        Symbol* sym = syms.front().get();
        add_to_breaklist(lwpid, sym, addrs);
    }
    else
    {
        SelectFunDialog dlg(syms.begin(), syms.end(),
            "Please select the desired function(s) "
            "from the following possible matches:");

        dlg.set_transient_for(*this);
        vector<Gtk::SelectionItem> sel = dlg.run(this);
        addrs.reserve(sel.size());

        vector<Gtk::SelectionItem>::const_iterator i = sel.begin();
        for (; i != sel.end(); ++i)
        {
            Symbol* sym = reinterpret_cast<Symbol*>(Gtk_USER_DATA(*i));
            add_to_breaklist(lwpid, sym, addrs);
        }
    }
    if (check_disabled_breakpoints(debugger(), *this, addrs))
    {
        dbgout(0) << __func__ << endl;
        // finally, callback into the main thread to actually
        // insert the breakpoints.
        CALL_MAIN_THREAD_(command(&MainWindow::insert_breakpoints,
                                  this,
                                  thread,
                                  addrs,
                                  false,    // silent
                                  true));   // permanent
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_edit_breakpoints,())
{
    EditBreakPointDialog dlg(debugger(), current_thread().get());

    dlg.delete_breakpoints.connect(
        Gtk_SLOT(this, &MainWindow::on_delete_breakpoints));

    dlg.breakpoint_modified.connect(
        Gtk_BIND(Gtk_SLOT(this, &MainWindow::update_breakpoint_view), 0));

    assert(progView_.get());

    dlg.show_code.connect(Gtk_SLOT(progView_.get(),
                                   &ProgramView::show_breakpoint));
    dlg.set_transient_for(*this);
    dlg.run(this);
}
END_SLOT()



////////////////////////////////////////////////////////////////
void MainWindow::update_breakpoint_view(addr_t addr, size_t line)
{
    if (line)
    {
        CHKPTR(progView_)->refresh_line(line);
    }
    else
    {
        CHKPTR(progView_)->breakpoint_state_changed(addr);
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_set_program_count, (addr_t addr))
{
    assert_ui_thread();
    bool confirm = false;
    question_message("This operation will alter the program counter \n"
                     "register of the current thread, are you sure?",
                    &confirm);
    if (!confirm)
    {
        return;
    }

    if (RefPtr<Thread> thread = current_thread())
    {
        Runnable& task = interface_cast<Runnable&>(*thread);
        CALL_MAIN_THREAD_(command(&Runnable::set_program_count, &task, addr));
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_variable,())
{
    DebugSymbolList empty;
    on_menu_evaluate(empty);
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_evaluate,(DebugSymbolList syms))
{
    if (!current_thread())
    {
        error_message("Current thread is NULL");
        return;
    }
    // create dialog for evaluating variables and expressions
    ExprEvalDialog dialog(debugger(), *current_thread());
    {
        Temporary<ExprEvalDialog*, Mutex> setInScope(evalView_, &dialog, &evalMutex_);

        dialog.signal_evaluate()->connect(Gtk_SLOT(this, &MainWindow::on_evaluate));
        dialog.signal_refresh()->connect(Gtk_SLOT(this, &MainWindow::on_refresh));
        dialog.signal_eval_error()->connect(Gtk_SLOT(this, &MainWindow::on_eval_error));
        dialog.signal_eval_warn()->connect(Gtk_SLOT(this, &MainWindow::info_message));
        dialog.signal_confirm()->connect(Gtk_SLOT(this, &MainWindow::question_message));

        connect_var_common(*dialog.view());

        if (!syms.empty())
        {
        #if 1
            // if we get several matches, C/C++ language rules dictate
            // that they are in different scopes: show just the innermost
            // symbol

            dialog.view()->notify(syms.back().get());
        #else

            for (DebugSymbolList::iterator i = syms.begin(); i != syms.end(); ++i)
            {
                dialog.view()->notify(i->get());
            }
        #endif
        }

        const string selection = progView_->selection();
        dialog.text_entry()->set_text(selection);
        dialog.set_transient_for(*this);
        dialog.run(this);
    }
    assert(!evalView_);
}
END_SLOT()



////////////////////////////////////////////////////////////////
void MainWindow::insert_breakpoints (
    RefPtr<Thread>  thread,
    vector<addr_t>  addr, // by value purposely
    bool            silent,
    bool            permanent
    )
{
    assert(pthread_self() == maintid);
    assert(thread);

    ostringstream msg;
    size_t count = 0;

    Runnable* runnable = get_runnable(thread.get());

    vector<addr_t>::const_iterator i(addr.begin());
    for (; i != addr.end(); ++i)
    {
        dbgout(0) << __func__ << ": " << (void*)*i << endl;

        if (permanent)
        {
            // set a GLOBAL breakpoint:
            if (debugger().set_user_breakpoint(runnable, *i, true))
            {
                ++count;
            }
            else
            {
                msg << "Breakpoint already exists at 0x" << hex << *i << endl;
            }
        }
        else
        {
            debugger().set_temp_breakpoint(runnable, *i);
        }
    }
    debugger().save_properties();

    if (!silent)
    {
        // if silent, don't popup a message box; this is useful
        // when breakpoints are inserted as a result of the
        // mouse right-click menu

        report_breakpoints_inserted(msg, count);
    }
}


////////////////////////////////////////////////////////////////
void
MainWindow::report_breakpoints_inserted(ostringstream& msg, size_t count)
{
    msg << count << " breakpoint";
    if (count > 1)
    {
        msg << "s";
    }
    msg << " inserted.";

    //POST_COMMAND(&MainWindow::info_message, this, msg.str());
    MainWindow::info_message(msg.str());
}



////////////////////////////////////////////////////////////////
bool MainWindow::insert_breakpoints_and_resume
(
    RefPtr<Thread> thread,
    vector<addr_t> addrs, // by value purposely
    bool permanent
)
{
    insert_breakpoints(thread, addrs, true, permanent);

    schedule_response(new ResumeCommand);
    return true; // resume
}


////////////////////////////////////////////////////////////////
void MainWindow::insert_deferred_breakpoints (
    RefPtr<Thread> thread,
    vector<RefPtr<Symbol> > symbols,
    bool silent,
    bool /* permanent */
    )
{
    assert(pthread_self() == maintid);
    assert(thread);

    ostringstream msg;
    vector<RefPtr<Symbol> >::const_iterator i = symbols.begin();
    for (; i != symbols.end(); ++i)
    {
        (*i)->set_deferred_breakpoint(BreakPoint::GLOBAL /* FIXME */);
    }
    debugger().save_properties();

    if (!silent)
    {
        report_breakpoints_inserted(msg, symbols.size());
    }
}



////////////////////////////////////////////////////////////////
void
MainWindow::on_delete_breakpoints(const vector<BreakPointSite>& locs)
{
    CALL_MAIN_THREAD_(command(&MainWindow::delete_breakpoints, this, locs));
}


////////////////////////////////////////////////////////////////
void MainWindow::delete_breakpoints(vector<BreakPointSite> locs)
{
    assert(pthread_self() == maintid);

    vector<BreakPointSite>::const_iterator i(locs.begin());
    for (; i != locs.end(); ++i)
    {
        if (debugger().remove_user_breakpoint(0, i->tid_, i->addr_))
        {
            dbgout(0) << __func__ << ": " << hex << (*i).addr_;
            dbgout(0) << " tid="  << dec  << (*i).tid_ << endl;
        }
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_view_type, (ViewType type))
{
    if (!progView_.get() || !current_thread())
    {
        return;
    }
    const ViewType currentViewType = progView_->view_type();

    if (currentViewType != type)
    {
        RefPtr<Symbol> sym = progView_->symbol();
        progView_->set_view_type(type);

        if (RefPtr<Thread> thread = current_thread())
        {
            progView_->show_function(sym, thread);
        }
        update_status_bar();
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::set_option,(uint64_t mask, bool option, bool force))
{
    dbgout(0) << __func__ << "(" << hex << mask << dec 
              << ", " << option << ") " << force << endl;

    if (force || is_at_debug_event())
    {
        if (is_ui_thread())
        {
            CALL_MAIN_THREAD_(
                command(&MainWindow::set_option,
                        this,
                        mask,
                        option,
                        true));
        }
        else
        {
            uint64_t opts = debugger().options();

            if (option)
            {
                opts |= mask;
            }
            else
            {
                opts &= ~mask;
            }
            debugger().set_options(opts);
        }
   }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::set_option_mask,(uint64_t opts, bool force))
{
    dbgout(0) << __func__ << "(" << hex << opts << dec << ") " << force << endl;
    if (force || is_at_debug_event())
    {
        if (is_ui_thread())
        {
            CALL_MAIN_THREAD_(
                command(&MainWindow::set_option_mask,
                        this,
                        opts,
                        true));
        }
        else
        {
            debugger().set_options(opts);
        }
   }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::do_attach,(pid_t pid, const string& param))
{
    ostringstream msg;
    msg << "Attaching to process " << pid << ", please wait...";

    statText_->set_text(msg.str());
    targetParam_ = param;

    CALL_MAIN_THREAD_(command(&Debugger::attach,
                              &debugger(),
                              pid,
                              targetParam_.c_str()));
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_attach,())
{
    SelectPidDialog dlg(debugger(), mainpid, "Attach to process");
    dlg.set_transient_for(*this);

    if (pid_t pid = dlg.run())
    {
        do_attach(pid, dlg.target_param());
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_detach,())
{
    string msg("Detach from program ");

    if (Thread* tp = debugger().get_thread(DEFAULT_THREAD))
    {
        msg += tp->filename();
        if (tp->process()->origin() == ORIGIN_DEBUGGER)
        {
            msg += ", and kill it";
        }
    }
    msg += "?";

    MessageBox dlg(msg, MessageBox::btn_yes_no, 0, question_xpm);
    dlg.set_transient_for(*this);

    if (dlg.run() == DialogBox::btn_yes)
    {
        CALL_MAIN_THREAD_(resume_cmd(&Debugger::detach, &debugger()));
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_close,())
{
    CHKPTR(progView_)->close_current_tab();
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_close_all,())
{
    CHKPTR(progView_)->clear();
}
END_SLOT()


namespace
{
    /**
     * Instructs the debugger to start a program
     */
    class ZDK_LOCAL Exec : public RefCountedImpl<InterThreadCommand>
    {
    public:
        Exec
        (
            Debugger& dbg,
            string program,
            bool expandArgs = false,
            const char* const* env = NULL
        )
            : debugger_(dbg)
            , program_(program)
            , expandArgs_(expandArgs)
            , env_(env)
        {
            assert_ui_thread();
            if (!env)
            {
                // inherit the parent's process (i.e. the debugger's)
                // environmental variables
                env_ = SArray(dbg.environment());
            }
        }

        ~Exec() throw() { }

        bool execute()
        {
            assert(pthread_self() == maintid);
            debugger_.exec(program_.c_str(), expandArgs_, env_.cstrings());

            return true;
        }

        const char* name() const
        {
            return program_.c_str();
        }

    private:
        Debugger&   debugger_;
        string      program_;
        bool        expandArgs_;
        SArray      env_;
    };


    /**
     * Run a debugger command in the debugger's main thread.
     */
    class ZDK_LOCAL DebuggerShellCommand : public RefCountedImpl<InterThreadCommand>
    {
    public:
        DebuggerShellCommand
        (
            Debugger& debugger,
            const RefPtr<Thread>& thread,
            const string& cmd,
            int fd
        )
            : debugger_(debugger)
            , thread_(const_cast<RefPtr<Thread>&>(thread))
            , cmd_(cmd)
            , fd_(fd)
        {}

        ~DebuggerShellCommand() throw() {}

        bool execute()
        {
            Redirect redir_stdout(STDOUT_FILENO, fd_);

            assert(pthread_self() == maintid);
            bool res = debugger_.command(cmd_.c_str(), thread_.get());
            cout << flush;

            return res;
        }

        const char* name() const { return cmd_.c_str(); }

    private:
        Debugger&       debugger_;
        RefPtr<Thread>  thread_;
        string          cmd_;
        int             fd_;
    };

} // namespace


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_run,(const string& program))
{
    set_cursor(*this, Gdk_FLAG(WATCH));
    CALL_MAIN_THREAD_(CommandPtr(new Exec(debugger(), program)));
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_execute,(const HistoryEntry* hist))
{
    string cmd;
 /*
    if (const char* target = CHKPTR(hist)->target_param())
    {
        cmd = target;
    }
  */
    if (const char* s = CHKPTR(hist)->command_line())
    {
        cmd += s;
    }
    else
    {
        cmd += hist->name();
    }
    set_cursor(*this, Gdk_FLAG(WATCH));
    if (hist->is_live())
    {
        CALL_MAIN_THREAD_(new Exec(debugger(), cmd, false, hist->environ()));
    }
    else
    {
        post_debugger_command("loadcore " + cmd);
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_run,())
{
    static const char title[] = "Execute";

    RunDialog box(
        debugger(),
        "Filename of program to run under the debugger \n"
        "with (optional) command line parameters:",
        title);

    box.set_transient_for(*this);

    const string prog = box.run();
    if (!prog.empty())
    {
        set_cursor(*this, Gdk_FLAG(WATCH));
        const bool expand = box.expand_args_in_shell();
        CALL_MAIN_THREAD_(new Exec(debugger(), prog, expand));
    }
}
END_SLOT()



////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_source,())
{
    if (progView_.get())
    {
        FileSel fsel("Open Source File", get_icon());
        close_on_cancel(fsel);
        Gtk_CONNECT_SLOT(&fsel, destroy, Gtk::Main::quit);
        Gtk_CONNECT_1(fsel.get_ok_button(), clicked,
            this, &MainWindow::on_source_selected, &fsel);

        if (!sourcesPath_.empty())
        {
            fsel.set_filename(sourcesPath_);
        }
        fsel.hide_fileop_buttons();
        fsel.show();
        fsel.set_modal(true);
        fsel.set_transient_for(*this);

        Gtk::Main::run();
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
//
// File-->Open-->Source Code
//
BEGIN_SLOT(MainWindow::on_source_selected, (Gtk::FileSelection* sel))
{
    string filename = sel->get_filename();
    Gtk_POPDOWN(sel);

    progView_->set_current(current_thread(), filename);
    sourcesPath_ = filename;
}
END_SLOT()



////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_load_core,())
{
    FileSel fsel("Open Core File", get_icon());
    fsel.set_transient_for(*this);

    close_on_cancel(fsel);

    Gtk_CONNECT_SLOT(&fsel, destroy, Gtk::Main::quit);
    Gtk_CONNECT_1(fsel.get_ok_button(), clicked,
        this, &MainWindow::on_core_selected, &fsel);

    fsel.hide_fileop_buttons();
    fsel.complete("core*");
    fsel.show();
    fsel.set_modal(true);

    Gtk::Main::run();
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_core_selected, (Gtk::FileSelection* sel))
{
    string corefile = sel->get_filename();
    Gtk_POPDOWN(sel);

    // load the core file on the main debugger thread
    statText_->set_text(string("Loading ") + corefile);
    post_debugger_command("loadcore " + corefile);
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::save_config()
{
    assert_ui_thread();
    save_geometry();

    Properties& props = *CHKPTR(debugger().properties());
    save_toolbars_visibility(props);

    if (localVarView_)
    {
        localVarView_->save_config(props, "locals");
    }
    if (watches_)
    {
        watches_->save_config();
    }
    
}


////////////////////////////////////////////////////////////////
void MainWindow::save_geometry()
{
    assert_ui_thread();
    int x = 0, y = 0;

    Gtk_WINDOW(this)->get_root_origin(x, y);

    Properties& props = *CHKPTR(debugger().properties());

    props.set_word(WINDOW_MAXIMIZED, maximized_);

    if (!maximized_)
    {
        props.set_word(WINDOW_X, x);
        props.set_word(WINDOW_Y, y);
        props.set_word(WINDOW_WIDTH,  Gtk_WIDTH(this));
        props.set_word(WINDOW_HEIGHT, Gtk_HEIGHT(this));

        if (layoutStrategy_)
        {
            layoutStrategy_->save_geometry();
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::on_menu_escape_to_command_line()
{
    ownsUserInteraction_ = false;
    message_box("Yielding control to command prompt... the graphical\n"
                "user interface will resume upon the next debug event",
                info2_xpm);

    CALL_MAIN_THREAD();
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_quit,())
{
    assert_ui_thread();

    bool doQuit = false;
    question_message("Are you sure that you want to quit?",
                     &doQuit, "confirm_quit");

    if (doQuit)
    {
        save_config();
        set_shutdown(true);

        // test if main thread is still running --
        // if not, then hmm... something bad must've happened,
        // exit right away
        if (pthread_kill(maintid, 0) < 0)
        {
            Gtk::Main::quit();
            _exit(errno);
        }
        // -- GUI::shutdown() takes care of this:
        // Gtk::Main::quit();

        if (!is_at_debug_event() && debugger().enum_processes())
        {
            // cannot simply post a quit command here:
            // the main thread may be blocked in a waitpid()
            // call and it is not listening to the responses
            // queue; need to stop debugged program first

            debugger().stop();

            // The call above is asynchronous; when a debug
            // event is reported, is_shutting_down() will return
            // true, and we'll proceed then with quitting the
            // debugger (see on_debug_event)
        }
        else
        {
            if (!waiting_)
            {
                if (Thread* t = debugger().get_thread(DEFAULT_THREAD))
                {
                    if (t->is_live())
                    {
                    #ifdef DEBUG
                        clog << __func__ << ": debuggee not responding.\n";
                    #endif
                        pthread_kill(maintid, SIGKILL);
                        Gtk::Main::quit();
                        _exit(0);
                    }
                }
            }
            CALL_MAIN_THREAD_(command(&Debugger::quit, &debugger()));
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_stop,())
{
    assert_ui_thread();
    // disable all buttons until thread stops
    update_state(uisThreadStop | uisAttachedLive);

    statText_->set_text("Stopping program...");

    set_cursor(*this, Gdk_FLAG(WATCH));
    debugger().stop(); // todo: thread-safety
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_set_reg,
(
    RefPtr<Register> reg,
    string v,
    string field
))
{
    if (is_ui_thread())
    {
        // marshal this call to the main thread
        CALL_MAIN_THREAD_(command(&MainWindow::on_set_reg, this, reg, v, field));
    }
    else
    {
        reg->set_value(v.c_str(), field.c_str());
    }
}
END_SLOT()



////////////////////////////////////////////////////////////////
void MainWindow::on_describe_type(RefPtr<DebugSymbol> sym, int fd)
{
    assert(!sym.is_null());
    assert(sym->type());

    run_on_main_thread(command(&DataType::describe, sym->type(), fd));
}


////////////////////////////////////////////////////////////////
/* void MainWindow::on_what_is(RefPtr<DataType> type)
{
    assert_ui_thread();
    Pipe pipe;

    OutputDialog output("Type Info", pipe.output());
    output.set_transient_for(*this);

    DataType* dt = type.get();

    run_on_main_thread(command(&DataType::describe, dt, pipe.input()));

    output.run(this);
}
*/

////////////////////////////////////////////////////////////////
bool MainWindow::on_variable_edit
(
    RefPtr<DebugSymbol> sym,
    const string&       val
)
{
    // the function is called on the UI thread, but the
    // actual work happens on the main debugger thread

    assert(!sym.is_null());
    assert(sym->type());

    if (is_ui_thread())
    {
        // marshal the call to the main thread
        CALL_MAIN_THREAD_(command(boost::bind(
            &MainWindow::on_variable_edit, this, sym, val)));
    }
    else
    {
        // main thread -- here's where the actual work is
        sym->write(val.c_str());
    }
    return false;
}


////////////////////////////////////////////////////////////////
void MainWindow::post_debugger_command(const string& cmd, int fd)
{
    // post a command to the responses queue, to be
    // executed by the debugger on the main thread.
    CALL_MAIN_THREAD_(CommandPtr(
        new DebuggerShellCommand(debugger(),
                                 current_thread(),
                                 cmd, fd)));
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_continue,())
{
    set_cursor(*this, Gdk_FLAG(WATCH));

    CALL_MAIN_THREAD_(new ResumeCommand());
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_restart,())
{
    post_debugger_command("restart");
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_return,())
{
    post_debugger_command("ret");
}
END_SLOT()


///////////////////////////////////////////////////////////////
void MainWindow::on_menu_next()
{
    next(Debugger::STEP_OVER_SOURCE_LINE);
}


////////////////////////////////////////////////////////////////
void MainWindow::on_menu_step()
{
    next(Debugger::STEP_SOURCE_LINE);
}


////////////////////////////////////////////////////////////////
static bool check_current_frame(
    const Frame&        frame,
    const StackView&    stackView,
    MainWindow&         w)
{
    bool proceed = true;

    RefPtr<Symbol> currentSymbolInView;

    if (w.program_view())
    {
        currentSymbolInView = w.program_view()->symbol();
    }

    bool sameFrame = true;
    bool stackTop  = true;

    if (&frame != stackView.top())
    {
        sameFrame = stackTop = false;
    }
    else if (
        currentSymbolInView && 
        currentSymbolInView->addr() != frame.program_count())
    {
        sameFrame = false;
    }

    while (!sameFrame)
    {
        // this condition may occur if we exec a stripped
        // program -- so if the user is switching to an
        // exec-ed thread, skip this warning

        if (RefPtr<Thread> t = w.current_thread())
        {
            if (stackTop && t->is_execed())
            {
                break; 
            }
        }

        static const string msg =
            "You have navigated away from the current stack frame."
            "\nDo you want to continue?";

        w.question_message(msg, &proceed, "confirm_next");
        break;
    }

    return proceed;
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_instruction,())
{
    if (current_thread())
    {
        addr_t from = 0;

        if (const Frame* frame = stackView_->selected_frame())
        {
            if (!check_current_frame(*frame, *stackView_, *this))
            {
                return;
            }
            from = frame->program_count();
        }
        CALL_MAIN_THREAD_(resume_cmd(&MainWindow::step_to,
            this, Debugger::STEP_INSTRUCTION, from, (addr_t)0));
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::next, (Debugger::StepMode mode))
{
    assert_ui_thread();

    if (!current_thread())
    {
        return;
    }
    addr_t from = 0;
    if (const Frame* frame = stackView_->selected_frame())
    {
        if (!check_current_frame(*frame, *stackView_, *this))
        {
            return;
        }
        from = frame->program_count();
    }

    addr_t toAddr = 0; // destination

    // In disassembly views do not step over entire ranges
    // that correspond to a line of source-code; just step
    // over CALL instructions.

    if (progView_->have_disassembly())
    {
        toAddr = progView_->next_addr();

        dbgout(0) << __func__ << ": " << (void*)toAddr << endl;

        if (toAddr)
        {
            if (mode == Debugger::STEP_OVER_SOURCE_LINE)
            {
                mode = Debugger::STEP_OVER_INSTRUCTION;
            }
            else
            {
                mode = Debugger::STEP_INSTRUCTION;
            }
        }
    }
    CommandPtr cmd =
        resume_cmd(&MainWindow::step_to, this, mode, from, toAddr);
    CALL_MAIN_THREAD_(cmd);
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::step_to(Debugger::StepMode mode,
                         addr_t from,
                         addr_t to)
{
    assert(pthread_self() == maintid);
    
    if (current_thread())
    {
        debugger().step(current_thread().get(), mode, from, to);
    }
}


////////////////////////////////////////////////////////////////
// todo: document
bool MainWindow::check_thread(RefPtr<Thread> thread) volatile
{
    if (thread.is_null())
    {
        return false;
    }

    ostringstream msg;
    bool result = true;

    if (thread_finished(*thread))
    {
        result = false;
        msg << "Thread " << thread->lwpid();

        if (pthread_t ptid = thread->thread_id())
        {
            msg << " (pthread id " << ptid << ")";
        }
        if (thread_exited(*thread))
        {
            msg << " has exited, exitcode=";
            msg << thread_exit_code(*thread);
        }
        else
        {
            msg << " was killed by signal ";
            msg << thread->signal();
        }
    }

    return result;
}


////////////////////////////////////////////////////////////////
void MainWindow::message_box_signal()
{
    assert_ui_thread();

    if (RefPtr<Thread> thread = current_thread())
    {
        if (!thread_stopped(*thread))
        {
            return;
        }
        const int signum = thread->signal();
        if (signum == 0)
        {
            dbgout(0) << thread->lwpid() << ": signal=0" << endl;
        }
        else if ((signum != SIGTRAP)
                 && debugger().signal_policy(signum)->stop())
        {
            if (signum == SIGSTOP)
            {
                const pid_t pid = thread->get_signal_sender();
                if ((pid == 0) || (pid == mainpid))
                {
                    return; // signal was sent by debugger
                }
            }
            ostringstream msg;

            const pid_t lwpid = thread->lwpid();
            msg << "Thread " << lwpid << " received signal " << signum;

            thread_set_event_description(*thread, strsignal(signum));

            if (thread->is_live() && !thread->get_user_object(".stop"))
            {
                msg << ": " << strsignal(signum);
                error_message(msg.str());
            }
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::set_active_thread(RefPtr<Thread> thread)
{
    assert(pthread_self() == maintid);

    const bool threadDied = !check_thread(thread);

    if (threadDied)
    {
        // The thread reporting an event is going away; the
        // ProgramView widget will use the old thread for display
        // purposes; prime the stack trace while we're still
        // in the main debugger thread
        RefPtr<Thread> codeThread = progView_->thread();

        if (!codeThread.is_null() &&
            !thread_finished(*codeThread) &&
            thread_is_attached(*codeThread))
        {
            codeThread->stack_trace();
        }
        if (thread)
        {
            thread = debugger().get_thread(thread->ppid());
        }
        if (thread.is_null())
        {
            thread = debugger().get_thread(DEFAULT_THREAD);
        }
    }
    if (thread != current_thread() &&
        (thread.is_null() || thread_is_attached(*thread)))
    {
        set_current_thread(thread);
        stackView_->update(thread);

        if (!thread)
        {
            dbgout(0) << __func__ << ": NULL thread" << endl;
        }
    }
}



////////////////////////////////////////////////////////////////
void MainWindow::update_after_responses()
{
    update_eval_view();

    //
    // some state changes might need to be reflected in the UI
    //
    POST_COMMAND(&MainWindow::compute_and_update_state,
                this,
                RefPtr<Thread>(),
                0U);
}


////////////////////////////////////////////////////////////////
void MainWindow::update_stack_view()
{
    CHKPTR(stackView_)->update(current_thread());
}


////////////////////////////////////////////////////////////////
void MainWindow::wait_and_process_responses() volatile
{
    assert(pthread_self() == maintid);
    dbgout(1) << __func__ << endl;

    Temporary<bool> set_flag(const_cast<bool&>(waiting_), true);

    if (process_responses())
    {
        debugger().resume(true);
    }
}

class AutoFlag : boost::noncopyable
{
    volatile atomic_t& flag_;

public:
    explicit AutoFlag( volatile atomic_t& flag ) : flag_(flag)
    {
        atomic_inc(flag_);
    }
    ~AutoFlag()
    {
        atomic_dec(flag_);
    }
};

////////////////////////////////////////////////////////////////
bool MainWindow::on_debug_event(

    RefPtr<Thread> thread,
    EventType      eventType)volatile
{
    assert(pthread_self() == maintid);

    dbgout(1) << __func__ << ": eventType=" << eventType << endl;

    if ((eventType != E_PROMPT) || is_service_call_pending())
    {
        ownsUserInteraction_ = true;
    }

    if (is_shutting_down())
    {
        debugger().quit();
    }
    else if (ownsUserInteraction_)
    {
        // True up breakpoint count, on_insert_breakpoint / on_remove_breakpoint
        // are not very accurate;
        // the culprit is the engine's breakpoint manager: when a deferred bpnt
        // is turned into a "real breakpoint" (i.e. it gets "activated") two callbacks
        // should fire: one for the deferred breakpoint being removed, and another
        // one for the "real" breakpoint being inserted; sometimes the second
        // callback is not sent, and thus the UI may think that we have one breakpoint
        // less... hence this hack here.
        //
        // @todo revisit breakpoint manager code
        //
        if (BreakPointManager* mgr = interface_cast<BreakPointManager*>(&debugger()))
        {
            EnumBreakPoints observ;
            mgr->enum_breakpoints(&observ);

            breakpointCount_ = observ.count();
        }

        AutoFlag autoFlag(atDebugEvent_);

        while (!process_debug_event(thread, eventType))
        {
            if (!is_response_queue_empty())
            {
                process_responses();
            }
        }

        if (!debugger().is_resumed())
        {
            // At this point, the main thread blocks until the
            // UI thread posts to the responses queue.
            wait_and_process_responses();
        }
    }

    return ownsUserInteraction_;
}


////////////////////////////////////////////////////////////////
void MainWindow::update_var_views()
{
    assert_main_thread();
    update_eval_view();

    RefPtr<Thread> thread = current_thread();

    if (layoutStrategy_)
    {
        if (layoutStrategy_->is_variables_view_visible())
        {
            localVarView_->update(thread);
        }
    }

    if (watches_)
    {
        if (!watches_->update(thread))
        {
            debugger().resume();
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::update_thread_view(RefPtr<Thread> thread)
{
    assert_main_thread();
    if (threadView_)
    {
        threadView_->update(*thread);
    }
}


////////////////////////////////////////////////////////////////
bool MainWindow::process_debug_event(

    const RefPtr<Thread>& thread,
    EventType             eventType ) volatile
{
    assert_main_thread();

    string error;

    try
    {
        Lock<Mutex> lock(mutex(), TryLock());

        if (!lock)
        {
            // the UI owns the mutex -- and is perhaps
            // waiting for the main thread to handle a
            // response
            return false;
        }
        
        // Update the data for the various views, but do not
        // display anything just yet, since the main thread cannot
        // call Gtk functions directly; after the data is updated,
        // we will marshal and update_display object to the UI.

        MainWindow* THIS = const_cast<MainWindow*>(this);

        set_debuggee_running(false);
        THIS->set_active_thread(thread);

        assert(THIS->localVarView_.get());
        assert(THIS->registerView_.get());

        if (RefPtr<Thread> thread = current_thread())
        {
            if (Process* proc = thread->process())
            {
                proc->vdso_symbol_tables();
            }
        }
        RefPtr<Thread> currentThread = current_thread();
        THIS->update(currentThread);
    }
    catch (const exception& e)
    {
        error = e.what();
    }

    if (!error.empty())
    {
        cerr << __func__ << ": " << error << endl;
        POST_COMMAND(&MainWindow::error_message, this, error);
    }

    if (!is_service_call_pending())
    {
        //
        // wrap the update_display() call into a function object and
        // marshal it to the UI thread via the requests queue
        //
        POST_COMMAND(&MainWindow::update_display, this, eventType);
    }
    return true;
}


////////////////////////////////////////////////////////////////
void MainWindow::update(RefPtr<Thread> currentThread)
{
    stackView_->update(currentThread);
    registerView_->update(currentThread);
    if (currentThread)
    {
        update_thread_view(currentThread);
    }

    // update memory views
    MemoryViews::iterator k = memoryViews_.begin();
    for (; k != memoryViews_.end(); ++k)
    {
        k->second->update();
    }
    update_var_views();
}


////////////////////////////////////////////////////////////////
void MainWindow::on_attached(RefPtr<Thread> thread)
{
    class ThreadViewHelper : public EnumCallback<Thread*>
    {
        ThreadView& view_;

    public:
        explicit ThreadViewHelper(ThreadView& view) : view_(view)
        {}
        void notify(Thread* thread)
        {
            if (thread) view_.add_thread(*thread);
        }
    }; // ThreadViewHelper

    assert_ui_thread();
    assert(thread);

    compute_and_update_state(thread);

    if (!CHKPTR(threadView_)->add_thread(*thread))
    {
        threadView_->clear();

        ThreadViewHelper helper(*threadView_);
        debugger().enum_threads(&helper);

        // automatically show the thrads view when
        // new threads are created -- maybe this
        // behavior should be controlled by an option?
        layoutStrategy_->show_threads_view();
    }

    if (!current_thread())
    {
        set_current_thread(thread);
    }
    string title ("zero: ");
    thread = current_thread();

    if (thread)
    {
        title += thread->filename();

        if (SharedString* event = thread_get_event_description(*thread))
        {
            statText_->set_text(event->c_str());
        }
        else
        {
            statText_->set_text("");
        }
        CHKPTR(progView_)->restore_open_tabs(*thread);
        if (watches_)
        {
            watches_->restore(*thread);
        }
    }
    set_title(title);
}


////////////////////////////////////////////////////////////////
void MainWindow::on_detached(RefPtr<Thread> thread)
{
    assert_ui_thread();
    Lock<Debugger> lock(debugger());
    progressBox_->reset();

    if (thread)
    {
        CHKPTR(threadView_)->remove(thread->lwpid());

        MemoryViews tmp;

        tmp.swap(memoryViews_);
        tmp.erase(thread->lwpid());
        tmp.swap(memoryViews_);

        dbgout(0) << __func__ << " tid=" << thread->lwpid() << endl;

        if (current_thread() == thread)
        {
            reset_current_thread();

            CHKPTR(progView_)->clear();
            CHKPTR(registerView_)->clear();
        }
    }
    else
    {
        // receiving this notification with a null thread
        // means that we have disconnected from all threads
        if (RefPtr<Thread> current = current_thread())
        {
            CHKPTR(progView_)->save_open_tabs(*current);
        }
        else
        {
            dbgout(0) << __func__ << ": no current thread" << endl;
        }
        statTop_->set_text("No thread");
        set_title("zero: No Program");

        dbgout(0) <<  __func__ << ": resetting breakpoint count" << endl;
        // todo:
        // revisit when per-thread breakpoints are supported in the UI
        //
        breakpointCount_ = 0;

        clear_all_views();
        reset_current_thread();

        set_debuggee_running(false);
        compute_and_update_state(NULL, 0);
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::on_thread_finished(RefPtr<Thread> t, string msg)
{
    assert(!is_debuggee_running());

    compute_and_update_state(t);
    statText_->set_text(msg);

    if (t->is_live())
    {
        error_message(msg);
    }
}


////////////////////////////////////////////////////////////////
static void
print_progress(ostream& out, const char* what, double percent)
{
    out << "\r\033[K"; // ANSI: clear to end-of-line
    out << what << " " << (int)(percent * 100) << "%";
    if (percent >= 1.)
    {
        out << endl;
    }
    out << flush;
}


////////////////////////////////////////////////////////////////
bool
MainWindow::on_progress(const char* what, double perc) volatile
{
    bool const useConsole = is_ui_thread();

    if (useConsole)
    {
        print_progress(cout, what, perc);
        return true;
    }
    assert_main_thread();
    assert(what);

    MainWindow* THIS = const_cast<MainWindow*>(this);
    assert(THIS->progressBox_.get());
    if (THIS->progressBox_->cancelled())
    {
        dbgout(0) << __func__ << ": cancelled" << endl;

        return false;
    }
    if (THIS->progressBox_->done() && perc < 1.)
    {
        THIS->progressBox_->reset();
    }

    if (!write_percent(what, perc))
    {
        print_progress(cout, what, perc);
    }
    return true;
}


////////////////////////////////////////////////////////////////
void MainWindow::show_progress_indicator(string what, double percent)
{
    progressBox_->update(*this, what, percent);
}


////////////////////////////////////////////////////////////////
void MainWindow::update_display(EventType event)
{
    if (is_shutting_down() || !is_at_debug_event())
    {
        return;
    }
    assert_ui_thread();

    // change cursor shape in the scope of this func
    SetCursorInScope csr(*this, Gdk_FLAG(WATCH));
    update_eval_view(true);

    CHKPTR(registerView_)->display();
    CHKPTR(localVarView_)->display();
    CHKPTR(watches_)->display();
    if (threadView_)
    {
        threadView_->display();
    }
    MemoryViews::const_iterator k = memoryViews_.begin();
    for (; k != memoryViews_.end(); ++k)
    {
        k->second->display();
    }
    RefPtr<Thread> thread = current_thread();
    if (thread)
    {
        if (thread->stack_trace_depth())
        {
            CHKPTR(stackView_)->display();
        }
    }
    display_code_and_status(event);

    refresh_state();

    if (thread && !thread_finished(*thread))
    {
        threadView_->select(thread->lwpid());
        update_status_bar();
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::display_code_and_status(EventType eventType)
{
    assert_ui_thread();

    RefPtr<Thread> thread = current_thread();

    // if the thread has passed to the Great Beyond, we might
    // not be able to get its stack and CPU registers.
    if (!thread || thread_finished(*thread))
    {
        statTop_->set_text("No Symbol");
        statText_->set_text("No Program");
        return;
    }
    assert(!thread_finished(*thread));

    if (!thread->stack_trace_depth())
    {
        return;
    }

    if (Frame* stackFrame = thread_current_frame(thread.get()))
    {
        RefPtr<Symbol> sym = stackFrame->function();
        if (sym.is_null())
        {
            statTop_->set_text("No Symbol");
        }
        else
        {
            progView_->set_current(sym,
                                   thread,
                                   stackFrame,
                                   true,
                                   VIEW_DEFAULT);
            // automatically switch to the mixed (source + assembly)
            // view if thread is in single-step mode
            if (thread->single_step_mode())
            {
                progView_->force_mixed_view();
            }
        }
        const bool isInternalEvent = (eventType == E_PROMPT)
                                  || (eventType == E_EVAL_COMPLETE);
        if (!isInternalEvent)
        {
            message_box_signal();
        }
    }
    else
    {
        dbgout(0) << __func__ << ": No stack frame" << endl;
    }
}



////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_symbol_change,
(
    RefPtr<Thread> thread,
    RefPtr<Symbol> symbol,
    Frame* frame
))
{
    compute_and_update_state(thread, 0);
    show_symbol(thread, symbol, frame);
}
END_SLOT()


////////////////////////////////////////////////////////////////
void
MainWindow::show_symbol(
    RefPtr<Thread>  thread,
    RefPtr<Symbol>  symbol,
    Frame*          frame)
{
    if (!thread || !symbol || is_shutting_down())
    {
        return;
    }
    assert_ui_thread();

    bool threadChanged = false;

    if (check_thread(thread))
    {
        if (current_thread() != thread)
        {
            threadChanged = true;
            set_current_thread(thread);
        }
    }
    thread = current_thread();
    if (!thread)
    {
        return;
    }
    const pid_t pid = thread->lwpid();

    if (threadChanged)
    {
        debugger().set_current_thread(thread.get());
        CHKPTR(threadView_)->select(pid);

        update_stack_on_main_thread();

        stackView_->display();
    }

    assert(symbol && symbol->file() && symbol->file()->c_str());

    string fileName = CHKPTR(symbol->file())->c_str();
    const size_t len = fileName.size();

    if (len > 80)
    {
        // keep file name down to some sane length
        fileName = "..." + fileName.substr(len - 80);
    }
    ostringstream info;

    info << '[' << pid << "] " << fileName;
    string func = symbol->demangled_name(false)->c_str();

    // keep it down to some sane length...
    if (func.length() > 80)
    {
        func.resize(80);
        func += " ...";
    }
    if (size_t line = symbol->line())
    {
        info << ':' << line;
    }
    else
    {
        info << " in "<< func;
        info << "+0x" << hex << symbol->offset();
    }
    statTop_->set_text(info.str());

    ostringstream ss;
    ss << '[' << pid << "]: " << func;
    funcBar_->set_text(ss.str());

    // sync menu with current view
    set_view_mode(progView_->view_type());

    show_frame(frame);
}



////////////////////////////////////////////////////////////////
void MainWindow::show_frame(Frame* frame)
{
    if (CHKPTR(stackView_)->show_frame(frame))
    {
        assert(frame);

        CHKPTR(localVarView_)->restore_state(frame);
        CHKPTR(localVarView_)->display();
    }
    else if (frame)
    {
        status_message("FRAME NOT ACTIVE");

        CHKPTR(localVarView_)->reset();
        CHKPTR(localVarView_)->display();
    }
}


/**
 * @note triggered by on_program_resumed()
 */
void MainWindow::update_running()
{
    assert_ui_thread();
    Lock<Mutex> lock(mutex(), TryLock());

    if (lock)
    {
        compute_and_update_state(NULL, 0);

        if (size_t line = progView_->current_line())
        {
            if (progView_->is_line_visible(line)
             && progView_->query_breakpoint_at_line(line))
            {
                progView_->hilite_line(line, false);
            }
        }
    }
    else
    {
        assert(false);
    }
}


////////////////////////////////////////////////////////////////
static unsigned check_can_restart(const RefPtr<Properties>& prop)
{
    unsigned result = uisNone;

    if (prop)
    {
        if (RefPtr<History> hist =
            interface_cast<History*>(prop->get_object("hist")))
        {
            if (hist->entry_count())
            {
                if (hist->entry(0)->origin() == ORIGIN_DEBUGGER)
                {
                    result |= uisCommandLine;
                }
            }
        }
    }
    return result;
}


////////////////////////////////////////////////////////////////
static unsigned check_live_state(
    Debugger&       debugger,
    RefPtr<Thread>  thread)
{
    unsigned result = uisNone;

    if (!thread)
    {
        thread = debugger.current_thread();
    }
   
    if (thread)
    {
        if (thread->is_live())
        {
            result |= uisAttachedLive;

            if (RefPtr<Process> proc = thread->process())
            {
                if (proc->origin() == ORIGIN_DEBUGGER)
                {
                    result |= uisCommandLine;
                }
            }
        }
        else
        {
            result |= uisAttached; // loaded a core file
        }
    }

    return result;
}

////////////////////////////////////////////////////////////////
void MainWindow::compute_and_update_state
(
    RefPtr<Thread> thread,
    unsigned state
)
{
    assert_ui_thread();
    if (is_shutting_down())
    {
        return;
    }
    SetCursorInScope csr(*this, Gdk_FLAG(WATCH));
    //
    // allow users to check for updates once a day at most
    //
    RefPtr<Properties> properties = debugger().properties();
    time_t lastCheck = properties ? properties->get_word("upchk", 0) : 0;
    if (difftime(time(NULL), lastCheck) >= (24 * 60 * 60))
    {
        state |= uisCheckUpdates;
    }

    if (is_debuggee_running())
    {
        state |= uisThreadRun;
        state &= ~uisThreadStop;
    }
    else
    {
        state &= ~uisThreadRun;
        state |= uisThreadStop;
    }
    if (!thread)
    {
        if (current_thread() && thread_is_attached(*current_thread()))
        {
            thread = current_thread();
        }
    }
    if (is_at_debug_event())
    {
        if (!thread)
        {
            thread = debugger().get_thread(DEFAULT_THREAD);
        }
        if (thread)
        {
            if (breakpointCount_)
            {
                state |= uisBreakPoints;
            }
            if (BreakPointManager* mgr = debugger().breakpoint_manager())
            {
                if (mgr->enum_watchpoints())
                {
                    state |= uisWatchPoints;
                }
            }
        }
    }
    state |= check_can_restart(properties);
    state |= check_live_state(debugger(), thread);

    state &= ~(uisCodeViews | uisSymbolicView);

    if (progView_.get() && progView_->has_views())
    {
        state |= uisCodeViews;
        if (progView_->symbol())
        {
            state |= uisSymbolicView;
        }
    }
    update_state(state);
    update_eval_view(true);
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::update_state,(size_t state))
{
    // NOTE: we should already acquired have the mutex by now.
    if (is_debuggee_running())
    {
        statTop_->set_text("Running...");
        statText_->set_text("Running...");

        state |= uisThreadRun;
        state &= ~uisThreadStop;

        if (toolbar_)
        {
            toolbar_->on_can_backup(0);
            toolbar_->on_can_forward(0);
        }
    }
    else if (is_at_debug_event())
    {
        if (RefPtr<Symbol> sym = CHKPTR(progView_)->symbol())
        {
            Frame* frame = NULL;
            show_symbol(current_thread(), sym, frame);
        }
    }
    on_change_state(*this, state);
    foreach(Gtk_BIND(Gtk_PTR_FUN(&MainWindow::on_change_state), state));
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::update_status_bar()
{
    assert_ui_thread();

    assert(statText_);

    if (RefPtr<Thread> thread = current_thread())
    {
        if (SharedString* event = thread_get_event_description(*thread))
        {
            statText_->set_text(event->c_str());
        }
    }
}


////////////////////////////////////////////////////////////////
void
MainWindow::handle_error( const char* func,
                          const string& errMsg
                        ) volatile
{
    if (!errMsg.empty())
    {
        cerr << func << ": " << errMsg << endl;
        MainWindow* THIS = const_cast<MainWindow*>(this);

        THIS->error_message(errMsg);

        if (is_ui_thread())
        {
            THIS->refresh_state();
        }
    }
}



////////////////////////////////////////////////////////////////
void MainWindow::on_symbol_expand(DebugSymbol* /* symbol */)
{
    assert_ui_thread();

    set_cursor(*this, Gdk_FLAG(WATCH));
    CALL_MAIN_THREAD();
}



////////////////////////////////////////////////////////////////
void MainWindow::on_query_symbols
(
    string              name,
    addr_t              addr,
    DebugSymbolList*    syms,
    bool                readValues
)
{
    assert(syms);
    if (is_at_debug_event())
    {
        RefPtr<Thread> t = progView_ ? progView_->thread() : current_thread();
        AppSlots::on_query_symbols(t, name, addr, syms, readValues);
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT_(bool, MainWindow::on_evaluate,
(
    string         expr,
    addr_t         addr,
    ExprEvents*    events,
    int            base
))
{
    return AppSlots::on_evaluate(current_thread(), expr, addr, events, base);
}
END_SLOT_(false)


////////////////////////////////////////////////////////////////
void MainWindow::on_refresh(VariablesView* /* view */)
{
    assert_main_thread();

    POST_COMMAND(&MainWindow::update_display, this, E_EVAL_COMPLETE);
}


////////////////////////////////////////////////////////////////
void MainWindow::on_insert_breakpoint(RefPtr<Thread> thread,
                                      RefPtr<Symbol> symbol,
                                      bool isDeferred)
{
    assert_ui_thread();

    if ((!thread || (thread == current_thread())) && symbol)
    {
    #if 0
        // todo: cleanup [Thu Jan 31 22:32:15 PST 2008]
        if (progView_->on_insert_breakpoint(symbol))
        {
            ++breakpointCount_;
        }
    #else
        progView_->on_insert_breakpoint(symbol, isDeferred);
        ++breakpointCount_;
    #endif
    }

    dbgout(0) << __func__ << ", count=" << breakpointCount_ << endl;
}


////////////////////////////////////////////////////////////////
void MainWindow::on_remove_breakpoint(RefPtr<Thread> thread,
                                      RefPtr<Symbol> symbol,
                                      bool isDeferred)
{
    assert_ui_thread();
    dbgout(0) << __func__ << ": " << breakpointCount_ << endl;

    // todo: a better name for this variable would
    // be userBreakPoints or userBreakPointCount
    if (breakpointCount_)
    {
        --breakpointCount_;

        dbgout(0) << __func__ << ": " << breakpointCount_
                  << " deferred=" << isDeferred << endl;

        if ((!thread || (thread == current_thread())) && symbol)
        {
            progView_->on_remove_breakpoint(symbol, isDeferred);
        }
    }
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_heap, ())
{
    HeapView heapView(debugger(), *CHKPTR(progView_));

    heapView.set_transient_for(*this);
    heapView.run();
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::on_menu_toggle_toolbar(ToolBar* toolbar)
{
    if (toolbar)
    {
        if (toolbar->is_visible())
        {
            toolbar->hide_all();
        }
        else
        {
            toolbar->show_all();
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::help_search(string msg)
{
    if (is_ui_thread())
    {
        on_help(msg.c_str());
    }
    else
    {
        run_on_ui_thread(command(&MainWindow::help_search, this, msg));
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::on_menu_help()
{
    on_help();
}

////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_help,(const char* searchTopic))
{
    if (Gtk::Window* w = HelpViewer::active_dialog())
    {
        Gtk_WINDOW(w)->show();
        Gtk_WINDOW(w)->raise();
    }
    else
    {
        HelpViewer helpViewer(debugger().properties());

        if (searchTopic)
        {
            helpViewer.search(searchTopic);
        }
        helpViewer.run(this, false);
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_view_modules,())
{
    if (RefPtr<Thread> thread = current_thread())
    {
        ModulesView modulesView;

        if (RefPtr<Process> proc = thread->process())
        {
            modulesView.populate_using(*proc);
            modulesView.set_transient_for(*this);
            modulesView.run(this);
        }
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
void MainWindow::popup_memory_window(addr_t addr)
{
    if (RefPtr<Thread> thread = current_thread())
    {
        Properties* props = CHKPTR(debugger().properties());
        MemoryViewPtr view(new MemoryView(props, thread, addr));
        memoryViews_.insert(make_pair(thread->lwpid(), view));

        Gtk_CONNECT_1(view, destroy,
            this, &MainWindow::remove_mem_view, view.get());

        view->evaluate.connect(Gtk_SLOT(this, &MainWindow::on_evaluate));
        view->update_request.connect(Gtk_SLOT(this, &MainWindow::update_mem_view));
        view->show_all();
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::remove_mem_view(MemoryView* view)
{
    MemoryViews::iterator i = memoryViews_.begin();
    for (; i != memoryViews_.end(); ++i)
    {
        if (i->second.get() == view)
        {
            memoryViews_.erase(i);
            break;
        }
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::update_mem_view(RefPtr<MemoryView> memView)
{
    assert(memView.get());
    if (is_ui_thread())
    {
        CALL_MAIN_THREAD();
    }
    else // main thread
    {
        memView->update();
        POST_COMMAND(&MainWindow::update_display, this, E_EVAL_COMPLETE);
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::save_toolbars_visibility(Properties& props) const
{
    ToolMap::const_iterator i = toolMap_.begin();
    for (; i != toolMap_.end(); ++i)
    {
        string name = "toolbar." + i->first;

        props.set_word(name.c_str(), is_toolbar_visible(i->second));
    }
}


////////////////////////////////////////////////////////////////
bool MainWindow::is_toolbar_visible(ToolBar* toolbar)
{
    bool visible = false;

    if (toolbar)
    {
        visible = toolbar->is_visible();
    }
    return visible;
}


////////////////////////////////////////////////////////////////
void MainWindow::clear_all_views()
{
    assert_ui_thread();
    threadView_->clear();
    stackView_->items().clear();

    registerView_->clear();

    if (ProgramView* progView = progView_.get())
    {
        progView->clear();
    }
    localVarView_->reset();
    watches_->reset();

    MemoryViews tmp;
    tmp.swap(memoryViews_);
    funcBar_->set_text("");
}


////////////////////////////////////////////////////////////////
void MainWindow::set_view_mode(ViewType viewType)
{
    assert(progView_.get());
    assert(viewMode_);
    assert(viewType != VIEW_DEFAULT);

    assert_ui_thread();

    if (viewType != VIEW_DEFAULT)
    {
        if (progView_->view_type() != viewType)
        {
            progView_->set_view_type(viewType);
        }
        Temporary<ProgramViewPtr> set(progView_, ProgramViewPtr());
        Gtk_MENU_ITEM((*viewMode_)[viewType]).activate();
    }
}


////////////////////////////////////////////////////////////////
bool MainWindow::can_interact() // const
{
    return is_at_debug_event();
}


////////////////////////////////////////////////////////////////
void MainWindow::on_eval_error(std::string msg)
{
    if (!is_ui_thread())
    {
        post_command(&MainWindow::on_eval_error, this, msg);
    }
    else
    {
        update_eval_view(true);
        error_message(msg);
    }
}


////////////////////////////////////////////////////////////////
void MainWindow::update_eval_view(bool complete) volatile
{
    Lock<Mutex> lock(evalMutex_);

    if (evalView_)
    {
        if (complete)
        {
            evalView_->update_state_eval_complete(true);
        }
        else
        {
            assert_main_thread();
            evalView_->view()->update(current_thread().get());
        }
    }
}


#ifdef DEBUG_OBJECT_LEAKS
////////////////////////////////////////////////////////////////
void MainWindow::print_instance_counted()
{
    if (is_ui_thread())
    {
        set_service_call(true);
        CALL_MAIN_THREAD_(
            command(&MainWindow::print_instance_counted, this));
    }
    else
    {
        debugger().print_counted_objects(__PRETTY_FUNCTION__);
    }
}
#endif // DEBUG_OBJECT_LEAKS


////////////////////////////////////////////////////////////////
static void
apply_fonts(Gtk::FontSelectionDialog* fsd, ProgramView* view)
{
    view->apply_font(fsd->get_font_name());
}


////////////////////////////////////////////////////////////////
void MainWindow::on_menu_fonts()
{
    Gtk::FontSelectionDialog fsd;

    Gtk_CONNECT(fsd.get_apply_button(), clicked,
        Gtk_BIND(Gtk_PTR_FUN(apply_fonts), &fsd, progView_.get()));
    Gtk_CONNECT(fsd.get_ok_button(), clicked,
        Gtk_BIND(Gtk_PTR_FUN(apply_fonts), &fsd, progView_.get()));

#ifdef GTKMM_2
    Gtk_CONNECT_SLOT(fsd.get_cancel_button(), clicked, Gtk::Main::quit);
    Gtk_CONNECT_SLOT(fsd.get_ok_button(), clicked, Gtk::Main::quit);
#else
    Gtk_CONNECT_SLOT(fsd.get_cancel_button(), clicked, fsd.destroy);
    Gtk_CONNECT_SLOT(fsd.get_ok_button(), clicked, fsd.destroy);
    Gtk_CONNECT_SLOT(&fsd, destroy, Gtk::Main::quit);
#endif

    fsd.set_modal(true);
    fsd.set_transient_for(*this);
    fsd.set_font_name(progView_->font_name());
    fsd.show_all();

    Gtk::Main::run();
}

////////////////////////////////////////////////////////////////
void MainWindow::connect_codeview()
{
    assert(progView_.get());

    // connect the code view contextual menu
    progView_->set_breakpoints.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_set_breakpoint));

    progView_->delete_breakpoint.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_delete_breakpoint));

    progView_->disable_breakpoint.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_disable_breakpoint));
    progView_->enable_breakpoint.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_enable_breakpoint));

    progView_->can_interact.connect(
        Gtk_SLOT(this, &MainWindow::can_interact));

    progView_->set_program_count.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_set_program_count));
    progView_->show_next_line.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_show_next_line));
    progView_->evaluate.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_evaluate));

    progView_->query_symbols.connect(
        Gtk_SLOT(this, &MainWindow::on_query_symbols));

    // toolbar navigation
    toolbar_->tool_back.connect(
        Gtk_SLOT(progView_.get(), &ProgramView::history_back));
    toolbar_->tool_forward.connect(
        Gtk_SLOT(progView_.get(), &ProgramView::history_forward));

    progView_->history_can_back.connect(
        Gtk_SLOT(this, &MainWindow::on_can_navigate_back));
    progView_->history_can_forward.connect(
        Gtk_SLOT(this, &MainWindow::on_can_navigate_forward));

    // for showing tooltips when hovering over variables
    progView_->filter.connect(Gtk_SLOT(this, &AppSlots::on_apply_filter));

    // for the find dialog
    progView_->string_not_found.connect(Gtk_SLOT(this, &MainWindow::on_not_found));

    progView_->symbol_changed.connect(Gtk_SLOT(this, &MainWindow::on_symbol_change));
    progView_->status_message.connect(Gtk_SLOT(this, &MainWindow::status_message));
    progView_->run_on_main_thread.connect(
        Gtk_SLOT(this, &AppSlots::run_on_main_thread));

    // connect the "Step Over" slots on the right click menu
    progView_->step_over_func.connect(Gtk_SLOT(this, &AppSlots::step_over_func));
    progView_->step_over_file.connect(Gtk_SLOT(this, &AppSlots::step_over_file));
    progView_->step_over_dir.connect(Gtk_SLOT(this, &AppSlots::step_over_dir));
    progView_->step_over_reset.connect(Gtk_SLOT(this, &AppSlots::step_over_reset));
    progView_->step_over_manage.connect(
        Gtk_SLOT(this, &MainWindow::on_menu_manage_step_over));

    progView_->views_changed.connect(Gtk_SLOT(this, &MainWindow::refresh_state));
    progView_->accept_invalid_utf8.connect(Gtk_SLOT(this, &MainWindow::on_invalid_utf8));
}


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_show_next_line,(addr_t addr, bool inUnit))
{
    if (RefPtr<Thread> thread = current_thread())
    {

        Frame* frame = inUnit ? stackView_->selected_frame() : stackView_->top();
        if (frame)
        {
            if (Symbol* sym = frame->function())
            {
                progView_->set_current(sym, thread, frame, true);
                return;
            }
        #ifdef DEBUG
            addr_t pc = frame->program_count();
            clog << __func__ << " pc=" << hex << pc << dec << endl;
        #endif
        }
    }
}
END_SLOT()


/**
 * This slot is connected to a signal emitted by the
 * right-click menu over the code view
 */
BEGIN_SLOT(MainWindow::on_menu_set_breakpoint,
(
    RightClickInfo* rclick,
    bool permanent
))
{
    if (permanent)
    {
        set_service_call(true);
    }
    static const bool silent = true; // no confirmation message
    const bool deferred = !CHKPTR(rclick)->deferred_symbols().empty();

    if (!deferred && rclick->addrs().empty())
    {
        error_message("Sorry, no matching address was found for this line");
    }
    else if (permanent)
    {
        if (deferred)
        {
            CALL_MAIN_THREAD_(
                command(&MainWindow::insert_deferred_breakpoints,
                        this,
                        current_thread(),
                        rclick->deferred_symbols(),
                        silent,
                        true));
        }
        else
        {
            CALL_MAIN_THREAD_(
                command(&MainWindow::insert_breakpoints,
                        this,
                        current_thread(),
                        rclick->addrs(),
                        silent,
                        true));
        }
    }
    else
    {
        if (deferred)
        {
            error_message("Cannot set temporary breakpoint at this location: \n"
                "it resolves to a shared object that has not been loaded yet.");
        }
        else
        {
            CALL_MAIN_THREAD_(
                command(&MainWindow::insert_breakpoints_and_resume,
                        this,
                        current_thread(),
                        rclick->addrs(),
                        false));
        }

    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_disable_breakpoint,
(
    addr_t addr,
    size_t line
))
{
    // NOTE that it is okay for now to call this on the UI
    // thread, since it only affects breakpoint actions, and
    // it does not need to make ptrace calls
    disable_user_breakpoint_actions(debugger(), addr);

    update_breakpoint_view(addr, line);
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_enable_breakpoint,
(
    addr_t addr,
    size_t line
))
{
    // note that it is okay for now to call this on the UI
    // thread, since it only affects breakpoint actions, and
    // it does not need to make ptrace calls
    enable_user_breakpoint_actions(debugger(), addr);
    update_breakpoint_view(addr, line);
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_menu_save_stack,())
{
    FileSel fsel("Save Stack Trace", get_icon());
    fsel.set_transient_for(*this);

    close_on_cancel(fsel);

    Gtk_CONNECT_SLOT(&fsel, destroy, Gtk::Main::quit);
    Gtk_CONNECT_1(fsel.get_ok_button(), clicked,
                  this, &MainWindow::on_stack_file_selected, &fsel);

    // fsel.hide_fileop_buttons();
    fsel.complete("*.txt");
    fsel.show();
    fsel.set_modal(true);

    Gtk::Main::run();
}
END_SLOT()


////////////////////////////////////////////////////////////////
BEGIN_SLOT(MainWindow::on_stack_file_selected,(Gtk::FileSelection* fsel))
{
    if (!fsel)
    {
        return; // should never happen
    }
    string fname = fsel->get_filename();
    struct stat st;
    if (stat(fname.c_str(), &st) == 0)
    {
        if (S_ISREG(st.st_mode))
        {
            bool overwrite = false;
            question_message(fname + " exists. Overwrite?", &overwrite);
            if (!overwrite)
            {
                return;
            }
        }
        else
        {
            error_message(fname + " is not a regular file");
            return;
        }
    }
    ofstream fs(fname.c_str());
    if (!fs)
    {
        SystemError err("Could not open " + fname);
        error_message(err.what());
    }
    else
    {
        Gtk_POPDOWN(fsel);

        save_stack(&fs);
    }
}
END_SLOT()


////////////////////////////////////////////////////////////////
static bool
macro(DebuggerCommand* cmd, RefPtr<Thread> thread, SArray args)
{
    return cmd->execute(thread.get(), args.cstrings());
}


////////////////////////////////////////////////////////////////
void
MainWindow::run_macro(DebuggerCommand* cmd, const SArray& args)
{
    post_response(wrap(boost::bind(macro, cmd, current_thread(), args)));
}


////////////////////////////////////////////////////////////////
void MainWindow::enable_command(DebuggerCommand* cmd, bool enable)
{
    assert (cmd);

    CommandMap::iterator i = commandMap_.find(cmd);
    if (i != commandMap_.end())
    {
        if (enable)
        {
            i->second->set_data(STATE_MASK, gpointer(uisThreadStop));
        }
        else
        {
            i->second->set_data(STATE_MASK, gpointer(uisDisable));
        }
    }
}


void MainWindow::on_toolbar_font_set()
{
    if (progView_ && toolbar_)
    {
        progView_->apply_font(toolbar_->font_name());
    }
}


void MainWindow::apply_font(const string& fontName)
{
    if (ProgramView* view = program_view())
    {
        if (!view->apply_font(fontName))
        {
            view->redraw();
        }
    }
    CHKPTR(toolbar_)->set_font_name(fontName);
}


bool MainWindow::on_invalid_utf8(const char* text, size_t lineNum)
{
    bool result  = false;
    ostringstream msg;

    msg << "File contains invalid UTF8 in line " << lineNum << ":\n";
    msg << text << "\n";
    msg << "Continue?";

    question_message(msg.str(), &result);
    return result;
}


bool MainWindow::is_at_debug_event() const
{
    return atomic_test(atDebugEvent_);
}


void MainWindow::on_menu_manage_step_over(RefPtr<Symbol>)
{
    StepOverDialog dlg;

    dlg.signal_populate().connect(Gtk_SLOT(&debugger(), &Debugger::enum_step_over));
    dlg.signal_delete().connect(Gtk_SLOT(this, &AppSlots::step_over_delete));
    dlg.populate();
    dlg.set_transient_for(*this);
    dlg.run(*this);
}
// vim: tabstop=4:softtabstop=4:expandtab:shiftwidth=4
