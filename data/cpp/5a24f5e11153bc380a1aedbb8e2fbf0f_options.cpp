//
// $Id: options.cpp 67498 2012-01-28 04:14:52Z unknown $
//
// -------------------------------------------------------------------------
// This file is part of ZeroBugs, Copyright (c) 2010 Cristian L. Vlasceanu
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
// -------------------------------------------------------------------------
//
#include <iomanip>
#include <iostream>
#include "accel.h"
#include "options.h"
#include "options_dlg.h"
#include "dharma/sigutil.h"
#include "gtkmm/accelkey.h"
#include "gtkmm/accelmap.h"
#include "gtkmm/box.h"
#include "gtkmm/buttonbox.h"
#include "gtkmm/checkbutton.h"
#include "gtkmm/connect.h"
#include "gtkmm/flags.h"
#include "gtkmm/frame.h"
#include "gtkmm/fontselection.h"
#include "gtkmm/image.h"
#include "gtkmm/menuitem.h"
#include "gtkmm/resize.h"
#include "gtkmm/scrolledwindow.h"
#include "gtkmm/spinbutton.h"
#include "gtkmm/stock.h"
#include "gtkmm/table.h"
#include "zdk/data_filter.h"
#include "zdk/signal_policy.h"
#include "zdk/zero.h"
#include "bool_filter_param.h"
#include "layout_manager.h"
#include "main_window.h"
#include "program_view.h"
#include "text_entry.h"


using namespace std;
using namespace Gtk;


// for signal policies page
static const char* titles[] =
{
    "\nSignal",
    "\nPass to debuggee",
    "\nStop in debugger"
};


Options::Options(Debugger& debugger)
    : debugger_(debugger)
    , exceptBtn_(NULL)
    , mainBtn_(NULL)
    , traceForkBtn_(NULL)
    , spawnForkBtn_(NULL)
    , saveTabsBtn_(NULL)
    , confirmQuitBtn_(NULL)
    , confirmNextBtn_(NULL)
    , fontSel_(NULL)
    , layoutName_(DEFAULT_STRATEGY)
    , asmSyntax_(Disassembler::ASM_FIXED)
    , filterBox_(NULL)
    , tab_(NULL)
{
    layoutCurrent_ =
        debugger_.properties()->get_string("layout.strategy", DEFAULT_STRATEGY);
    if (layoutCurrent_ == "default")
    {
        layoutCurrent_ = DEFAULT_STRATEGY;
    }
    layoutName_ = layoutCurrent_;

    set_border_width(5);

    add_signals_page();
    add_lang_page();
    add_disasm_page();
    add_fonts_page();
    add_accel_page();
    add_layout_page();
    add_misc_page();
}


void Options::set_current_page_title(const char* text)
{
    get_page(get_n_pages() - 1).set_tab_label_text(string("   ") + text);
}


void Options::add_fonts_page()
{
    fontSel_ = manage(new FontSelection);

    Box& box = add_vbox("Source");
    box.add(*fontSel_);
    add_tab_size_control(box);
}


void Options::add_signals_page()
{
    Table* table = manage(new Table(2, 3));
    add(*table);

    set_current_page_title("Signals");

    for (int i = 0; i != 3; ++i)
    {
        Label* label = manage(new Label(titles[i], .0));
        table->attach(*label, i, i + 1, 0, 1,
            Gtk_FLAG(ATTACH_NONE), Gtk_FLAG(ATTACH_NONE));
    }
    ScrolledWindow* sw = manage(new ScrolledWindow());
    table->attach(*sw, 0, 3, 1, 2);

    table = manage(new Table(_NSIG - 1, 3, true));

    Gtk_add_with_viewport(sw, *table);
    Gtk_set_size(sw, 360, 300);

    sw->set_policy(Gtk_FLAG(POLICY_AUTOMATIC), Gtk_FLAG(POLICY_AUTOMATIC));

    for (int i = 1; i != _NSIG; ++i)
    {
        ostringstream os;

        os << setw(2) << i << "  " << sig_name(i);

        // label with signal's name
        Label* label = manage(new Label(os.str(), .0, .5));
        label->set_justify(Gtk_FLAG(JUSTIFY_LEFT));

        table->attach(*label, 0, 1, i - 1, i,
            Gtk_FLAG(FILL), Gtk_FLAG(ATTACH_NONE));

        // "Pass" button
        CheckButton* btn = manage(new CheckButton("Pass", .0));
        pass_.push_back(btn);

        if (debugger_.signal_policy(i)->pass())
        {
            btn->set_active(true);
        }
        table->attach(*btn, 1, 2, i - 1, i,
            Gtk_FLAG(EXPAND), Gtk_FLAG(ATTACH_NONE));

        // "Stop" button
        btn = manage(new CheckButton("Stop", .0));
        stop_.push_back(btn);

        if (debugger_.signal_policy(i)->stop())
        {
            btn->set_active(true);
        }
        table->attach(*btn, 2, 3, i - 1, i,
            Gtk_FLAG(EXPAND), Gtk_FLAG(ATTACH_NONE));
    }
    assert(pass_.size() == stop_.size());
    assert(pass_.size() == _NSIG - 1);
}


static void set_option(
    Debugger::Option    opt,
    bool                enable,
    uint64_t&           options )
{
    if (enable)
    {
        options |= opt;
        assert((options & opt) == opt);
    }
    else
    {
        options &= ~opt;
        assert((options & opt) == 0);
    }
}


void Options::apply_options(MainWindow* mainWindow)
{
#if 0
    mainWindow->set_option(Debugger::OPT_BREAK_ON_THROW, exceptBtn_->get_active());
    mainWindow->set_option(Debugger::OPT_START_AT_MAIN, mainBtn_->get_active());
    if (traceForkBtn_)
    {
        mainWindow->set_option(Debugger::OPT_TRACE_FORK, traceForkBtn_->get_active());
    }
    if (spawnForkBtn_)
    {
        mainWindow->set_option(Debugger::OPT_SPAWN_ON_FORK, spawnForkBtn_->get_active());
    }
#else
    uint64_t opts = debugger_.options();
    set_option(Debugger::OPT_BREAK_ON_THROW, exceptBtn_->get_active(), opts);
    set_option(Debugger::OPT_START_AT_MAIN, mainBtn_->get_active(), opts);

    if (traceForkBtn_)
    {
        set_option(Debugger::OPT_TRACE_FORK, traceForkBtn_->get_active(), opts);
    }

    if (spawnForkBtn_)
    {
        set_option(Debugger::OPT_SPAWN_ON_FORK, spawnForkBtn_->get_active(), opts);
    }

    mainWindow->set_option_mask(opts);
#endif
}


void Options::apply_fonts(MainWindow* wnd, const string& fontName)
{
    if (wnd)
    {
        wnd->apply_font(fontName);
    }
}


void Options::apply_signal_handling()
{
    for (int i = 1; i != _NSIG; ++i)
    {
        SignalPolicy* policy = debugger_.signal_policy(i);
        assert(policy);

        try
        {
            policy->set_stop(stop_[i - 1]->get_active());
            policy->set_pass(pass_[i - 1]->get_active());
        }
        catch (...)
        {
            stop_[i - 1]->set_active(policy->stop());
            pass_[i - 1]->set_active(policy->pass());

            throw;
        }
    }
}


bool Options::notify(const char* name, const char* descr, const Variant* v)
{
    assert(filterBox_);
    FilterParamPtr param;

    if (v)
    {
        if (v->type_tag() == Variant::VT_BOOL)
        {
            param.reset(new BoolFilterParam(name,
                                            descr,
                                            debugger_.properties(),
                                            v->bits()));
        }
    // any other types are not supported at this time
    /* todo: as needed
        else if (is_integer(*v))
        {
        }
        else if (is_float(*v))
        {
        }
     */
    }
    if (param)
    {
        param->add_to(*filterBox_);
        filterParams_.push_back(param);
    }
    else
    {
        clog << "*** Warning: param type not supported: " << name << endl;
    }
    return true;
}


void Options::add_data_filter_params()
{
    if (DataFilter* filt = interface_cast<DataFilter*>(&debugger_))
    {
        assert(filterBox_);

        filt->enum_params(this);

        // reconcile default param values with possible
        // previously saved properties

        // apply_filter_params(filt);
    }
}


void Options::apply_filter_params(DataFilter* filt)
{
    if (!filt)
    {
        filt = interface_cast<DataFilter*>(&debugger_);
    }
    if (filt)
    {
        FilterParamList::iterator i = filterParams_.begin();

        for (; i != filterParams_.end(); ++i)
        {
            if ((*i)->has_changed())
            {
                (*i)->apply();
                filt->on_param_change(debugger_.properties(), (*i)->name());
            }
        }
    }
}


CheckButton* new_check_button(const char* label)
{
    string labelText(label);

#ifdef GTKMM_2
    return new CheckButton(labelText, true);
#else

    string::size_type pos = labelText.find('_');
    if (pos != string::npos)
    {
        labelText.erase(pos, 1);
    }
    return new CheckButton(labelText, .0, .5);
#endif
}


CheckButton*
new_check_button(Debugger& debugger, uint64_t mask, const char* label)
{
    CheckButton* btn = new_check_button(label);
    if (debugger.options() & mask)
    {
        btn->set_active(true);
    }
    return btn;
}


void Options::add_lang_page()
{
    Box& box = add_vbox("Language");
    exceptBtn_ = manage(new_check_button(debugger_,
                                         Debugger::OPT_BREAK_ON_THROW,
                                         "Break on _exceptions"));
    mainBtn_ = manage(new_check_button( debugger_,
                                        Debugger::OPT_START_AT_MAIN,
                                        "Always start debugging at _main()"));
    box.pack_start(*exceptBtn_, false, false);
    box.pack_start(*mainBtn_, false, false);

    traceForkBtn_ = manage(new_check_button(debugger_,
                                        Debugger::OPT_TRACE_FORK,
                                        "Trace _forked processes"));
    spawnForkBtn_ = manage(new_check_button(debugger_,
                                        Debugger::OPT_SPAWN_ON_FORK,
                                        "Spawn new debugger instance on fork()"));
    Gtk_CONNECT_1(traceForkBtn_, toggled, this, &Options::on_trace_fork, traceForkBtn_);
    box.pack_start(*traceForkBtn_, false, false);
    box.pack_start(*spawnForkBtn_, false, false);
    if (debugger_.enum_processes(NULL))
    {
        traceForkBtn_->set_sensitive(false);
        spawnForkBtn_->set_sensitive(false);
    }
    else
    {
        spawnForkBtn_->set_sensitive(traceForkBtn_->get_active());
    }

    filterBox_ = manage(new VBox);
    box.pack_start(*filterBox_, false, false, 20);
    add_data_filter_params();
}


void Options::add_layout_controls(Gtk::Box& box)
{
#ifdef GTKMM_2
    //
    // Add controls for layout selection.
    // Changing the layout causes a crash under Gtkmm-1.x
    //
    Gtk::Frame* frame = manage(new Gtk::Frame("Layout"));
    box.pack_start(*frame, false, false, 30);

    Box* layoutBox = manage(new VBox);
    layoutBox->set_border_width(5);
    frame->add(*layoutBox);

    add_layout_entry(*layoutBox, "_Classic", ALL_VISIBLE);
    add_layout_entry(*layoutBox, "_Tabbed",  TAB_LAYOUT);
    add_layout_entry(*layoutBox, "_Left Tab", LEFT_LAYOUT);
    add_layout_entry(*layoutBox, "_Split Tab", SPLIT_TAB_LAYOUT);

    Box* infoBox = manage(new HBox);
    box.pack_end(*infoBox, false, false);

    Image* img = manage(new Image(Stock::DIALOG_INFO, ICON_SIZE_DIALOG));
    infoBox->pack_start(*img);

    Label* label = manage(new Label(
"The \"classic\" user interface layout allows you to see code,\n"
"registers, stack trace and variables, all at once.\n\n"
"The \"tabbed\" styles make more efficient use of the screen space."));

    infoBox->pack_end(*label, false, false, 10);
#endif // GTKMM_2
}


void Options::add_layout_page()
{
    //Box& box = add_vbox("Look and Feel");
    Box& box = add_vbox("Layout");
    // this option controls whether to restore the open
    // source files when debugging a saved target
    saveTabsBtn_ = manage(new_check_button("Save source tabs at e_xit"));
    box.pack_start(*saveTabsBtn_, false, false);

    if (debugger_.properties()->get_word("atexit.savetabs", 0))
    {
        saveTabsBtn_->set_active(true);
    }

    confirmQuitBtn_ = manage(new_check_button("Confirm before _Quit"));
    box.pack_start(*confirmQuitBtn_, false, false);
    if (debugger_.properties()->get_word("confirm_quit", 1) == 0)
    {
        confirmQuitBtn_->set_active(true);
    }
    confirmNextBtn_ = manage(new_check_button(
        "Confirm stepping if not at current frame"));
    box.pack_start(*confirmNextBtn_, false, false);
    if (debugger_.properties()->get_word("confirm_next", 0) == 0)
    {
        confirmNextBtn_->set_active(true);
    }
    add_layout_controls(box);
}


void Options::add_tab_size_control(Box& box)
{
#if GTKMM_2
    Gtk::Frame* f = manage(new Gtk::Frame("Tab Size"));
    box.pack_start(*f, false, false, 7);
    Gtk::Box* b = manage(new Gtk::HBox);
    b->set_border_width(3);
    f->add(*b);
    Gtk::SpinButton* spin = manage(new Gtk::SpinButton(1.0));
    spin->set_range(1, 16);
    spin->set_numeric();
    spin->set_wrap();
    spin->set_increments(1.0, 2.0);
    spin->set_snap_to_ticks();
    spin->set_value(debugger_.properties()->get_word("tab_size", 4));
    b->pack_start(*spin, false, false, 5);
    tab_ = spin;
#endif
}


void Options::add_layout_entry(Box& box, const char* label, string name)
{
    RadioButton* btn = manage(new RadioButton(layoutGroup_, label, true));
    box.pack_start(*btn, false, false);

    if (layoutCurrent_ == name)
    {
        btn->set_active(true);
    }
    Gtk_CONNECT_2(btn, toggled, this, &Options::set_layout_name, btn, name);
}


void Options::set_layout_name(RadioButton* btn, string name)
{
    if (btn->get_active())
    {
        layoutName_ = name;
    }
}


void Options::apply_layout(MainWindow* mainWindow)
{
    if (layoutName_ != layoutCurrent_)
    {
        mainWindow->layout_work_area(layoutName_);
        debugger_.properties()->set_string("layout.strategy", layoutName_.c_str());
    }
    debugger_.properties()->set_word("atexit.savetabs", saveTabsBtn_->get_active());
    debugger_.properties()->set_word("confirm_quit", !confirmQuitBtn_->get_active());
    debugger_.properties()->set_word("confirm_next", !confirmNextBtn_->get_active());
    if (tab_)
    {
        debugger_.properties()->set_word("tab_size", tab_->get_value_as_int());
    }
}


void Options::add_accel_page()
{
    Box& box = add_vbox("Accelerators");

    add_accel_entry(box, "menu_new_breakpoint");
    add_accel_entry(box, "menu_edit_breakpoints");
    add_accel_entry(box, "menu_clear_breakpoints");
    add_accel_entry(box, "menu_continue");
    add_accel_entry(box, "menu_next");
    add_accel_entry(box, "menu_return");
    add_accel_entry(box, "menu_step");
    add_accel_entry(box, "menu_step_instruction");
    add_accel_entry(box, "menu_restart");
}


void Options::add_accel_entry(Box& box, const char* name)
{
    assert(name);

    string path("<Action>/");
    path += name;

    Box* hbox = manage(new HBox);
    box.pack_start(*hbox, false, false);
    hbox->set_border_width(3);

    string text;

    MenuItemPtr mip = TheAccelMap::instance().get(path);
    Label* accelLabel = Gtk_get_label(mip);
    if (accelLabel)
    {
        text = accelLabel->get_text();
    }

    Label* label = manage(new Label(text, .0, .5));
    label->set_justify(Gtk_FLAG(JUSTIFY_LEFT));

    hbox->pack_start(*label, false, false);
    Gtk_set_size(label, 180, -1);

    TextEntry* entry = manage(
        new TextEntry(debugger_.properties(), name));
    hbox->pack_end(*entry);

    accelEntryMap_[name] = entry;

#ifdef GTKMM_2
    //use the gtk function because AccelMap::lookup_entry
    // is only available in gtkmm 2.10 and above
    GtkAccelKey ak;
    if (gtk_accel_map_lookup_entry(path.c_str(), &ak))
    {
        string accel = AccelGroup::name(ak.accel_key,
                              Gdk::ModifierType(ak.accel_mods));
        entry->set_text(accel);
    }
#else
    AccelKey ak(mip->accel_key);
    string accelText = AccelGroup::name(Gtk_accel_key(ak), Gtk_accel_mod(ak));
    entry->set_text(accelText);
#endif
}


void Options::apply_accels(MainWindow* mainWindow)
{
    AccelEntryMap::const_iterator i = accelEntryMap_.begin();
    for (; i != accelEntryMap_.end(); ++i)
    {
        const string accel = i->second->get_text();
        if (accel.empty())
        {
            continue;
        }

        string path = "<Action>/" + i->first;
        AccelKey ak(accel);

        if (!AccelMap::change_entry(path, ak))
        {
            /* const string errmsg =
                "Could not set accelerator for " + path + " to: "
                + Gtk_accel_abbrev(ak);

            mainWindow->error_message(errmsg); */
        }
    }
    mainWindow->save_key_bindings();
}



void Options::add_disasm_syntax(Box& box,
                                const char* labelText,
                                Disassembler::Syntax syntax,
                                Disassembler::Syntax current)
{
#ifdef GTKMM_2
    RadioButton* btn = manage(new RadioButton(asmGroup_, labelText, true));

#else
    RadioButton* btn = manage(new RadioButton(asmGroup_, labelText, .0, .5));
#endif
    box.pack_start(*btn, false, false);

    if (syntax == current)
    {
        btn->set_active(true);
        asmSyntax_ = syntax;
    }
    Gtk_CONNECT_2(btn, toggled, this, &Options::set_asm_syntax, btn, syntax);
}


void Options::add_disasm_page()
{
    if (Disassembler* disasm = interface_cast<Disassembler*>(&debugger_))
    {
        Disassembler::Syntax syntax = disasm->syntax();

        // syntax can be set by user?
        if (syntax != Disassembler::ASM_FIXED)
        {
            Box& box = add_vbox("Disassembler");

            add_disasm_syntax(box, "AT&T Syntax", Disassembler::ASM_ATT, syntax);
            add_disasm_syntax(box, "Intel Syntax", Disassembler::ASM_INTEL, syntax);

            Box* infoBox = manage(new HBox);

            box.pack_end(*infoBox, false, false, 10);

        #ifdef GTKMM_2
            Image* img = manage(new Image(Stock::DIALOG_INFO, ICON_SIZE_DIALOG));
            infoBox->pack_start(*img, false, false);
        #endif
            Label* label = manage(new Label(
"The Intel assembly language syntax differs from AT&T's:\n"
"in Intel syntax there are no register prefixes or immediate prefixes;\n"
"in AT&T, registers are prefixed with a '%' and immediates are prefixed\n"
"with a dollar sign '$'. Select the one that you are most familiar with."));

            infoBox->pack_end(*label, false, false, 10);
        }
    }
}


void
Options::set_asm_syntax(Gtk::RadioButton*, Disassembler::Syntax syntax)
{
    asmSyntax_ = syntax;
}


void Options::apply_asm_syntax()
{
    if (asmSyntax_ != Disassembler::ASM_FIXED)
    {
        if (Disassembler* disasm = interface_cast<Disassembler*>(&debugger_))
        {
            disasm->set_syntax(asmSyntax_);
            debugger_.properties()->set_word("asm_syntax", asmSyntax_);
        }
    }
}



void Options::add_misc_page()
{
    // catch-all / place-holder options page
}


Box& Options::add_vbox(const char* tabLabel)
{
    Box* box = manage(new VBox);
    add(*box);
    box->set_border_width(10);
    set_current_page_title(tabLabel);

    return *box;
}


void Options::on_trace_fork(ToggleButton* btn)
{
    if (spawnForkBtn_)
    {
        spawnForkBtn_->set_sensitive(btn->get_active());
    }
}


OptionsDialog::OptionsDialog(Debugger& debugger)
    : DialogBox(btn_ok_cancel, "Options")
    , options_(manage(new Options(debugger)))
{
    get_vbox()->add(*options_);
    get_button_box()->set_layout(Gtk_FLAG(BUTTONBOX_END));
}



void OptionsDialog::run(MainWindow& mainWindow)
{
    FontSelection* fontSel = options_->font_selection();
    ProgramView* progView = mainWindow.program_view();

    string fontName;

    if (progView)
    {
        fontName = progView->font_name();
        fontSel->set_font_name(fontName);

        if (Entry* entry = fontSel->get_size_entry())
        {
            if (entry->get_text().empty() || entry->get_text() == "0")
            {
                fontSel->set_font_name(fontName + " 10");
            }
        }
        else
        {
            clog << "font not found: " << fontName << endl;
        }
        fontName = fontSel->get_font_name();
    }

    if (DialogBox::run(&mainWindow) == btn_ok)
    {
        if (fontName == fontSel->get_font_name())
        {
            // font name has not changed, keep the settings
            fontName = progView->font_name();
        }
        else
        {
            fontName = fontSel->get_font_name();
        }
        options_->apply_signal_handling();
        options_->apply_options(&mainWindow);
        options_->apply_accels(&mainWindow);
        options_->apply_asm_syntax();
        options_->apply_filter_params();
        options_->apply_layout(&mainWindow);

        // apply fonts last, because it internally triggers a redraw
        options_->apply_fonts(&mainWindow, fontName);
    }
}

// vim: tabstop=4:softtabstop=4:expandtab:shiftwidth=4
