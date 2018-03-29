//
// $Id: edit_breakpoint_dlg.cpp 67498 2012-01-28 04:14:52Z unknown $
//
// -------------------------------------------------------------------------
// This file is part of ZeroBugs, Copyright (c) 2010 Cristian L. Vlasceanu
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
// -------------------------------------------------------------------------
//
#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <sstream>

#include "gtkmm/adjustment.h"
#include "gtkmm/checkbutton.h"
#include "gtkmm/button.h"
#include "gtkmm/connect.h"
#include "gtkmm/frame.h"
#include "gtkmm/flags.h"
#include "gtkmm/label.h"
#include "gtkmm/list.h"
#include "gtkmm/resize.h"
#include "gtkmm/scrolledwindow.h"
#include "gtkmm/spinbutton.h"
#include "check_listitem.h"
#include "popup_menu.h"
#include "text_entry.h"
#include "dharma/symbol_util.h"
#include "zdk/breakpoint_util.h"
#include "zdk/check_ptr.h"
#include "zdk/switchable.h"
#include "zdk/thread_util.h"
#include "edit_breakpoint_dlg.h"


using namespace std;
using namespace SigC;
using namespace Gtk;

#if !defined(GTKMM_2)
 using namespace Gtk::List_Helpers;
#endif

typedef RefPtr<BreakPointAction> ActionPtr;

/**
 * Holds breakpoint information while editing the breakpoints
 */
struct ZDK_LOCAL BreakPointInfo
    : public ZObjectImpl<>
    , private EnumCallback<BreakPointAction*>
{
DECLARE_UUID("c6459df5-06ef-4406-ac6c-e86e0765803d")

BEGIN_INTERFACE_MAP(BreakPointInfo)
    INTERFACE_ENTRY(BreakPointInfo)
END_INTERFACE_MAP()

    RefPtr<BreakPoint> bpnt_;   //! associated breakpoint
    string condition_;          //! expression for conditional activation
    size_t activation_;         //! hit count activation threshold
    size_t hitCount_;           //! current hit count
    bool autoReset_;            //! reset automatically after activation?
    RefPtr<Symbol> symbol_;     //! symbol at breakpoint
    ActionPtr userAction_;      //! associated user action

    bool isDeferred_;

    ~BreakPointInfo() throw() {}

    explicit BreakPointInfo(volatile BreakPoint& bpnt)
        : bpnt_(const_cast<BreakPoint*>(&bpnt))
        , activation_(0)
        , hitCount_(0)
        , autoReset_(false)
        , isDeferred_(bpnt.is_deferred())
    {
        bpnt.enum_actions("USER", this);
    }

    explicit BreakPointInfo(Symbol* symbol)
        : activation_(0)
        , hitCount_(0)
        , autoReset_(0)
        , symbol_(symbol)
        , isDeferred_(false)
    {
    }

    bool is_deferred() const { return isDeferred_; }

    void notify(BreakPointAction* act)
    {
        assert(strcmp(act->name(), "USER") == 0);

        if (Switchable* sw = interface_cast<Switchable*>(act))
        {
            // there should be only one user-defined action
            // per breakpoint
            // assert(userAction_.is_null());

            userAction_ = act;
            assert(activation_ == 0);
            assert(hitCount_ == 0);

            activation_ = sw->activation_counter();
            hitCount_  = sw->counter();
            condition_ = sw->activation_expr();
            autoReset_ = sw->auto_reset();
        }
    }
};


BreakPointSite::BreakPointSite(const volatile BreakPoint& bpnt)
    : addr_(bpnt.addr()), tid_(0)
{
    if (bpnt.type() != BreakPoint::GLOBAL)
    {
        tid_ = CHKPTR(bpnt.thread())->lwpid();
    }
}


// hint message that shows at the top of the dialog box
static const char topmsg[] =
    "Right-click on the list to edit selected items, "
    "click the OK button to commit the changes. ";

// dialog title
static const char title[] = "Edit Breakpoints";


EditBreakPointDialog::~EditBreakPointDialog()
{
}


EditBreakPointDialog::EditBreakPointDialog
(
    Debugger& debugger,
    const Thread* thread
)
    : SelectDialog(btn_ok_cancel, title, topmsg, false)
    , conditionsBox_(manage(new Gtk::VBox))
    , hits_(0)
    , activation_(0)
    , autoReset_(0)
    , condition_(0)
    , pid_(CHKPTR(thread->process())->pid())
{
    get_ok_button()->set_sensitive(true);
    Gtk_set_size(get_scrolled_window(), -1, 200);
    conditionsBox_->set_sensitive(false);
    conditionsBox_->set_border_width(8);
    conditionsBox_->set_spacing(5);

    // The bottom of the dialog allows the user to specify
    // conditions for activating the breakpoint selected
    // from the list -- the activation can be conditioned
    // by an expression evaluating to true, and by the breakpoint
    // being reached a number of times -- the activation_counter,
    // or hit count threshold.
    Gtk::Frame* frame = manage(new Gtk::Frame("Conditions"));
    frame->add(*conditionsBox_);
    frame->set_border_width(5);

    get_vbox()->pack_start(*frame, false, false);
    get_vbox()->set_spacing(3);

    // --- condition expression -------------------------------
    Gtk::HBox* hbox = manage(new Gtk::HBox);
    conditionsBox_->pack_start(*hbox, false, false);

    static const char text[] = "Activate when expression is true:";
    Gtk::Label* label = manage(new Gtk::Label(text, .0));
    hbox->pack_start(*label, false, false);
    hbox->set_spacing(3);

    condition_ = manage(new TextEntry(debugger.properties(), "break_cond"));
    hbox->pack_start(*condition_);

    hbox = manage(new Gtk::HBox);
    hbox->set_spacing(3);
    conditionsBox_->pack_start(*hbox, false, false);

    // --- hit count ------------------------------------------
    label = manage(new Label("Activate after", .0));
    hbox->pack_start(*label, false, false);

    Gtk::Adjustment* adj = manage(new Gtk::Adjustment(0, 0, INT_MAX));
    activation_ = manage(new SpinButton(*adj, 1));
    hbox->pack_start(*activation_, false, false);
    hbox->pack_start(*manage(new Label("hits.")), false, false);

    Gtk_set_size(activation_, 100, -1);

    activation_->set_numeric(true);
    activation_->set_wrap(true);
    activation_->set_update_policy(Gtk_FLAG(UPDATE_ALWAYS));

    Gtk::Button* btn = manage(new Gtk::Button("Reset"));
    hbox->pack_end(*btn, false, false);

    Gtk_set_size(btn, 80, -1);

    Gtk_CONNECT_0(btn, clicked, this, &EditBreakPointDialog::on_reset_count);

    autoReset_ = manage(new CheckButton("Auto-reset", .0));
    hbox->pack_end(*autoReset_, false, false);

    hits_ = manage(new Gtk::Label("", .0));
    hbox->pack_end(*hits_, false, false);
    update_current_hits(0);

    populate_breakpoint_list(debugger);
}


void EditBreakPointDialog::populate_breakpoint_list(Debugger& debugger)
{
    BreakPointManager& mgr = interface_cast<BreakPointManager&>(debugger);
    mgr.enum_breakpoints(this);
}


/**
 * Implements the EnumCallback<volatile BreakPoint*> interface:
 * add breakpoint to the list shown inside of this dialog.
 */
void EditBreakPointDialog::notify(volatile BreakPoint* bpnt)
{
    // only interested in breakpoints that have user actions
    if (!bpnt || CHKPTR(bpnt)->enum_actions("USER") == 0)
    {
        return;
    }
    if (!thread_in_process(*CHKPTR(bpnt->thread()), pid_))
    {
        return;
    }
    ostringstream buf;
    if (Symbol* symbol = bpnt->symbol())
    {
        print_symbol(buf, bpnt->addr(), symbol);
        assert(!buf.str().empty());
    }
    const bool enabled = has_enabled_actions(*bpnt);

    CheckListItem* item = manage(new CheckListItem(buf.str(), enabled));
    RefPtr<BreakPointInfo> info = new BreakPointInfo(*bpnt);

    item->set_data(info);

    Gtk_CONNECT_1(item, button_press_event, this, &Dialog::on_click, item);
    item->toggled.connect(Gtk_BIND(Gtk_SLOT(this, &Dialog::on_toggled), item));

    get_list()->add(*item);
}


void EditBreakPointDialog::notify(SymbolTable* table)
{
    CHKPTR(table)->enum_deferred_breakpoints(this);
}


/**
 * Add deferred breakpoints to list
 */
void EditBreakPointDialog::notify(BreakPoint* bpnt, const SymbolTable*)
{
    notify(bpnt);
}


/**
 * Popup menu on right click
 */
int
EditBreakPointDialog::on_click(GdkEventButton* event, CheckListItem* item)
{
    assert(event);
    assert(item);

    if (event->type == GDK_BUTTON_PRESS)
    {
        switch (event->button)
        {
        case 3: // right-click?
            if (get_list()->selection().empty())
            {
                Gtk_list_item_select(*get_list(),
                    static_cast<Gtk::ListItem*>(CHKPTR(item)));
            }
            popup_menu(*event, *item);
            break;
        }
    }
    return 0;
}


/**
 * When a selected item is toggled, toggle all other selected
 * items as well
 */
void EditBreakPointDialog::on_toggled(CheckListItem* item)
{
    assert(item);

    if (item->get_state() == Gtk_FLAG(STATE_SELECTED))
    {
        const bool flag = is_checked(item);

        Gtk::List::SelectionList& sel = get_list()->selection();

        Gtk::List::SelectionList::iterator i = sel.begin();
        for (; i != sel.end(); ++i)
        {
            check(*i, flag);
        }
    }
}


/**
 * Take the selected items off the list and stick the associated
 * breakpoint address into the delete_ list.
 */
void EditBreakPointDialog::on_delete()
{
    SelectionList& selection = get_list()->selection();

    SelectionList::iterator i = selection.begin();
    SelectionList::iterator end = selection.end();
    for (; i != end; ++i)
    {
        if (CheckListItem* cli = dynamic_cast<CheckListItem*>((ListItem*)*i))
        {
            assert(cli->data().get());

            RefPtr<BreakPointInfo> info =
                interface_cast<BreakPointInfo>(cli->data());
            assert(!info.is_null());
            if (info.is_null())
            {
                throw logic_error("missing breakpoint info");
            }
            // add to the "to-be-deleted" list
            if (info->bpnt_.get())
            {
                deletionList_.push_back(BreakPointSite(*info->bpnt_));
            }
            else
            {
                assert(info->symbol_.get());
                deferredDeletionList_.push_back(info->symbol_);
            }
        }
    }
    get_list()->remove_items(selection.begin(), end);
}


void EditBreakPointDialog::on_enable(bool flag)
{
    SelectionList::iterator i = get_list()->selection().begin();
    SelectionList::iterator end = get_list()->selection().end();

    for (; i != end; ++i)
    {
        if (CheckListItem* cli = dynamic_cast<CheckListItem*>((ListItem*)*i))
        {
            cli->check(flag);
        }
    }
}


/**
 * Construct and popup the right-click menu
 */
void EditBreakPointDialog::popup_menu(
    const GdkEventButton&   event,
    CheckListItem&          item)
{
    auto_ptr<PopupMenu> menu(new PopupMenu);
    bool can_enable = false;
    bool can_disable = false;

    vector<Gtk::SelectionItem> items =
        Gtk_get_selection_items(get_list()->selection());

    vector<Gtk::SelectionItem>::const_iterator i = items.begin(),
        end = items.end();

    for (; i != end; ++i)
    {
        if (has_check_button(*i))
        {
            if (is_checked(*i))
            {
                can_disable = true;
            }
            else
            {
                can_enable = true;
            }
        }
    }
    MenuItem* mitem = new MenuItem("Disable");
    menu->add_manage_item(mitem);

    if (can_disable)
    {
        Gtk_CONNECT_1(mitem, activate, this, &Dialog::on_enable, false);
    }
    else
    {
        mitem->set_sensitive(false);
    }
    mitem = new MenuItem("Enable");
    menu->add_manage_item(mitem);

    if (can_enable)
    {
        Gtk_CONNECT_1(mitem, activate, this, &Dialog::on_enable, true);
    }
    else
    {
        mitem->set_sensitive(false);
    }
    menu->items().push_back(Gtk::Menu_Helpers::SeparatorElem());

    RefPtr<BreakPointInfo> info =
        interface_cast<BreakPointInfo>(item.data());
    if (info.is_null())
    {
        throw logic_error("missing breakpoint info");
    }

    mitem = 0;
    RefPtr<BreakPoint> bpnt = info->bpnt_;

    if (bpnt.get())
    {
        mitem = new MenuItem("Goto Code");
        Gtk_CONNECT(mitem, activate, bind(show_code.slot(), bpnt));
    }

    if (mitem)
    {
        menu->add_manage_item(mitem);
        Gtk_CONNECT_0(mitem, activate, this, &DialogBox::close_dialog);
        menu->items().push_back(Gtk::Menu_Helpers::SeparatorElem());
    }
    mitem = new MenuItem("Remove");
    menu->add_manage_item(mitem);
    Gtk_CONNECT_0(mitem, activate, this, &Dialog::on_delete);

    menu.release()->popup(event.button, event.time);
}


DialogBox::ButtonID EditBreakPointDialog::run(const Widget* parent)
{
    ButtonID btnID = DialogBox::run(parent);
    if (btnID == btn_ok)
    {
        on_selection_changed_impl();
        commit_breakpoint_changes();
    }
    return btnID;
}


/**
 * Commit changes to breakpoints (and their associated actions)
 */
void EditBreakPointDialog::commit_breakpoint_changes()
{
#ifdef GTKMM_2
    get_list()->select_all();

    SelectionList::const_iterator i = get_list()->selection().begin();
    SelectionList::const_iterator end = get_list()->selection().end();
#else
    ItemList::const_iterator i = get_list()->items().begin();
    ItemList::const_iterator end = get_list()->items().end();
#endif
    for (; i != end; ++i)
    {
        const CheckListItem& item = reinterpret_cast<const CheckListItem&>(**i);

        RefPtr<BreakPointInfo> info =
            interface_cast<BreakPointInfo>(item.data());

        if (info.is_null())
        {
            throw logic_error("missing breakpoint info");
        }
        if (info->bpnt_.is_null())
        {
            continue; // skip deferred breakpoints
        }
        BreakPointAction& action = *CHKPTR(info->userAction_.get());
        Switchable& sw = interface_cast<Switchable&>(action);

        if (is_checked(*i))
        {
            sw.enable();
        }
        else
        {
            sw.disable();
        }
        sw.set_activation_counter(info->activation_, info->autoReset_);
        sw.set_activation_expr(info->condition_.c_str());
        sw.set_counter(info->hitCount_);

        breakpoint_modified(info->bpnt_->addr()); // notify observers
    }

    delete_breakpoints(deletionList_);
}



void EditBreakPointDialog::on_selection_changed_impl()
{
    if (current_.get())
    {
        current_->condition_  = CHKPTR(condition_)->get_text();
        current_->activation_ = CHKPTR(activation_)->get_value_as_int();
        current_->autoReset_  = CHKPTR(autoReset_)->get_active();
    }

    Gtk::List::SelectionList selection = get_list()->selection();
    // TODO: has_check_button() actually means that this is a real
    // breakpoint (and not a deferred one). Change the condition here
    // to be self-documenting, has_check_button does not convey the
    // actual logic, which is that a deferred breakpoint cannot be
    // a conditional breakpoint.
    if ((selection.size() == 1) && has_check_button(*selection.begin()))
    {
        CHKPTR(conditionsBox_)->set_sensitive(true);
    }
    else
    {
        CHKPTR(conditionsBox_)->set_sensitive(false);
    }
    if (!selection.empty())
    {
        CheckListItem& item =
            reinterpret_cast<CheckListItem&>(**selection.begin());
        current_ = interface_cast<BreakPointInfo>(item.data());
        if (current_.is_null())
        {
            throw logic_error("missing breakpoint info");
        }
        if (current_->is_deferred())
        {
            CHKPTR(conditionsBox_)->set_sensitive(false);
        }

        update_current_hits(current_->hitCount_);
        CHKPTR(condition_)->set_text(current_->condition_);
        CHKPTR(activation_)->set_value(current_->activation_);
        CHKPTR(autoReset_)->set_active(current_->autoReset_);
    }
}


void EditBreakPointDialog::on_reset_count()
{
    if (current_.get())
    {
        current_->hitCount_ = 0;
    }
    update_current_hits(0);
}


void EditBreakPointDialog::update_current_hits(unsigned long count)
{
    char buf[256];
    snprintf(buf, 256, "Current breakpoint hits: %lu", count);

    CHKPTR(hits_)->set_text(buf);
}
// vim: tabstop=4:softtabstop=4:expandtab:shiftwidth=4
