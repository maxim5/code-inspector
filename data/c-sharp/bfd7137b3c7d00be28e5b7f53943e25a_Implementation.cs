using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.SoftwarePlusServices.ReferenceBits.OutlookPlusServices;
using Outlook = Microsoft.Office.Interop.Outlook;
using Microsoft.SoftwarePlusServices.ReferenceBits.OutlookPlusServices.Regions;

namespace Microsoft.SoftwarePlusServices.ReferenceBits.OutlookPlusServices
{
    ///
    /// <summary>
    /// The members class are called by the VSTO-generated ThisAddIn class to build and/or initialize
    /// all of the items which are implemented by your addin:
    ///     Folder(s)
    ///     Folder View(s)
    ///     Custom Ribbon UX
    ///     Option Panes (aka PropertyPages in the Tools -> Options dialog)
    ///     Scheduled Tasks
    ///
    /// Version 1.0
    /// October 2007
    /// Licensed under the Microsoft Permissive Licence
    /// See www.codeplex.com for more information and support forums
    /// 
    /// Visit the http://channel9.msdn.com/ReferenceBits WIKI for more information including
    /// screencasts, interviews with the authors, and a community of other Software + Services developers
    /// </summary>
    /// 
    public class Implementation
    {

        //
        // Outlook can support zero or more property pages which surface in the Tools -> Options dialog.
        // These variables define the property pages and events that they fire.
        //
        private List<PropertyPageInfo> _propertyPages = new List<PropertyPageInfo>();

        //
        // Outlook + Services supports zero or more "scheduled" tasks that can perform background updates
        // Each scheduled task can fire at a different rate.  This is a list of the scheduler objects
        //
        private List<SchedulerInfo> _schedulerList = new List<SchedulerInfo>();

        //
        // Outlook + Services support zero or more custom Outlook folders.
        // This list keeps track of all the folders and their hierarchy
        //
        private List<OutlookFolderInfo> _folders = new List<OutlookFolderInfo>();

        /// <summary>
        /// This is reference to Outlook for hooking up events, etc.
        /// </summary>
        private Outlook.Application _application;

        /// <summary>
        /// This is a reference to the Root folder where our folders live
        /// </summary>
        private static Outlook.Folder _rootFolder;

        //patrickyong
        private FormRegionManager _formRegionService = new FormRegionManager(); 


        ///
        /// <summary>
        /// Returns the one and only RibbonFactory for this add-in
        ///
        /// </summary>
        /// <returns></returns>
        ///
        public static Regions.RibbonFactory GetRibbonFactory()
        {
            return (new Regions.RibbonFactory(Microsoft.SoftwarePlusServices.ReferenceBits.OutlookPlusServices.Properties.Resources.ResourceManager));
        }

        ///
        /// <summary>
        /// Returns the Resource Manager class.
        /// This is necessary since the Resources can only exist in one assembly.
        /// </summary>
        /// <returns></returns>
        ///
        public System.Resources.ResourceManager GetResourceManager()
        {
            return (Properties.Resources.ResourceManager);
        }

        ///
        /// <summary>
        /// Here is where you put logic that should be run before any
        /// folders, regions, ribbons, option panes and scheduled tasks are initialized
        /// </summary>
        /// 
        public void InitialSetup(Outlook.Application appRef)
        {
            _application = appRef;

        }


        ///
        /// <summary>
        /// Here is where we put initialization logic to create folders.
        /// </summary>
        /// 
        public void BuildFolders()
        {
            Outlook.Folder rootFolder = GetPersonalFolder();
            Implementation._rootFolder = rootFolder;

            OutlookFolder IssueTrackerClass_Object = new Folders.IssueTrackerClass(rootFolder);
_folders.Add(new OutlookFolderInfo(IssueTrackerClass_Object.Folder, rootFolder));

/**@#$FOLDER_BUILDERS_GO_HERE@#$**/

            if (_folders.Count == 0)
                System.Windows.Forms.MessageBox.Show("Your Outlook+Services AddIn is running but you have not yet created any Folders.  Right-click on the _Folders folder in the OUTLOOK-PLUS-SERVICES-IMPLEMENTATION project and select Configure from the S+S Blueprints context menu to create a folder.", "Implementation Alert", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Exclamation);
        }

        public void BuildRegions()
        {

            Regions.IItemRegion IssueTrackerFolderView_Object = new Regions.IssueTrackerFolderView(new Guid().ToString());
_formRegionService.AddItemRegion(IssueTrackerFolderView_Object, "IssueTrackerFolderViewResource");

/**@#$ITEMREGIONCONSTRUCTORS_GO_HERE@#$**/
        }

        ///
        /// <summary>
        /// Here is where we put initialization logic that builds custom UI ribbons
        /// </summary>
        /// <param name="_ribbons"></param>
        /// 
        public void BuildRibbons()
        {
            //@#$RIBBON_INIT_GOES_HERE@#$//

            //@#$RIBBON_ACTIONS_GO_HERE@#$//

        }

        public void BuildScheduledTasks()
        {
            _schedulerList.Add(new SchedulerInfo("GetTaskList", new _ScheduledTasks.GetTaskList(), 10, false));

/**@#$SCHEDULED_TASKS_GO_HERE@#$**/
        }

        public void BuildOptionPages()
        {

            /**@#$OPTION_PANES_GO_HERE@#$**/

            // connect to the option page add event
            _application.OptionsPagesAdd +=
                new Outlook.ApplicationEvents_11_OptionsPagesAddEventHandler(AddOptionPages);

        }

        ///
        /// <summary>
        /// When Outlook needs to populate its Options dialog, it fires an OptionPagesAdd event that is handled 
        /// by this method.  This method adds all the custom settings property pages to Outlook's options dialog.
        /// </summary>
        /// <param name="Pages">A list of property pages displayed in Outlook's options dialog</param>
        /// 
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Reliability", "CA2000:DisposeObjectsBeforeLosingScope")]
        private void AddOptionPages(Outlook.PropertyPages Pages)
        {
            foreach (PropertyPageInfo _propertyPage in _propertyPages)
            {
                Pages.Add(_propertyPage.Page, _propertyPage.Title);
            }
        }

        ///
        /// <summary>
        /// Loop through the scheduled tasks lists and start them up.
        /// </summary>
        ///
        public void StartScheduledTasks()
        {
            //
            // And finally, fire off the scheduled tasks
            //
            foreach (SchedulerInfo _scheduler in _schedulerList)
            {
                _scheduler.Scheduler.StartTimer(_scheduler.Time);
                //
                // A scheduled task can either fire once the initial timer
                // expires or it can be called during initialization.
                // 
                if (_scheduler.PerformInitialUpdate)
                    _scheduler.Scheduler.UpdateNow();
            }
        }

        ///
        /// <summary>
        /// Called by addin shutdown to stop all the schedule task timers
        /// </summary>
        /// 
        public void StopScheduledTasks()
        {
            foreach (SchedulerInfo _scheduler in _schedulerList)
            {
                _scheduler.Scheduler.StopTimer();
            }
        }

        ///
        /// <summary>
        /// When settings change, the settings classes fire a PropertyChanged event.  This method handles this 
        /// event and if there is an event handler for this name, the event handler's PropertyChanged method is called.
        /// </summary>
        /// <param name="sender">A reference to the object that cause the event.</param>
        /// <param name="e">A <see cref="PropertyChangedEventArgs" /> object containing information about the property changed.</param>
        /// 
        private void SettingsPropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            //foreach (PropertyPageInfo _propertyPage in PropertyPages)
            //{
            //    if (e.PropertyName == _propertyPage.EventName)
            //    {
            //        if (_propertyPage.EventHandler != null)
            //            _propertyPage.EventHandler.PropertyChanged(sender, e);
            //        break;
            //    }
            //}
        }

        /// <summary>
        /// Enable this object to handle the PropertySettingsChanged event
        /// to call the SettingsPropertyChanged method
        /// </summary>
        public void EnablePropertySettingsEvents()
        {
            Properties.Settings.Default.PropertyChanged +=
                new System.ComponentModel.PropertyChangedEventHandler(SettingsPropertyChanged);

        }


        ///
        /// <summary>
        /// Here is where you put any final logic that should be run before
        /// Outlook starts calling your logic
        /// </summary>
        /// 
        public void FinalSetup()
        {
            //
            // Put your custom initialization code here
            //
        }

        ///
        /// <summary>
        /// Return the list of PropertyPageInfo objects
        /// </summary>
        ///
        public List<PropertyPageInfo> PropertyPages
        {
            get
            {
                return (_propertyPages);
            }
        }

        ///
        /// <summary>
        /// Return the "root" Outlook.Folder where our folders live
        /// </summary>
        ///
        public static Outlook.Folder RootFolder
        {
            get { return _rootFolder; }
        }

        ///
        /// <summary>
        /// Find the parent of the inbox (aka the Default Folder)
        /// </summary>
        /// <returns>The "default" folder for the active user</returns>
        ///
        public Outlook.Folder GetPersonalFolder()
        {
            // find the inbox
            Outlook.Folder inbox =
                (Outlook.Folder)_application.Session.GetDefaultFolder(Outlook.OlDefaultFolders.olFolderInbox);

            // return the personal folders
            return (Outlook.Folder)inbox.Parent;
        }


    }
}
