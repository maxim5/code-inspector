//Implementation inspired by an article by Josh Smith

using System.Collections.Generic;
using System.ComponentModel;

namespace Tum.CollabXT.DocxGen
{
    public class CheckableTreeItem : INotifyPropertyChanged
    {
        #region Data

        bool? _IsChecked = false;

        #endregion // Data

        public CheckableTreeItem(string name)
        {
            this.Name = name;
            this.Children = new List<CheckableTreeItem>();
            AutoSubCheck = true;
        }

        public void Initialize()
        {
            foreach (CheckableTreeItem child in this.Children)
            {
                child.Parent = this;
                child.Initialize();
            }
        }

        #region Properties

        public List<CheckableTreeItem> Children { get; private set; }

        public bool IsInitiallySelected { get; private set; }

        public string Name { get; private set; }

        public object Tag { get; set; }

        public CheckableTreeItem Parent { get; private set; }

        /// <summary>
        /// If true, children are checked, when the parent is checked.
        /// </summary>
        public bool AutoSubCheck { get; set; }

        #region IsChecked

        /// <summary>
        /// Gets/sets the state of the associated UI toggle (ex. CheckBox).
        /// The return value is calculated based on the check state of all
        /// child FooViewModels.  Setting this property to true or false
        /// will set all children to the same check state, and setting it 
        /// to any value will cause the parent to verify its check state.
        /// </summary>
        public bool? IsChecked
        {
            get { return _IsChecked; }
            set { this.SetIsChecked(value, true, true); }
        }

        void SetIsChecked(bool? value, bool updateChildren, bool updateParent)
        {
            if (value == _IsChecked)
                return;

            _IsChecked = value;

            if (AutoSubCheck || (_IsChecked.HasValue && !_IsChecked.Value))
            {
                if (updateChildren && _IsChecked.HasValue)
                    this.Children.ForEach(c => c.SetIsChecked(_IsChecked, true, false));
            }

            if (updateParent && Parent != null)
                Parent.VerifyCheckState();

            this.OnPropertyChanged("IsChecked");
        }

        void VerifyCheckState()
        {
            bool? state = null;
            for (int i = 0; i < this.Children.Count; ++i)
            {
                bool? current = this.Children[i].IsChecked;
                if (i == 0)
                {
                    state = current;
                }
                else if (state != current)
                {
                    state = null;
                    break;
                }
            }

            if (!AutoSubCheck && state != null && !state.Value)
                return; //Do not uncheck parent is this case

            this.SetIsChecked(state, false, true);
        }

        #endregion // IsChecked

        #endregion // Properties

        #region INotifyPropertyChanged Members

        void OnPropertyChanged(string prop)
        {
            if (this.PropertyChanged != null)
                this.PropertyChanged(this, new PropertyChangedEventArgs(prop));
        }

        public event PropertyChangedEventHandler PropertyChanged;

        #endregion
    }
}