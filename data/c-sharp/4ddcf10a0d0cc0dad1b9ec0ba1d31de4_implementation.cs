using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using myOSity.API;

namespace solitaire
{
    public class Implementation : ImoApplication
    {
        private moSol _app;
        private MoRuntimeAPI _api;
        private Guid _win;


        #region ImoApplication Members

        public void ApplicationEnd()
        {
            _app.StopTimer();
            _app = null;
            _api = null;           
        }

        public string ApplicationName
        {
            get { return "Klondike Solitaire"; }
        }

        public UserControl ApplicationStart(MoRuntimeAPI runtimeAPI, Guid mainWindowHandle)
        {
            _win = mainWindowHandle;
            _api = runtimeAPI;
            _app = new moSol();
            _api.SetWindowMinimumWidth(_win, 720);
            _api.SetWindowMinimumHeight(_win, 650);
            _api.SetWindowWidth(_win, 720);
            _api.SetWindowHeight(_win, 650);
            _api.SetWindowAllowScrollbars(_win, false);
            _api.SetWindowBackground(_win, new SolidColorBrush(Colors.Brown));
            _api.SetAllowWindowResize(_win, false);
            _api.SetAllowWindowPerspective(_win, false);
            _api.WindowSizeChanged += APIWindowSizeChanged;

            return _app;
        }

        void APIWindowSizeChanged(object sender, MoSizeChangedEventArgs e)
        {
            _app.LayoutRoot.Width = e.NewWidth;
            _app.LayoutRoot.Height = e.NewHeight;
        }

        #endregion

        #region ImoApplication Members


        public ResourceDictionary Resources
        {
            get { return null;}
        }

        #endregion

        #region ImoApplication Members


        public void NewInstanceRequested(MoNewInstanceRequestEventArgs args)
        {
            if (args.HasFileData) args.FileData.Close();
        }

        #endregion
    }
}
