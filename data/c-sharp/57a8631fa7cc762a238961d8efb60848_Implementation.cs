using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ServiceModel;
using SimpleNotify.PushLibrary;

namespace SimpleNotify.Powershell
{
    public class Implementation
    {
        internal string[] GetSubscriber(string server)
        {
            PushLibrary.PushClient c = new PushLibrary.PushClient(server);
            return c.GetSubscribers();
        }

        internal PushResult PushNotification(string client, string server, string title, string text, NotificationType type, string data)
        {
            PushLibrary.PushClient c = new PushLibrary.PushClient(server);
            return c.PushMessageToClient(client, title, text, type, data);
        }
    }
}
