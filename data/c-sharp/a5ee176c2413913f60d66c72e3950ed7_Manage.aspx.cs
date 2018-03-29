using System;
using System.Linq;
using System.Web.Security;
using System.Web.UI;
using QuestionsDAL;

namespace MoodleQuestions.Account
{
    public partial class Manage : Page
    {
        #region Properties

        /// <summary>
        /// Gets the success message.
        /// </summary>
        /// <value>
        /// The success message.
        /// </value>
        protected string SuccessMessage
        {
            get;
            private set;
        }

        #endregion

        #region Methods

        /// <summary>
        /// Converts to display date time.
        /// </summary>
        /// <param name="utcDateTime">The UTC date time.</param>
        /// <returns>Converted time.</returns>
        protected static string ConvertToDisplayDateTime(DateTime? utcDateTime)
        {
            // You can change this method to convert the UTC date time into the desired display
            // offset and format. Here we're converting it to the server timezone and formatting
            // as a short date and a long time string, using the current thread culture.
            return utcDateTime.HasValue ? utcDateTime.Value.ToLocalTime().ToString("G") : "[never]";
        }

        /// <summary>
        /// Handles the Load event of the Page control.
        /// </summary>
        protected void Page_Load()
        {
            if (!IsPostBack)
            {
                // Render success message
                var message = Request.QueryString["m"];
                if (message != null)
                {
                    // Strip the query string from action
                    Form.Action = ResolveUrl("~/Account/Manage.aspx");

                    SuccessMessage =
                        message == "ChangePwdSuccess" ? "Your password has been changed."
                        : string.Empty;
                    successMessage.Visible = !string.IsNullOrEmpty(SuccessMessage);
                }

                var loggedUserId = (Guid)Membership.GetUser().ProviderUserKey;

                using (var context = new Entities())
                {
                    var user = (from item in context.Users
                                where item.UserId == loggedUserId
                                select item).FirstOrDefault();

                    FirstName.Text = user.FirstName;
                    LastName.Text = user.LastName;
                }
            }
        }

        protected void setPassword_Click(object sender, EventArgs e)
        {
        }

        protected T Item<T>() where T : class
        {
            return GetDataItem() as T ?? default(T);
        }

        protected void NameChange_Click(object sender, EventArgs e)
        {
            var loggedUserId = (Guid)Membership.GetUser().ProviderUserKey;

            using (var context = new Entities())
            {
                var user = (from item in context.Users
                            where item.UserId == loggedUserId
                            select item).FirstOrDefault();

                user.FirstName = FirstName.Text;
                user.LastName = LastName.Text;

                context.SaveChanges();
            }
        }

        #endregion
    }
}